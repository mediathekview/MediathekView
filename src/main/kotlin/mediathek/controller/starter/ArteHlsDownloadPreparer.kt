package mediathek.controller.starter

import mediathek.daten.DatenDownload
import mediathek.daten.DownloadType
import mediathek.daten.FilmResolution
import mediathek.tool.ArteHlsQualitySelector
import mediathek.tool.HlsEgressPolicy
import mediathek.tool.ProcessCommandUtils
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.apache.logging.log4j.LogManager
import java.util.*
import java.util.concurrent.TimeUnit

/** Prepares a run-local command; neither the Pset nor persisted download invocations are modified. */
internal object ArteHlsDownloadPreparer {
    private val logger = LogManager.getLogger(ArteHlsDownloadPreparer::class.java)
    private val senderPattern = Regex("arte(?:[.](?:de|en|es|fr|it|pl))?", RegexOption.IGNORE_CASE)
    private val attributePattern = Regex("([A-Z0-9-]+)=(\"[^\"]*\"|[^,\"]+)(?:,|$)")
    private val programIdPattern = Regex("p:[0-9]+")
    private val subtitleProgramPattern = Regex("0:p:[0-9]+")
    private val languageTagPattern = Regex("[A-Za-z0-9]+(?:-[A-Za-z0-9]+)*")
    private val nonSubtitleStreamTypes = setOf("a", "v", "V", "d", "t")
    private val copyOptions = listOf("-c", "copy", "-bsf:a", "aac_adtstoasc")
    private val hlsEscapePattern = Regex("""\\(.)""")

    fun prepare(
        download: DatenDownload,
        detectQuotedSelectors: (String) -> Boolean? = ::detectQuotedSelectors,
        load: (HttpUrl) -> ArteHlsQualitySelector.Playlist = ArteHlsQualitySelector::loadPlaylist,
    ): List<String>? {
        if (download.art != DownloadType.PROGRAM || download.isDownloadManager ||
            !senderPattern.matches(download.sender) ||
            !ArteHlsQualitySelector.isArteManifest(download.downloadUrl)
        ) return null

        var qualityArguments: List<String>? = null
        return try {
            val args = download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY)
            if (args.size < 6 || !ProcessCommandUtils.isFfmpeg(args[0]) ||
                args.last().removeSurrounding("\"") != download.targetPathFileName
            ) return null
            val inputIndex = args.indices.singleOrNull { args[it] == "-i" } ?: return null
            val inputEndIndex = inputIndex + 2
            if (args.getOrNull(inputIndex + 1) != download.downloadUrl || inputEndIndex >= args.size) return null
            val switches = args.subList(inputEndIndex, args.lastIndex)
            val mapping = switches.take(4)
            val mapCount = args.count { it == "-map" }
            val includeSubtitles = download.automaticArteHlsProgramMap && mapCount == 1 &&
                mapping.firstOrNull() == "-map" && mapping.getOrNull(1)?.matches(subtitleProgramPattern) == true
            val programId = if (includeSubtitles) mapping[1].substringAfterLast(':').toIntOrNull()
            else if (mapping.size == 4 && mapping[0] == "-map" &&
                programIdPattern.matches(mapping[1]) && mapping[2] == "-map" && mapping[3] == "-0:s"
            ) mapping[1].substringAfter(':').toIntOrNull() else null
            // Provenance comes from invocation construction, never from a coincidentally equal Pset mapping.
            val automatic = download.automaticArteHlsProgramMap && programId != null &&
                (includeSubtitles || mapCount == 2) &&
                (programId == ArteHlsQualitySelector.fallbackProgramId(download.downloadUrl, download.selectedResolution) ||
                    (programId == 3 && download.selectedResolution == FilmResolution.Enum.LOW))
            // Quality selection preserves every additional option. Audio metadata rewriting retains its
            // narrower contract so custom metadata, filters and input/output options keep their semantics.
            val standardInput = inputIndex == 1 || (inputIndex == 3 && args.subList(1, 3) == listOf("-http_seekable", "0"))
            val mp4Output = download.targetPathFileName.endsWith(".mp4", ignoreCase = true)
            val prepareAudio = !includeSubtitles && mp4Output && standardInput &&
                (switches == copyOptions || (programId != null && switches.drop(4) == copyOptions))
            if (!automatic && !prepareAudio) return null
            val playlist = load(download.downloadUrl.toHttpUrl())
            val lines = playlist.text.lineSequence().map(String::trim).filter(String::isNotEmpty).toList()
            require(lines.firstOrNull() == "#EXTM3U") { "Not an HLS playlist" }
            val variants = lines.filter { it.startsWith("#EXT-X-STREAM-INF:") }
                .map { attributes(it.substringAfter(':')) }
            // HLS program IDs are the zero-based variant indexes used by FFmpeg.
            val selectedProgram = if (automatic) {
                ArteHlsQualitySelector.selectProgram(playlist.text, download.selectedResolution) ?: programId
            } else programId ?: if (variants.size == 1) 0 else return null
            if (selectedProgram != programId && programId != null) {
                val programMap = if (includeSubtitles) "0:p:$selectedProgram" else "p:$selectedProgram"
                qualityArguments = args.toMutableList().apply { this[inputEndIndex + 1] = programMap }
            }
            if (includeSubtitles && mp4Output) {
                val group = variants.getOrNull(selectedProgram)?.get("SUBTITLES") ?: return qualityArguments
                val metadata = mp4SubtitleLanguageOptions(lines, group, switches)
                if (metadata.isEmpty()) return qualityArguments
                return (qualityArguments ?: args).dropLast(1) + metadata + args.last()
            }
            if (!prepareAudio) return qualityArguments
            val group = variants.getOrNull(selectedProgram)?.get("AUDIO") ?: return qualityArguments
            val renditions = lines.filter { it.startsWith("#EXT-X-MEDIA:") }
                .map { attributes(it.substringAfter(':')) }
                .filter { it["TYPE"] == "AUDIO" }
            val audio = renditions.filter { it["GROUP-ID"] == group }
            if (audio.isEmpty() || audio.size > 32) return qualityArguments
            val names = audio.map { it["NAME"]?.takeIf(String::isNotBlank) ?: return qualityArguments }
            // FFmpeg stores HLS rendition names in a 64-byte, NUL-terminated field.
            if (names.distinct().size != names.size || names.any { it.toByteArray(Charsets.UTF_8).size >= 64 }) return qualityArguments
            val streamNames = names.map { name ->
                // FFmpeg's HLS parser consumes backslash escapes inside quoted attributes.
                // Keep the original NAME for output labels, but match the demuxer's value.
                if (name.takeLastWhile { it == '\\' }.length % 2 != 0) return qualityArguments
                hlsEscapePattern.replace(name) { it.groupValues[1] }
            }
            if (streamNames.distinct().size != streamNames.size) return qualityArguments
            fun audioUrl(rendition: Map<String, String>): String? {
                val uri = rendition["URI"]?.takeIf(String::isNotBlank) ?: return null
                return HlsEgressPolicy.requirePublicHttpUrl(requireNotNull(playlist.url.resolve(uri))).toString()
            }
            // A shared URI with conflicting rendition names can overwrite the demuxer's comment.
            // Only use metadata selectors when each selected rendition has an unambiguous identity.
            val allAudioUrls = renditions.map(::audioUrl)
            if (audio.any { rendition ->
                val uri = audioUrl(rendition) ?: return qualityArguments
                allAudioUrls.count { it == uri } != 1
            }) return qualityArguments
            val quotedSelectors = detectQuotedSelectors(args[0]) ?: return qualityArguments
            // The old -map parser splits at commas (sync syntax) and question marks
            // (optional maps) before matching metadata; quoting cannot protect them.
            if (!quotedSelectors && streamNames.any { ',' in it || '?' in it }) return qualityArguments
            // FFmpeg's HLS demuxer exposes EXT-X-MEDIA NAME as the stream's comment metadata.
            // Resolve by that identity inside the selected program instead of guessing stream
            // indexes. Keep one HLS input so its audio/video timestamp alignment is preserved.
            buildList {
                addAll(args.take(inputEndIndex))
                addAll(listOf("-map", "0:p:$selectedProgram:v:0"))
                streamNames.forEach {
                    val selector = if (quotedSelectors) quoteStreamName(it) else it
                    addAll(listOf("-map", "0:p:$selectedProgram:a:m:comment:$selector"))
                }
                addAll(copyOptions)
                addAll(listOf("-disposition:a", "0"))
                audio.forEachIndexed { index, rendition ->
                    rendition["NAME"]?.takeIf(String::isNotBlank)?.let { name ->
                        addAll(listOf("-metadata:s:a:$index", "title=$name"))
                        addAll(listOf("-metadata:s:a:$index", "handler_name=$name"))
                    }
                    rendition["LANGUAGE"]?.let(::mp4Language)?.let { language ->
                        addAll(listOf("-metadata:s:a:$index", "language=$language"))
                    }
                    if ("public.accessibility.describes-video" in rendition["CHARACTERISTICS"].orEmpty().split(',')) {
                        addAll(listOf("-disposition:a:$index", "visual_impaired"))
                    }
                }
                add(download.targetPathFileName)
            }
        } catch (ex: Exception) {
            logger.warn("ARTE-Spuren konnten nicht vorbereitet werden; verfügbare Qualitätsauswahl wird beibehalten: {}", ex.message)
            qualityArguments
        }
    }

    private fun mp4SubtitleLanguageOptions(lines: List<String>, group: String, switches: List<String>): List<String> {
        if (switches.withIndex().any { (index, option) ->
                option == "-map_metadata" || option.startsWith("-map_metadata:") ||
                    (canSetSubtitleMetadata(option) && switches.getOrNull(index + 1)?.startsWith("language=", ignoreCase = true) == true)
            }
        ) return emptyList()

        val languages = lines.asSequence()
            .filter { it.startsWith("#EXT-X-MEDIA:") }
            .map { attributes(it.substringAfter(':')) }
            .filter { it["TYPE"] == "SUBTITLES" && it["GROUP-ID"] == group }
            .mapNotNull { rendition ->
                val primary = rendition["LANGUAGE"]?.takeIf(languageTagPattern::matches) ?: return@mapNotNull null
                val associated = rendition["ASSOC-LANGUAGE"].orEmpty()
                if (associated.isNotEmpty() && !languageTagPattern.matches(associated)) return@mapNotNull null
                val source = if (associated.isEmpty()) primary else "$primary,$associated"
                val normalized = mp4Language(primary)?.takeUnless { it == source } ?: return@mapNotNull null
                source to normalized
            }
            .distinct()
        return buildList {
            // FFmpeg copies the rendition language before applying output metadata. Match the tag
            // so a missing rendition never shifts a language onto the wrong subtitle stream.
            languages.forEach { (source, normalized) ->
                addAll(listOf("-metadata:s:s:m:language:$source", "language=$normalized"))
            }
        }
    }

    private fun canSetSubtitleMetadata(option: String): Boolean =
        option == "-metadata:s" || (option.startsWith("-metadata:s:") &&
            option.removePrefix("-metadata:s:").substringBefore(':') !in nonSubtitleStreamTypes)

    /** Quote for FFmpeg's stream-specifier parser, not for a shell. */
    private fun quoteStreamName(name: String): String = "'" + name.replace("'", "'\\''") + "'"

    /** Probe the stream-specifier grammar, including on development builds with no release version. */
    internal fun detectQuotedSelectors(executable: String): Boolean? {
        // The new parser unquotes the metadata value and rejects the remaining suffix.
        // The old parser stores the option verbatim and reaches the empty-output check.
        // No input, decoding, encoding, network access or output file is needed.
        val process = ProcessBuilder(
            executable, "-v", "error", "-nostdin",
            "-c:a:m:mediathek_probe:'value':mediathek_invalid", "copy", "-f", "null", "-",
        ).redirectErrorStream(true).start()
        try {
            check(process.waitFor(3, TimeUnit.SECONDS)) { "FFmpeg parser probe timed out" }
            check(process.exitValue() != 0) { "Unexpected successful FFmpeg parser probe" }
            val output = process.inputStream.bufferedReader().use { it.readText() }
            return parseSelectorProbe(output)
        } finally {
            process.destroyForcibly()
            process.inputStream.close()
        }
    }

    internal fun parseSelectorProbe(output: String): Boolean? = when {
        "Trailing garbage at the end of a stream specifier: :mediathek_invalid" in output -> true
        Regex("""Output file(?: #\d+)? does not contain any stream""").containsMatchIn(output) -> false
        else -> null
    }

    private fun attributes(value: String): Map<String, String> = buildMap {
        var offset = 0
        while (offset < value.length) {
            val match = requireNotNull(attributePattern.matchAt(value, offset)) { "Invalid HLS attributes" }
            val key = match.groupValues[1]
            require(key !in this) { "Duplicate HLS attribute: $key" }
            put(key, match.groupValues[2].removeSurrounding("\""))
            offset = match.range.last + 1
        }
    }

    private fun mp4Language(language: String): String? = runCatching {
        when (language.lowercase(Locale.ROOT)) {
            "ger" -> "deu"
            "fre" -> "fra"
            else -> Locale.forLanguageTag(language).isO3Language.takeIf { it.length == 3 }
        }
    }.getOrNull()
}
