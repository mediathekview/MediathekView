package mediathek.controller.starter

import mediathek.daten.DatenDownload
import mediathek.daten.DownloadType
import mediathek.daten.FilmResolution
import mediathek.tool.ArteHlsQualitySelector
import mediathek.tool.HlsEgressPolicy
import mediathek.tool.http.MVHttpClient
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import java.nio.file.Path
import java.util.*
import kotlin.time.Duration.Companion.seconds
import kotlin.time.toJavaDuration

/** Prepares a run-local command; neither the Pset nor persisted download invocations are modified. */
internal object ArteHlsDownloadPreparer {
    internal data class Playlist(val url: HttpUrl, val text: String)
    private data class Variant(val audioGroup: String?)
    private val logger = LogManager.getLogger(ArteHlsDownloadPreparer::class.java)
    private val senderPattern = Regex("arte(?:[.](?:de|en|es|fr|it|pl))?", RegexOption.IGNORE_CASE)
    private val attributePattern = Regex("([A-Z0-9-]+)=(\"[^\"]*\"|[^,\"]+)(?:,|$)")
    private val hlsEscapePattern = Regex("""\\(.)""")
    private const val MAX_PLAYLIST_BYTES = 1_048_576L

    fun prepare(download: DatenDownload, load: (HttpUrl) -> Playlist = ::loadPlaylist): List<String>? {
        if (download.art != DownloadType.PROGRAM || download.isDownloadManager ||
            !senderPattern.matches(download.sender) ||
            ArteHlsQualitySelector.programId(download.downloadUrl, FilmResolution.Enum.HIGH_QUALITY) == null
        ) return null

        return try {
            val args = download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY)
            // Limit automatic rewriting to the standard, single-output stream-copy invocation.
            // Custom stream mappings, metadata, filters and input options keep their original semantics.
            if (args.size < 8 || Path.of(args[0]).fileName.toString().lowercase(Locale.ROOT) !in setOf("ffmpeg", "ffmpeg.exe") ||
                args.last().removeSurrounding("\"") != download.targetPathFileName ||
                !download.targetPathFileName.endsWith(".mp4", ignoreCase = true)
            ) return null
            // Preserve the supported HTTP input option in its original position before -i.
            val inputIndex = if (args.subList(1, 3) == listOf("-http_seekable", "0")) 3 else 1
            val inputEndIndex = inputIndex + 2
            if (args.subList(inputIndex, inputEndIndex) != listOf("-i", download.downloadUrl)) return null
            val switches = args.subList(inputEndIndex, args.lastIndex)
            val copyOptions = listOf("-c", "copy", "-bsf:a", "aac_adtstoasc")
            val programId = when {
                switches == copyOptions -> null
                switches.size == 8 && switches.take(4).let {
                    it[0] == "-map" && it[1].matches(Regex("p:[0-9]+")) && it[2] == "-map" && it[3] == "-0:s"
                } && switches.drop(4) == copyOptions -> switches[1].substringAfter(':').toInt()
                else -> return null
            }
            val playlist = load(download.downloadUrl.toHttpUrl())
            val lines = playlist.text.lineSequence().map(String::trim).filter(String::isNotEmpty).toList()
            require(lines.firstOrNull() == "#EXTM3U") { "Not an HLS playlist" }
            val variants = lines.filter { it.startsWith("#EXT-X-STREAM-INF:") }
                .map { Variant(attributes(it.substringAfter(':'))["AUDIO"]) }
            // HLS program IDs are the zero-based variant indexes used by FFmpeg.
            val selectedProgram = programId ?: if (variants.size == 1) 0 else return null
            val group = variants.getOrNull(selectedProgram)?.audioGroup ?: return null
            val renditions = lines.filter { it.startsWith("#EXT-X-MEDIA:") }
                .map { attributes(it.substringAfter(':')) }
                .filter { it["TYPE"] == "AUDIO" }
            val audio = renditions.filter { it["GROUP-ID"] == group }
            if (audio.isEmpty() || audio.size > 32) return null
            val names = audio.map { it["NAME"]?.takeIf(String::isNotBlank) ?: return null }
            // FFmpeg stores HLS rendition names in a 64-byte, NUL-terminated field.
            if (names.distinct().size != names.size || names.any { it.toByteArray(Charsets.UTF_8).size >= 64 }) return null
            val streamNames = names.map { name ->
                // FFmpeg's HLS parser consumes backslash escapes inside quoted attributes.
                // Keep the original NAME for output labels, but match the demuxer's value.
                if (name.takeLastWhile { it == '\\' }.length % 2 != 0) return null
                hlsEscapePattern.replace(name) { it.groupValues[1] }
            }
            if (streamNames.distinct().size != streamNames.size) return null
            fun audioUrl(rendition: Map<String, String>): String? {
                val uri = rendition["URI"]?.takeIf(String::isNotBlank) ?: return null
                return HlsEgressPolicy.requirePublicHttpUrl(requireNotNull(playlist.url.resolve(uri))).toString()
            }
            // A shared URI with conflicting rendition names can overwrite the demuxer's comment.
            // Only use metadata selectors when each selected rendition has an unambiguous identity.
            val allAudioUrls = renditions.map(::audioUrl)
            if (audio.any { rendition ->
                val uri = audioUrl(rendition) ?: return null
                allAudioUrls.count { it == uri } != 1
            }) return null
            // FFmpeg's HLS demuxer exposes EXT-X-MEDIA NAME as the stream's comment metadata.
            // Resolve by that identity inside the selected program instead of guessing stream
            // indexes. Keep one HLS input so its audio/video timestamp alignment is preserved.
            buildList {
                addAll(args.take(inputEndIndex))
                addAll(listOf("-map", "0:p:$selectedProgram:v:0"))
                streamNames.forEach { addAll(listOf("-map", "0:p:$selectedProgram:a:m:comment:${quoteStreamName(it)}")) }
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
            logger.warn("ARTE-Audiospuren konnten nicht vorbereitet werden; ursprünglicher Programmaufruf wird verwendet: {}", ex.message)
            null
        }
    }

    /** Quote for FFmpeg's stream-specifier parser, not for a shell. */
    private fun quoteStreamName(name: String): String = "'" + name.replace("'", "'\\''") + "'"

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

    private fun loadPlaylist(url: HttpUrl): Playlist {
        val client = HlsEgressPolicy.clientFor(MVHttpClient.httpClient).newBuilder()
            .callTimeout(10.seconds.toJavaDuration())
            .build()
        val request = Request.Builder().url(HlsEgressPolicy.requirePublicHttpUrl(url)).build()
        return client.newCall(request).execute().use { response ->
            check(response.isSuccessful) { "HLS HTTP ${response.code}" }
            val body = response.body
            val bytes = body.byteStream().readNBytes((MAX_PLAYLIST_BYTES + 1).toInt())
            require(bytes.size <= MAX_PLAYLIST_BYTES) { "HLS playlist too large" }
            Playlist(response.request.url, bytes.toString(Charsets.UTF_8))
        }
    }
}
