package mediathek.controller.starter

import mediathek.daten.*
import mediathek.tool.ArteHlsQualitySelector
import mediathek.tool.ArteQualityFixture
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import org.junit.jupiter.params.provider.ValueSource
import java.io.IOException

internal class ArteHlsDownloadPreparerTest {
    private val url = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/id/de/XQ/video.m3u8"
    private val baseArgs = listOf("/opt/bin/ffmpeg", "-i", url, "-map", "p:1", "-map", "-0:s", "-c", "copy", "-bsf:a", "aac_adtstoasc", "/tmp/a film.mp4")
    private val master = """
        #EXTM3U
        #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="other",NAME="Wrong track",LANGUAGE="fr",URI="wrong.m3u8"
        #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="selected",NAME="Deutsch, Stereo",LANGUAGE="de",DEFAULT=YES,URI="audio/de.m3u8"
        #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="selected",NAME="Audiodeskription <> ä",LANGUAGE="deu",DEFAULT=YES,CHARACTERISTICS="public.accessibility.describes-video,public.accessibility.transcribes-spoken-dialog",URI="../ad.m3u8"
        #EXT-X-STREAM-INF:BANDWIDTH=100,AUDIO="other"
        low.m3u8
        #EXT-X-STREAM-INF:BANDWIDTH=200,AUDIO="selected"
        high.m3u8
    """.trimIndent()

    private fun download(args: List<String> = baseArgs) = DatenDownload().apply {
        automaticArteHlsProgramMap = true
        sender = "ARTE.DE"
        art = DownloadType.PROGRAM
        downloadUrl = url
        targetPathFileName = args.last()
        programInvocationArray = args.joinToString(RuntimeExec.TRENNER_PROG_ARRAY)
        programInvocation = "unchanged display command"
    }

    private fun prepare(
        download: DatenDownload = download(),
        text: String = master,
        quotedSelectors: Boolean? = true,
    ): List<String>? =
        ArteHlsDownloadPreparer.prepare(download, { quotedSelectors }) { ArteHlsQualitySelector.Playlist(it, text) }

    @Test
    fun `maps only selected group and labels the explicit output audio order`() {
        val args = requireNotNull(prepare())
        assertEquals(listOf("0:p:1:v:0", "0:p:1:a:m:comment:'Deutsch, Stereo'", "0:p:1:a:m:comment:'Audiodeskription <> ä'"), optionValues(args, "-map"))
        assertEquals(listOf(url), optionValues(args, "-i"))
        assertEquals(listOf("title=Deutsch, Stereo", "handler_name=Deutsch, Stereo", "language=deu"), optionValues(args, "-metadata:s:a:0"))
        assertEquals(listOf("title=Audiodeskription <> ä", "handler_name=Audiodeskription <> ä", "language=deu"), optionValues(args, "-metadata:s:a:1"))
        assertEquals(listOf("0"), optionValues(args, "-disposition:a"))
        assertEquals(listOf("visual_impaired"), optionValues(args, "-disposition:a:1"))
        assertEquals("/tmp/a film.mp4", args.last())
        assertFalse(args.any { "wrong.m3u8" in it })
    }

    @ParameterizedTest
    @ValueSource(strings = ["ARTE", "ARTE.DE", "ARTE.EN", "ARTE.ES", "ARTE.FR", "ARTE.IT", "ARTE.PL", "arte.fr"])
    fun `accepts ARTE senders`(sender: String) {
        assertNotNull(prepare(download().apply { this.sender = sender }))
    }

    @ParameterizedTest
    @ValueSource(strings = ["ARD", "ZDF", "ARTEFACT", "ARTE.DE Extra", "ARTE.XX", ""])
    fun `other senders do not even fetch the playlist`(sender: String) {
        assertNull(ArteHlsDownloadPreparer.prepare(download().apply { this.sender = sender }) { fail("Unexpected request") })
    }

    @Test
    fun `saved Pset and both persisted commands remain unchanged across retries`() {
        val program = DatenProg("ffmpeg", "/opt/bin/ffmpeg", "-i %f -c copy -bsf:a aac_adtstoasc **", "false", "false")
        val pset = DatenPset().apply { listeProg.add(program) }
        val download = download().apply { pSet = pset }
        val originalSwitches = program.switches
        val originalCommand = download.programInvocation
        val originalArray = download.programInvocationArray
        val first = prepare(download)
        assertNotNull(first)
        assertEquals(first, prepare(download))
        assertSame(pset, download.pSet)
        assertEquals(originalSwitches, program.switches)
        assertEquals(originalCommand, download.programInvocation)
        assertEquals(originalArray, download.programInvocationArray)
    }

    @Test
    fun `styroll command preserves http seekable and saved Pset while correcting all six audio tracks`() {
        val streamUrl = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/ce27f048-f06a-4795-a243-93af69fabb71/de/XQ+KS+CHEV1/112793-000-A.m3u8"
        val target = "/tmp/Kino_-_Filme-Zwei_zu_Eins.mp4"
        val args = listOf(
            "bin/ffmpeg", "-http_seekable", "0", "-i", streamUrl,
            "-map", "p:1", "-map", "-0:s", "-c", "copy", "-bsf:a", "aac_adtstoasc", target,
        )
        val program = DatenProg("ffmpeg", "bin/ffmpeg", "-http_seekable 0 -i %f -c copy -bsf:a aac_adtstoasc **", "false", "false")
        val pset = DatenPset("Mac Speichern mod").apply { listeProg.add(program) }
        val download = download(args).apply {
            downloadUrl = streamUrl
            programInvocation = args.joinToString(" ")
            pSet = pset
        }
        val originalSwitches = program.switches
        val originalCommand = download.programInvocation
        val originalArray = download.programInvocationArray
        val tracks = listOf(
            "Deutsch" to "de",
            "Französisch" to "fr",
            "Deutsch (Klare Sprache)" to "de",
            "Deutsch (Audiodeskription)" to "de",
            "Französisch (Klare Sprache)" to "fr",
            "Französisch (Audiodeskription)" to "fr",
        )
        val playlist = buildString {
            appendLine("#EXTM3U")
            tracks.forEachIndexed { index, (name, language) ->
                val characteristics = if (index in listOf(3, 5)) ",CHARACTERISTICS=\"public.accessibility.describes-video\"" else ""
                appendLine("#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"audio_0\",NAME=\"$name\",LANGUAGE=\"$language\",URI=\"audio$index.m3u8\"$characteristics")
            }
            appendLine("#EXT-X-STREAM-INF:BANDWIDTH=100,AUDIO=\"audio_0\"")
            appendLine("low.m3u8")
            appendLine("#EXT-X-STREAM-INF:BANDWIDTH=200,AUDIO=\"audio_0\"")
            appendLine("high.m3u8")
        }

        val prepared = requireNotNull(prepare(download, playlist))

        assertEquals(args.take(5), prepared.take(5))
        assertEquals(listOf("0"), optionValues(prepared, "-http_seekable"))
        assertEquals(listOf(streamUrl), optionValues(prepared, "-i"))
        assertEquals(listOf("0:p:1:v:0") + tracks.map { (name, _) -> "0:p:1:a:m:comment:'$name'" }, optionValues(prepared, "-map"))
        tracks.forEachIndexed { index, (name, language) ->
            val isoLanguage = if (language == "de") "deu" else "fra"
            assertEquals(listOf("title=$name", "handler_name=$name", "language=$isoLanguage"), optionValues(prepared, "-metadata:s:a:$index"))
        }
        assertEquals(listOf("0"), optionValues(prepared, "-disposition:a"))
        assertEquals(listOf("visual_impaired"), optionValues(prepared, "-disposition:a:3"))
        assertEquals(listOf("visual_impaired"), optionValues(prepared, "-disposition:a:5"))
        assertEquals(target, prepared.last())
        assertEquals(prepared, prepare(download, playlist))
        assertSame(pset, download.pSet)
        assertSame(program, pset.listeProg.single())
        assertEquals("bin/ffmpeg", program.programPath)
        assertEquals(originalSwitches, program.switches)
        assertEquals(originalCommand, download.programInvocation)
        assertEquals(originalArray, download.programInvocationArray)
    }

    @Test
    fun `unsupported or incomplete input options are not rewritten`() {
        listOf(
            listOf("-http_seekable"),
            listOf("-http_seekable", "invalid"),
            listOf("-headers", "Custom: value"),
            listOf("-http_seekable", "0", "-headers", "Custom: value"),
        ).forEach { prefix ->
            val args = baseArgs.take(1) + prefix + baseArgs.drop(1)
            val download = download(args).apply { automaticArteHlsProgramMap = false }
            assertNull(ArteHlsDownloadPreparer.prepare(download) { fail("Unexpected request for $args") })
            assertEquals(args.joinToString(RuntimeExec.TRENNER_PROG_ARRAY), download.programInvocationArray)
        }
    }

    @Test
    fun `special character preparation preserves raw labels Pset and persisted commands`() {
        val name = " Français: version d'origine\\original <> ? "
        val program = DatenProg("ffmpeg", "/opt/bin/ffmpeg", "-i %f -c copy -bsf:a aac_adtstoasc **", "false", "false")
        val download = download().apply { pSet = DatenPset().apply { listeProg.add(program) } }
        val beforeSwitches = program.switches
        val beforeCommand = download.programInvocation
        val beforeArray = download.programInvocationArray
        val text = master.replace("Deutsch, Stereo", name)
        val prepared = requireNotNull(prepare(download, text))
        assertEquals("0:p:1:a:m:comment:' Français: version d'\\''origineoriginal <> ? '", optionValues(prepared, "-map")[1])
        assertTrue("handler_name=$name" in optionValues(prepared, "-metadata:s:a:0"))
        assertEquals(beforeSwitches, program.switches)
        assertEquals(beforeCommand, download.programInvocation)
        assertEquals(beforeArray, download.programInvocationArray)
        assertEquals(prepared, prepare(download, text))
    }

    @Test
    fun `older FFmpeg matches literal names including punctuation and whitespace`() {
        val name = " Français: version d'origine\\original <> "
        val prepared = requireNotNull(prepare(
            text = master.replace("Deutsch, Stereo", name),
            quotedSelectors = false,
        ))
        assertEquals(listOf("0:p:1:v:0", "0:p:1:a:m:comment: Français: version d'origineoriginal <> ",
            "0:p:1:a:m:comment:Audiodeskription <> ä"), optionValues(prepared, "-map"))
        assertTrue("handler_name=$name" in optionValues(prepared, "-metadata:s:a:0"))
    }

    @ParameterizedTest
    @ValueSource(strings = ["Deutsch, Stereo", "Español?", "Question? in the middle"])
    fun `legacy map control characters preserve the original invocation`(name: String) {
        val download = download()
        assertNull(prepare(download, master.replace("Deutsch, Stereo", name), quotedSelectors = false))
        assertEquals(baseArgs, download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY))
    }

    @Test
    fun `new parser uses quoted metadata selectors`() {
        val prepared = requireNotNull(prepare(quotedSelectors = true))
        assertEquals("0:p:1:a:m:comment:'Deutsch, Stereo'", optionValues(prepared, "-map")[1])
    }

    @Test
    fun `unrecognized parser preserves the original invocation`() {
        val download = download()
        assertNull(prepare(download, quotedSelectors = null))
        assertEquals(baseArgs, download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY))
    }

    @ParameterizedTest
    @ValueSource(strings = [
        "Output file #0 does not contain any stream",
        "[out#0/null @ 0x1234] Output file does not contain any stream",
    ])
    fun `recognizes old parser diagnostics across FFmpeg generations`(output: String) {
        assertEquals(false, ArteHlsDownloadPreparer.parseSelectorProbe(output))
    }

    @Test
    fun `recognizes the new parser diagnostic`() {
        assertEquals(true, ArteHlsDownloadPreparer.parseSelectorProbe(
            "Trailing garbage at the end of a stream specifier: :mediathek_invalid\nError opening output files: Invalid argument",
        ))
        assertNull(ArteHlsDownloadPreparer.parseSelectorProbe("Unrecognized option"))
    }

    @Test
    fun `failed parser probe preserves the original invocation`() {
        val download = download()
        assertNull(ArteHlsDownloadPreparer.prepare(download, { throw IOException("unavailable") }) {
            ArteHlsQualitySelector.Playlist(it, master)
        })
        assertEquals(baseArgs, download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY))
    }

    @Test
    fun `names that collide after FFmpeg HLS unescaping keep the original invocation`() {
        val text = master.replace("Deutsch, Stereo", "Audio\\Name").replace("Audiodeskription <> ä", "AudioName")
        assertNull(prepare(text = text))
        assertNull(prepare(text = master.replace("Deutsch, Stereo", "Trailing\\")))
    }

    @Test
    fun `skips custom options mappings executables and non MP4 targets before fetching`() {
        val commands = listOf(
            baseArgs.toMutableList().apply { this[4] = "0:a" },
            baseArgs.dropLast(1) + listOf("-metadata:s:a:0", "title=Custom", baseArgs.last()),
            baseArgs.dropLast(1) + listOf("-disposition:a", "default", baseArgs.last()),
            baseArgs.toMutableList().apply { this[0] = "vlc" },
            baseArgs.dropLast(1) + "/tmp/film.mkv",
            baseArgs.dropLast(1) + listOf("second.mp4", baseArgs.last()),
        )
        commands.forEach { args ->
            assertNull(ArteHlsDownloadPreparer.prepare(download(args).apply { automaticArteHlsProgramMap = false }) { fail("Unexpected request for $args") })
        }
        assertNull(ArteHlsDownloadPreparer.prepare(download().apply { art = DownloadType.DIRECT }) { fail("Unexpected request") })
        assertNull(ArteHlsDownloadPreparer.prepare(download().apply { downloadUrl = "https://example.org/a.m3u8" }) { fail("Unexpected request") })
    }

    @Test
    fun `lookup failure leaves the original invocation usable`() {
        val download = download()
        assertNull(ArteHlsDownloadPreparer.prepare(download) { throw IOException("offline") })
        assertEquals(baseArgs, download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY))
    }

    @Test
    fun `supports the Windows standard Pset target quoting without changing the stored command`() {
        val args = baseArgs.toMutableList().apply {
            this[0] = "/tools/ffmpeg.exe"
            this[lastIndex] = "\"${baseArgs.last()}\""
        }
        val download = download(args).apply { targetPathFileName = baseArgs.last() }
        val prepared = requireNotNull(prepare(download))
        assertEquals(baseArgs.last(), prepared.last())
        assertEquals(args.joinToString(RuntimeExec.TRENNER_PROG_ARRAY), download.programInvocationArray)
    }

    @Test
    fun `rejects incomplete or ambiguous playlist metadata`() {
        listOf(
            "not an HLS playlist",
            master.replace("GROUP-ID=\"selected\"", "GROUP-ID=\"missing\""),
            master.replace(",URI=\"../ad.m3u8\"", ""),
            master.replace("../ad.m3u8", "http://127.0.0.1/ad.m3u8"),
            master.replace("../ad.m3u8", "file:///tmp/ad.m3u8"),
            master.replace("LANGUAGE=\"de\"", "LANGUAGE=\"de\",LANGUAGE=\"fr\""),
            master.replace("NAME=\"Deutsch, Stereo\"", "NAME=\"unterminated"),
            master.replace("NAME=\"Deutsch, Stereo\",", ""),
            master.replace("Audiodeskription <> ä", "Deutsch, Stereo"),
            master.replace("../ad.m3u8", "audio/de.m3u8"),
            master.replace("Deutsch, Stereo", "x".repeat(64)),
            master.replace("Deutsch, Stereo", "ä".repeat(32)),
        ).forEach { assertNull(prepare(text = it), it) }
        assertNull(prepare(download(baseArgs.toMutableList().apply { this[4] = "p:99" })))
    }

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun `single variant works without a program map but multiple variants need an explicit selection`(disableHttpSeeking: Boolean) {
        val inputOptions = if (disableHttpSeeking) listOf("-http_seekable", "0") else emptyList()
        val args = baseArgs.take(1) + inputOptions + baseArgs.subList(1, 3) + baseArgs.drop(7)
        assertNull(prepare(download(args)))
        val singleVariant = master.substringBefore("#EXT-X-STREAM-INF") + "#EXT-X-STREAM-INF:BANDWIDTH=200,AUDIO=\"selected\"\nhigh.m3u8"
        val prepared = requireNotNull(prepare(download(args), singleVariant))
        assertEquals(args.take(3 + inputOptions.size), prepared.take(3 + inputOptions.size))
        assertEquals("0:p:0:v:0", optionValues(prepared, "-map").first())
    }

    @Test
    fun `recognizes ambiguous shared rendition URLs after a redirect`() {
        val args = requireNotNull(ArteHlsDownloadPreparer.prepare(download(), { true }) {
            ArteHlsQualitySelector.Playlist("https://cdn.example/path/master.m3u8".toHttpUrl(), master)
        })
        assertEquals(listOf(url), optionValues(args, "-i"))
        assertNull(ArteHlsDownloadPreparer.prepare(download(), { true }) {
            ArteHlsQualitySelector.Playlist("https://cdn.example/path/master.m3u8".toHttpUrl(), master.replace("../ad.m3u8", "https://cdn.example/path/audio/de.m3u8"))
        })
    }

    @ParameterizedTest
    @ValueSource(strings = ["fr:fra", "fre:fra", "en:eng", "ger:deu", "pl:pol", "de-DE:deu"])
    fun `normalizes MP4 language codes`(example: String) {
        val (input, expected) = example.split(':')
        val args = requireNotNull(prepare(text = master.replace("LANGUAGE=\"de\"", "LANGUAGE=\"$input\"")))
        assertTrue("language=$expected" in optionValues(args, "-metadata:s:a:0"))
    }

    @Test
    fun `unknown language does not invent language metadata`() {
        val args = requireNotNull(prepare(text = master.replace("LANGUAGE=\"de\"", "LANGUAGE=\"invalid\"")))
        assertFalse(optionValues(args, "-metadata:s:a:0").any { it.startsWith("language=") })
        assertEquals(3, optionValues(args, "-map").size)
    }

    @Test
    fun `shared selection preserves selected audio Psets and commands for every quality and playlist order`() {
        val qualities = listOf(FilmResolution.Enum.HIGH_QUALITY, FilmResolution.Enum.NORMAL, FilmResolution.Enum.LOW)
        val fixtures = listOf(
            ArteQualityFixture.variants to listOf(4, 2, 0),
            ArteQualityFixture.variants.take(3) to listOf(1, 2, 0),
            ArteQualityFixture.variants.reversed() to listOf(1, 3, 5),
        )
        for ((variants, indexes) in fixtures) {
            qualities.zip(indexes).forEach { (quality, index) ->
                val fallback = requireNotNull(ArteHlsQualitySelector.fallbackProgramId(url, quality))
                val args = baseArgs.toMutableList().apply { this[4] = "p:$fallback" }
                val program = DatenProg("ffmpeg", "/opt/bin/ffmpeg", "-i %f -c copy -bsf:a aac_adtstoasc **", "false", "false")
                val download = download(args).apply {
                    selectedResolution = quality
                    pSet = DatenPset().apply { listeProg.add(program) }
                }
                val switches = program.switches
                val stored = download.programInvocationArray
                val display = download.programInvocation
                val prepared = requireNotNull(prepare(download, ArteQualityFixture.master(variants)))
                assertEquals(listOf("0:p:$index:v:0", "0:p:$index:a:m:comment:'Track $index'"), optionValues(prepared, "-map"))
                assertTrue("title=Track $index" in optionValues(prepared, "-metadata:s:a:0"))
                assertEquals(listOf("visual_impaired"), optionValues(prepared, "-disposition:a:0"))
                assertEquals(switches, program.switches)
                assertEquals(stored, download.programInvocationArray)
                assertEquals(display, download.programInvocation)
                assertEquals(prepared, prepare(download, ArteQualityFixture.master(variants)))
            }
        }
    }

    @Test
    fun `legacy LOW program three is corrected and HEVC selection survives unavailable audio labels`() {
        val low = download(baseArgs.toMutableList().apply { this[4] = "p:3" }).apply { selectedResolution = FilmResolution.Enum.LOW }
        assertEquals("0:p:0:v:0", optionValues(requireNotNull(prepare(low, ArteQualityFixture.master())), "-map").first())
        val high = download().apply { selectedResolution = FilmResolution.Enum.HIGH_QUALITY }
        val prepared = requireNotNull(prepare(high, ArteQualityFixture.master().replace("NAME=", "UNKNOWN=")))
        assertEquals(listOf("p:4", "-0:s"), optionValues(prepared, "-map"))
    }

    private fun builtDownload(switches: String, quality: FilmResolution.Enum): Pair<DatenDownload, DatenProg> {
        val program = DatenProg("ffmpeg", "/opt/bin/ffmpeg", switches, "false", "false")
        val invocation = DownloadProgramInvocationBuilder.build(
            DownloadType.PROGRAM, program,
            DownloadInvocationRequest(url, "", "/tmp", "a film.mp4", "/tmp/a film.mp4", "", "", quality),
        )
        return download(invocation.commandArray.split(RuntimeExec.TRENNER_PROG_ARRAY)).apply {
            selectedResolution = quality
            automaticArteHlsProgramMap = invocation.automaticArteHlsProgramMap
            programInvocation = invocation.command
            pSet = DatenPset().apply { listeProg.add(program) }
        } to program
    }

    @ParameterizedTest
    @ValueSource(strings = ["srt", "mov_text"])
    fun `opt-in retains only the selected HLS program and survives restoration`(codec: String) {
        val switches = "-http_seekable 0 -i %f %arteHlsWithSubtitles -c:v copy -c:a copy -c:s $codec **"
        val variants = ArteQualityFixture.variants.reversed()
        val selections = listOf(FilmResolution.Enum.HIGH_QUALITY to 1, FilmResolution.Enum.NORMAL to 3, FilmResolution.Enum.LOW to 5)
        for ((quality, programId) in selections) {
            val (original, program) = builtDownload(switches, quality)
            for (download in listOf(original, original.copy, DatenDownload.fromConfig(original.toConfig()))) {
                assertTrue(download.automaticArteHlsProgramMap)
                val stored = download.programInvocationArray
                val display = download.programInvocation
                assertFalse(display.contains("%arteHlsWithSubtitles"))
                val prepared = prepare(download, ArteQualityFixture.master(variants)) ?: stored.split(RuntimeExec.TRENNER_PROG_ARRAY)
                assertEquals(
                    listOf("/opt/bin/ffmpeg", "-http_seekable", "0", "-i", url,
                        "-map", "0:p:$programId", "-c:v", "copy", "-c:a", "copy", "-c:s", codec, "/tmp/a film.mp4"),
                    prepared,
                )
                assertEquals(stored, download.programInvocationArray)
                assertEquals(display, download.programInvocation)
                assertEquals(switches, program.switches)
            }
        }
    }

    @Test
    fun `explicit maps keep control with and without the opt-in marker`() {
        val quality = FilmResolution.Enum.HIGH_QUALITY
        for (marker in listOf("", "%arteHlsWithSubtitles ")) {
            val (download, _) = builtDownload("-i %f ${marker}-map 0:p:1 -c:s mov_text **", quality)
            assertFalse(download.automaticArteHlsProgramMap)
            assertEquals(listOf("0:p:1"), optionValues(download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY), "-map"))
            assertNull(ArteHlsDownloadPreparer.prepare(download) { fail("Unexpected playlist request") })
        }
    }

    @ParameterizedTest
    @CsvSource(
        "'', true",
        "'-metadata:s:a:0 language=fra', true",
        "'-metadata:s:s:0 language=eng', false",
        "'-metadata:s:0 language=eng', false",
        "'-map_metadata:s:s -1', false",
        "'-map_metadata -1', false",
    )
    fun `MP4 subtitle languages follow the selected group and respect custom metadata`(customMetadata: String, setSubtitleLanguages: Boolean) {
        val options = if (customMetadata.isEmpty()) "" else "$customMetadata "
        val (download, program) = builtDownload(
            "-i %f %arteHlsWithSubtitles -c:v copy -c:a copy -c:s mov_text ${options}**",
            FilmResolution.Enum.HIGH_QUALITY,
        )
        val stored = download.programInvocationArray
        val switches = program.switches
        val playlist = """
            #EXTM3U
            #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="selected",NAME="Français",LANGUAGE="fr",URI="fr.m3u8"
            #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="selected",NAME="Français SDH",LANGUAGE="fr",URI="fr-sdh.m3u8"
            #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="selected",NAME="Deutsch",LANGUAGE="de",URI="de.m3u8"
            #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="other",NAME="Italiano",LANGUAGE="it",URI="it.m3u8"
            #EXT-X-STREAM-INF:BANDWIDTH=200,RESOLUTION=1920x1080,SUBTITLES="selected"
            high.m3u8
            #EXT-X-STREAM-INF:BANDWIDTH=100,RESOLUTION=768x432,SUBTITLES="other"
            low.m3u8
        """.trimIndent()
        val prepared = requireNotNull(prepare(download, playlist))
        val expected = stored.split(RuntimeExec.TRENNER_PROG_ARRAY).toMutableList().apply {
            this[indexOf("-map") + 1] = "0:p:0"
            if (setSubtitleLanguages) {
                addAll(lastIndex, listOf("-metadata:s:s:m:language:fr", "language=fra", "-metadata:s:s:m:language:de", "language=deu"))
            }
        }
        assertEquals(expected, prepared)
        assertEquals(stored, download.programInvocationArray)
        assertEquals(switches, program.switches)
    }

    @Test
    fun `explicit Pset mappings remain explicit even when their numbers match automatic or legacy LOW mappings`() {
        for ((quality, programId) in listOf(FilmResolution.Enum.HIGH_QUALITY to 1, FilmResolution.Enum.LOW to 0, FilmResolution.Enum.LOW to 3)) {
            val switches = "-i %f -map p:$programId -map -0:s -c copy -bsf:a aac_adtstoasc **"
            val (download, program) = builtDownload(switches, quality)
            assertFalse(download.automaticArteHlsProgramMap)
            val stored = download.programInvocationArray
            val display = download.programInvocation
            val prepared = requireNotNull(prepare(download, ArteQualityFixture.master()))
            assertEquals(listOf("0:p:$programId:v:0", "0:p:$programId:a:m:comment:'Track $programId'"), optionValues(prepared, "-map"))
            assertTrue("title=Track $programId" in optionValues(prepared, "-metadata:s:a:0"))
            assertEquals(listOf("visual_impaired"), optionValues(prepared, "-disposition:a:0"))
            assertEquals(switches, program.switches)
            assertEquals(stored, download.programInvocationArray)
            assertEquals(display, download.programInvocation)
        }
    }

    @ParameterizedTest
    @ValueSource(strings = ["-movflags +faststart", "-metadata title=Custom", "-disposition:a default", "-threads 2"])
    fun `automatic quality selection preserves additional output options across codecs qualities and restoration`(options: String) {
        val qualities = listOf(FilmResolution.Enum.HIGH_QUALITY, FilmResolution.Enum.NORMAL, FilmResolution.Enum.LOW)
        for ((variants, indexes) in listOf(
            ArteQualityFixture.variants to listOf(4, 2, 0),
            ArteQualityFixture.variants.take(3) to listOf(1, 2, 0),
            ArteQualityFixture.variants.reversed() to listOf(1, 3, 5),
        )) {
            qualities.zip(indexes).forEach { (quality, index) ->
                val switches = "-i %f -c copy -bsf:a aac_adtstoasc $options **"
                val (original, program) = builtDownload(switches, quality)
                assertTrue(original.automaticArteHlsProgramMap)
                for (download in listOf(original, original.copy, DatenDownload.fromConfig(original.toConfig()))) {
                    val stored = download.programInvocationArray
                    val display = download.programInvocation
                    val args = stored.split(RuntimeExec.TRENNER_PROG_ARRAY)
                    val expected = args.toMutableList().apply { this[indexOf("-map") + 1] = "p:$index" }
                    val prepared = prepare(download, ArteQualityFixture.master(variants)) ?: args
                    assertEquals(expected, prepared)
                    assertEquals(switches, program.switches)
                    assertEquals(stored, download.programInvocationArray)
                    assertEquals(display, download.programInvocation)
                }
            }
        }
    }

    @Test
    fun `automatic quality selection preserves additional input options and explicit custom options never fetch`() {
        val (download, program) = builtDownload("-user_agent MediathekView -i %f -c copy -bsf:a aac_adtstoasc **", FilmResolution.Enum.HIGH_QUALITY)
        val original = download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY)
        val prepared = requireNotNull(prepare(download, ArteQualityFixture.master()))
        assertEquals(original.toMutableList().apply { this[indexOf("-map") + 1] = "p:4" }, prepared)
        assertEquals("-user_agent MediathekView -i %f -c copy -bsf:a aac_adtstoasc **", program.switches)
        val (explicit, _) = builtDownload("-i %f -map p:1 -map -0:s -c copy -bsf:a aac_adtstoasc -movflags +faststart **", FilmResolution.Enum.HIGH_QUALITY)
        assertFalse(explicit.automaticArteHlsProgramMap)
        assertNull(ArteHlsDownloadPreparer.prepare(explicit) { fail("Explicit custom mapping must not fetch") })
    }

    private fun optionValues(args: List<String>, option: String): List<String> =
        args.indices.filter { args[it] == option }.map { args[it + 1] }
}
