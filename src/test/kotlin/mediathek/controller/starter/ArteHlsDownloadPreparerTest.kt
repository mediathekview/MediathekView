package mediathek.controller.starter

import mediathek.daten.DatenDownload
import mediathek.daten.DatenProg
import mediathek.daten.DatenPset
import mediathek.daten.DownloadType
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
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
        ArteHlsDownloadPreparer.prepare(download, { quotedSelectors }) { ArteHlsDownloadPreparer.Playlist(it, text) }

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
            val download = download(args)
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
            ArteHlsDownloadPreparer.Playlist(it, master)
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
            assertNull(ArteHlsDownloadPreparer.prepare(download(args)) { fail("Unexpected request for $args") })
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
            ArteHlsDownloadPreparer.Playlist("https://cdn.example/path/master.m3u8".toHttpUrl(), master)
        })
        assertEquals(listOf(url), optionValues(args, "-i"))
        assertNull(ArteHlsDownloadPreparer.prepare(download(), { true }) {
            ArteHlsDownloadPreparer.Playlist("https://cdn.example/path/master.m3u8".toHttpUrl(), master.replace("../ad.m3u8", "https://cdn.example/path/audio/de.m3u8"))
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

    private fun optionValues(args: List<String>, option: String): List<String> =
        args.indices.filter { args[it] == option }.map { args[it + 1] }
}
