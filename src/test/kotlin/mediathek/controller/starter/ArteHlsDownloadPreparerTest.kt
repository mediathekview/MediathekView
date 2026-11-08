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

    private fun prepare(download: DatenDownload = download(), text: String = master): List<String>? =
        ArteHlsDownloadPreparer.prepare(download) { ArteHlsDownloadPreparer.Playlist(it, text) }

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

    @Test
    fun `single variant works without a program map but multiple variants need an explicit selection`() {
        val args = baseArgs.take(3) + baseArgs.drop(7)
        assertNull(prepare(download(args)))
        val singleVariant = master.substringBefore("#EXT-X-STREAM-INF") + "#EXT-X-STREAM-INF:BANDWIDTH=200,AUDIO=\"selected\"\nhigh.m3u8"
        val prepared = requireNotNull(prepare(download(args), singleVariant))
        assertEquals("0:p:0:v:0", optionValues(prepared, "-map").first())
    }

    @Test
    fun `recognizes ambiguous shared rendition URLs after a redirect`() {
        val args = requireNotNull(ArteHlsDownloadPreparer.prepare(download()) {
            ArteHlsDownloadPreparer.Playlist("https://cdn.example/path/master.m3u8".toHttpUrl(), master)
        })
        assertEquals(listOf(url), optionValues(args, "-i"))
        assertNull(ArteHlsDownloadPreparer.prepare(download()) {
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
