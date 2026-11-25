package mediathek.controller.starter

import com.sun.net.httpserver.HttpServer
import kotlinx.serialization.json.*
import mediathek.daten.DatenDownload
import mediathek.daten.DatenProg
import mediathek.daten.DownloadType
import mediathek.daten.FilmResolution
import mediathek.gui.dialog.download.DownloadQualitySupport
import mediathek.tool.ArteHlsQualitySelector
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Assumptions.assumeTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.net.InetSocketAddress
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.TimeUnit

internal class ArteHlsDownloadIntegrationTest {
    @TempDir
    lateinit var directory: Path

    @Test
    fun `quality selection muxes the matching subtitle group into MP4 and MKV`() {
        val ffmpeg = executable("ffmpeg")
        val ffprobe = executable("ffprobe")
        assumeTrue(ffmpeg != null && ffprobe != null, "FFmpeg and FFprobe required for media integration test")
        val encoders = availableEncoders(runCommand(listOf(ffmpeg!!, "-hide_banner", "-encoders")))
        assumeTrue(encoders.containsAll(setOf("libx264", "aac", "webvtt", "srt", "mov_text")),
            "FFmpeg media integration test requires H.264, AAC, WebVTT, SubRip and mov_text encoders")
        val master = """
            #EXTM3U
            #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="high",NAME="Français",LANGUAGE="fr",DEFAULT=YES,AUTOSELECT=YES,URI="720_vtt.m3u8"
            #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="low",NAME="Deutsch",LANGUAGE="deu",DEFAULT=YES,AUTOSELECT=YES,URI="480_vtt.m3u8"
            #EXT-X-STREAM-INF:BANDWIDTH=200000,RESOLUTION=720x720,SUBTITLES="high"
            720.m3u8
            #EXT-X-STREAM-INF:BANDWIDTH=100000,RESOLUTION=480x480,SUBTITLES="low"
            480.m3u8
        """.trimIndent()
        val localMaster = Files.writeString(directory.resolve("master.m3u8"), master)
        for (size in listOf(480, 720)) {
            val captions = Files.writeString(directory.resolve("$size.srt"),
                "1\n00:00:00,250 --> 00:00:01,500\nOpening caption for $size\n\n" +
                    "2\n00:00:03,000 --> 00:00:04,500\nCaption for $size\n\n")
            runCommand(listOf(ffmpeg, "-v", "error", "-f", "lavfi", "-i", "color=size=${size}x$size:rate=25",
                "-f", "lavfi", "-i", "sine=frequency=440", "-i", captions.toString(), "-t", "6",
                "-map", "0:v", "-map", "1:a", "-map", "2:s", "-c:v", "libx264", "-g", "50", "-c:a", "aac",
                "-c:s", "webvtt", "-f", "hls", "-hls_time", "2", "-var_stream_map", "v:0,a:0,s:0,sgroup:subs",
                directory.resolve("$size.m3u8").toString()))
        }
        val remoteUrl = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/id/de/XQ/master.m3u8"
        for ((quality, extension) in listOf(FilmResolution.Enum.HIGH_QUALITY to "mp4", FilmResolution.Enum.LOW to "mkv")) {
            val mp4 = extension == "mp4"
            val codec = if (mp4) "mov_text" else "srt"
            val audioLanguage = if (mp4) "-metadata:s:a:0 language=eng " else ""
            val target = directory.resolve("film.$extension")
            val program = DatenProg("ffmpeg", ffmpeg,
                "-i %f %arteHlsWithSubtitles -c:v copy -c:a copy -c:s $codec ${audioLanguage}**", "false", "false")
            val invocation = DownloadProgramInvocationBuilder.build(
                DownloadType.PROGRAM, program,
                DownloadInvocationRequest(remoteUrl, "", directory.toString(), target.fileName.toString(), target.toString(), "", "", quality),
            )
            val download = DatenDownload().apply {
                selectedResolution = quality
                automaticArteHlsProgramMap = invocation.automaticArteHlsProgramMap
                sender = "ARTE.DE"
                art = DownloadType.PROGRAM
                downloadUrl = remoteUrl
                targetPathFileName = target.toString()
                programInvocationArray = invocation.commandArray
            }
            val prepared = requireNotNull(ArteHlsDownloadPreparer.prepare(download) { ArteHlsQualitySelector.Playlist(it, master) })
            runCommand(prepared.map { if (it == remoteUrl) localMaster.toString() else it })
            val report = Json.parseToJsonElement(runCommand(listOf(ffprobe!!, "-v", "error", "-show_streams", "-of", "json", target.toString()))).jsonObject
            val streams = report.getValue("streams").jsonArray.map { it.jsonObject }
            val video = streams.single { it.getValue("codec_type").jsonPrimitive.content == "video" }
            val subtitle = streams.single { it.getValue("codec_type").jsonPrimitive.content == "subtitle" }
            assertEquals(if (mp4) 720 else 480, video.getValue("width").jsonPrimitive.int)
            val audio = streams.single { it.getValue("codec_type").jsonPrimitive.content == "audio" }
            if (mp4) assertEquals("eng", audio["tags"]?.jsonObject?.get("language")?.jsonPrimitive?.content)
            assertEquals(if (mp4) "mov_text" else "subrip", subtitle.getValue("codec_name").jsonPrimitive.content)
            assertEquals(if (mp4) "fra" else "deu", subtitle["tags"]?.jsonObject?.get("language")?.jsonPrimitive?.content)
            val cues = runCommand(listOf(ffmpeg, "-v", "error", "-i", target.toString(), "-map", "0:s:0", "-f", "srt", "-"))
            assertEquals("Caption for ${if (mp4) 720 else 480}", cues.trim().lineSequence().last())
        }
    }

    @ParameterizedTest
    @MethodSource("audioNames")
    fun `FFmpeg preserves selected tracks and labels or safely retains the original mapping`(
        name: String,
        language: String,
        isoLanguage: String,
        disableHttpSeeking: Boolean,
        preferHevc: Boolean,
    ) {
        val ffmpeg = executable("ffmpeg")
        val ffprobe = executable("ffprobe")
        assumeTrue(ffmpeg != null && ffprobe != null, "FFmpeg and FFprobe required for media integration test")
        val quotedSelectors = requireNotNull(ArteHlsDownloadPreparer.detectQuotedSelectors(ffmpeg!!))
        val originalMapping = !quotedSelectors && (',' in name || '?' in name)
        val encoders = availableEncoders(runCommand(listOf(ffmpeg, "-hide_banner", "-encoders")))
        val missingEncoders = (listOf("libx264", "aac") + if (preferHevc) listOf("libx265") else emptyList()).filterNot { it in encoders }
        assumeTrue(missingEncoders.isEmpty(),
            "FFmpeg media integration test requires unavailable encoders: ${missingEncoders.joinToString()}; executable: $ffmpeg")
        val master = """
            #EXTM3U
            #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="other",NAME="Wrong track",LANGUAGE="fr",DEFAULT=YES,URI="wrong.m3u8"
            #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="audio",NAME="$name",LANGUAGE="$language",DEFAULT=YES,URI="de.m3u8"
            #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID="audio",NAME="Audiodeskription <> ä",LANGUAGE="de",DEFAULT=YES,CHARACTERISTICS="public.accessibility.describes-video",URI="ad.m3u8"
            #EXT-X-STREAM-INF:BANDWIDTH=100000,RESOLUTION=16x16,AUDIO="other"
            low.m3u8
            #EXT-X-STREAM-INF:BANDWIDTH=200000,RESOLUTION=32x32,AUDIO="audio"
            high.m3u8
        """.trimIndent() + if (preferHevc) """

            #EXT-X-STREAM-INF:BANDWIDTH=150000,RESOLUTION=32x32,CODECS="hev1.1.6.L90.B0,mp4a.40.2",AUDIO="audio"
            hevc.m3u8
        """.trimIndent().let { "\n$it" } else ""
        if (preferHevc) {
            runCommand(listOf(ffmpeg, "-v", "error", "-f", "lavfi", "-i", "color=size=32x32:rate=1", "-t", "1",
                "-c:v", "libx265", "-x265-params", "pools=1:frame-threads=1", "-f", "hls", directory.resolve("hevc.m3u8").toString()))
        }
        listOf("low" to 16, "high" to 32).forEach { (name, size) ->
            runCommand(listOf(ffmpeg, "-v", "error", "-f", "lavfi", "-i", "color=size=${size}x$size:rate=1", "-t", "1", "-c:v", "libx264", "-f", "hls", directory.resolve("$name.m3u8").toString()))
        }
        listOf("wrong" to 660, "de" to 440, "ad" to 880).forEach { (name, frequency) ->
            runCommand(listOf(ffmpeg, "-v", "error", "-f", "lavfi", "-i", "sine=frequency=$frequency", "-t", "1", "-c:a", "aac", "-f", "hls", directory.resolve("$name.m3u8").toString()))
        }
        Files.writeString(directory.resolve("master.m3u8"), master)
        val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
        server.createContext("/") { exchange ->
            exchange.use {
                val bytes = Files.readAllBytes(directory.resolve(exchange.requestURI.path.removePrefix("/")))
                exchange.sendResponseHeaders(200, bytes.size.toLong())
                exchange.responseBody.write(bytes)
            }
        }
        server.start()
        try {
            val remoteUrl = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/id/de/XQ/master.m3u8"
            val inputOptions = if (disableHttpSeeking) listOf("-http_seekable", "0") else emptyList()
            val args = listOf(ffmpeg) + inputOptions + listOf("-i", remoteUrl, "-map", "p:1", "-map", "-0:s", "-c", "copy", "-bsf:a", "aac_adtstoasc", directory.resolve("a film.mp4").toString())
            val download = DatenDownload().apply {
                selectedResolution = FilmResolution.Enum.HIGH_QUALITY
                automaticArteHlsProgramMap = true
                sender = "ARTE.DE"
                art = DownloadType.PROGRAM
                downloadUrl = remoteUrl
                targetPathFileName = args.last()
                programInvocationArray = args.joinToString(RuntimeExec.TRENNER_PROG_ARRAY)
            }
            val prepared = ArteHlsDownloadPreparer.prepare(download) {
                ArteHlsQualitySelector.Playlist(it, master)
            }
            if (originalMapping) assertNull(prepared)
            val invocation = if (originalMapping) args else requireNotNull(prepared)
            assertEquals(args.joinToString(RuntimeExec.TRENNER_PROG_ARRAY), download.programInvocationArray)
            assertEquals(listOf(ffmpeg) + inputOptions + listOf("-i", remoteUrl), invocation.take(3 + inputOptions.size))
            // Serve the same public-URL fixture locally; production preparation rejects loopback URLs.
            val remoteBase = remoteUrl.substringBeforeLast('/') + "/"
            val localBase = "http://127.0.0.1:${server.address.port}/"
            val localArgs = invocation.map { if (it.startsWith(remoteBase)) it.replaceFirst(remoteBase, localBase) else it }
            runCommand(localArgs)
            val report = Json.parseToJsonElement(runCommand(listOf(ffprobe!!, "-v", "error", "-show_streams", "-of", "json", args.last()))).jsonObject
            val streams = report.getValue("streams").jsonArray.map { it.jsonObject }
            val video = streams.single { it["codec_type"]?.jsonPrimitive?.content == "video" }
            assertEquals(32, video.getValue("width").jsonPrimitive.int)
            assertEquals(if (preferHevc) "hevc" else "h264", video.getValue("codec_name").jsonPrimitive.content)
            val selectedProgram = DownloadQualitySupport.selectArteProgram(remoteUrl, FilmResolution.Enum.HIGH_QUALITY) {
                ArteHlsQualitySelector.Playlist(it, master)
            }
            val probe = Json.parseToJsonElement(runCommand(listOf(ffprobe, "-v", "error", "-select_streams", "p:$selectedProgram:v:0",
                "-show_streams", "-of", "json", "${localBase}master.m3u8"))).jsonObject
            assertEquals(video.getValue("codec_name"), probe.getValue("streams").jsonArray.single().jsonObject.getValue("codec_name"))
            val audio = streams.filter { it["codec_type"]?.jsonPrimitive?.content == "audio" }
            assertEquals(2, audio.size)
            if (!originalMapping) {
                assertEquals(listOf(name, "Audiodeskription <> ä"), audio.map { it.getValue("tags").jsonObject.getValue("handler_name").jsonPrimitive.content })
                assertEquals(listOf(isoLanguage, "deu"), audio.map { it.getValue("tags").jsonObject.getValue("language").jsonPrimitive.content })
                assertTrue(audio.sumOf { it.getValue("disposition").jsonObject.getValue("default").jsonPrimitive.int } <= 1)
                if (supportsMp4VisualImpaired(ffmpeg, ffprobe)) {
                    assertEquals(1, audio[1].getValue("disposition").jsonObject.getValue("visual_impaired").jsonPrimitive.int)
                }
            }
        } finally {
            server.stop(0)
        }
    }

    @Test
    fun `encoder guard recognizes available video and audio encoders`() {
        val report = """
            Encoders:
             V..... = Video
             A..... = Audio
             ------
             V....D libx264              H.264 / AVC
             A..... aac                  AAC (Advanced Audio Coding)
        """.trimIndent()
        assertEquals(setOf("libx264", "aac"), availableEncoders(report))
        assertEquals(setOf("aac"), availableEncoders(report.replace("libx264", "mpeg4")) intersect setOf("libx264", "aac"))
    }

    @Test
    fun `failed FFmpeg command reports arguments and stderr`() {
        val ffmpeg = executable("ffmpeg")
        assumeTrue(ffmpeg != null, "FFmpeg required for diagnostic integration test")
        val failure = assertThrows(AssertionError::class.java) {
            runCommand(listOf(ffmpeg!!, "-mediathekview-invalid-option"))
        }
        assertTrue(failure.message.orEmpty().contains("Command: [$ffmpeg, -mediathekview-invalid-option]"))
        assertTrue(failure.message.orEmpty().contains("Unrecognized option"), failure.message)
    }

    companion object {
        @JvmStatic
        fun audioNames(): List<Arguments> = listOf(
            Arguments.of("Deutsch, Stereo", "de", "deu", false, false),
            Arguments.of("Français (Version d'origine)", "fr", "fra", false, false),
            Arguments.of("Italiano: originale", "it", "ita", false, false),
            Arguments.of("English\\Original", "en", "eng", false, false),
            Arguments.of("Español?", "es", "spa", false, false),
            Arguments.of("Polski, oryginał", "pl", "pol", false, false),
            Arguments.of(" Română: O'Ton <> ? ", "ro", "ron", false, false),
            Arguments.of(" Română: O'Ton <> ", "ro", "ron", false, false),
            Arguments.of("Deutsch", "de", "deu", true, false),
            Arguments.of("Deutsch", "de", "deu", true, true),
        )
    }

    private fun executable(name: String): String? = System.getenv("PATH").orEmpty().split(java.io.File.pathSeparator)
        .map { Path.of(it).resolve(name) }.firstOrNull(Files::isExecutable)?.toString()

    private fun availableEncoders(report: String): Set<String> = report.lineSequence()
        .map { it.trim().split(Regex("""\s+""")) }
        .filter { it.size >= 2 && it[0].matches(Regex("[VAS][A-Z.]{5}")) && it[1] != "=" }
        .map { it[1] }
        .toSet()

    private fun supportsMp4VisualImpaired(ffmpeg: String, ffprobe: String): Boolean {
        // Check the actual writer/reader pair independently of the prepared ARTE command.
        // Older MP4 muxers may discard this flag, and older probes may not read it.
        val probe = directory.resolve("disposition-probe.mp4").toString()
        runCommand(listOf(
            ffmpeg, "-v", "error", "-f", "lavfi", "-i", "sine=frequency=440", "-t", "0.1",
            "-c:a", "aac", "-disposition:a:0", "visual_impaired", probe,
        ))
        val report = Json.parseToJsonElement(runCommand(listOf(
            ffprobe, "-v", "error", "-show_streams", "-of", "json", probe,
        ))).jsonObject
        val stream = report.getValue("streams").jsonArray.single().jsonObject
        return stream.getValue("disposition").jsonObject.getValue("visual_impaired").jsonPrimitive.int == 1
    }

    private fun runCommand(args: List<String>): String {
        val log = Files.createTempFile(directory, "process", ".log")
        val process = ProcessBuilder(args).redirectErrorStream(true).redirectOutput(log.toFile()).start()
        try {
            assertTrue(process.waitFor(20, TimeUnit.SECONDS)) {
                "Command timed out: $args\n${Files.readString(log)}"
            }
            val output = Files.readString(log)
            assertEquals(0, process.exitValue(), "Command: $args\n$output")
            return output
        } finally {
            process.destroyForcibly()
        }
    }
}
