package mediathek.controller.starter

import com.sun.net.httpserver.HttpServer
import kotlinx.serialization.json.*
import mediathek.daten.DatenDownload
import mediathek.daten.DownloadType
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Assumptions.assumeTrue
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

    @ParameterizedTest
    @MethodSource("audioNames")
    fun `FFmpeg preserves selected video and labeled selectable audio tracks`(name: String, language: String, isoLanguage: String) {
        val ffmpeg = executable("ffmpeg")
        val ffprobe = executable("ffprobe")
        assumeTrue(ffmpeg != null && ffprobe != null, "FFmpeg and FFprobe required for media integration test")
        val encoders = runCommand(listOf(ffmpeg!!, "-hide_banner", "-encoders"))
            .lineSequence()
            .map { it.trim().split(Regex("\\s+")) }
            .filter { it.size >= 2 && it[0].matches(Regex("[VAS][A-Z.]{5}")) }
            .map { it[1] }
            .toSet()
        val missingEncoders = listOf("libx264", "aac").filterNot { it in encoders }
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
        """.trimIndent()
        listOf("low" to 16, "high" to 32).forEach { (name, size) ->
            runCommand(listOf(ffmpeg!!, "-v", "error", "-f", "lavfi", "-i", "color=size=${size}x$size:rate=1", "-t", "1", "-c:v", "libx264", "-f", "hls", directory.resolve("$name.m3u8").toString()))
        }
        listOf("wrong" to 660, "de" to 440, "ad" to 880).forEach { (name, frequency) ->
            runCommand(listOf(ffmpeg!!, "-v", "error", "-f", "lavfi", "-i", "sine=frequency=$frequency", "-t", "1", "-c:a", "aac", "-f", "hls", directory.resolve("$name.m3u8").toString()))
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
            val args = listOf(ffmpeg!!, "-i", remoteUrl, "-map", "p:1", "-map", "-0:s", "-c", "copy", "-bsf:a", "aac_adtstoasc", directory.resolve("a film.mp4").toString())
            val download = DatenDownload().apply {
                sender = "ARTE.DE"
                art = DownloadType.PROGRAM
                downloadUrl = remoteUrl
                targetPathFileName = args.last()
                programInvocationArray = args.joinToString(RuntimeExec.TRENNER_PROG_ARRAY)
            }
            val prepared = requireNotNull(ArteHlsDownloadPreparer.prepare(download) {
                ArteHlsDownloadPreparer.Playlist(it, master)
            })
            // Serve the same public-URL fixture locally; production preparation rejects loopback URLs.
            val remoteBase = remoteUrl.substringBeforeLast('/') + "/"
            val localBase = "http://127.0.0.1:${server.address.port}/"
            val localArgs = prepared.map { if (it.startsWith(remoteBase)) it.replaceFirst(remoteBase, localBase) else it }
            val process = requireNotNull(RuntimeExec(null, null, "", "", localArgs).exec(true))
            try {
                assertTrue(process.waitFor(20, TimeUnit.SECONDS), "FFmpeg timed out")
                assertEquals(0, process.exitValue())
            } finally {
                process.destroyForcibly()
            }
            val report = Json.parseToJsonElement(runCommand(listOf(ffprobe!!, "-v", "error", "-show_streams", "-of", "json", args.last()))).jsonObject
            val streams = report.getValue("streams").jsonArray.map { it.jsonObject }
            val video = streams.single { it["codec_type"]?.jsonPrimitive?.content == "video" }
            assertEquals(32, video.getValue("width").jsonPrimitive.int)
            val audio = streams.filter { it["codec_type"]?.jsonPrimitive?.content == "audio" }
            assertEquals(2, audio.size)
            assertEquals(listOf(name, "Audiodeskription <> ä"), audio.map { it.getValue("tags").jsonObject.getValue("handler_name").jsonPrimitive.content })
            assertEquals(listOf(isoLanguage, "deu"), audio.map { it.getValue("tags").jsonObject.getValue("language").jsonPrimitive.content })
            assertTrue(audio.sumOf { it.getValue("disposition").jsonObject.getValue("default").jsonPrimitive.int } <= 1)
            assertEquals(1, audio[1].getValue("disposition").jsonObject.getValue("visual_impaired").jsonPrimitive.int)
        } finally {
            server.stop(0)
        }
    }

    companion object {
        @JvmStatic
        fun audioNames(): List<Arguments> = listOf(
            Arguments.of("Deutsch, Stereo", "de", "deu"),
            Arguments.of("Français (Version d'origine)", "fr", "fra"),
            Arguments.of("Italiano: originale", "it", "ita"),
            Arguments.of("English\\Original", "en", "eng"),
            Arguments.of("Español?", "es", "spa"),
            Arguments.of("Polski, oryginał", "pl", "pol"),
            Arguments.of(" Română: O'Ton <> ? ", "ro", "ron"),
        )
    }

    private fun executable(name: String): String? = System.getenv("PATH").orEmpty().split(java.io.File.pathSeparator)
        .map { Path.of(it).resolve(name) }.firstOrNull(Files::isExecutable)?.toString()

    private fun runCommand(args: List<String>): String {
        val log = Files.createTempFile(directory, "process", ".log")
        val process = ProcessBuilder(args).redirectErrorStream(true).redirectOutput(log.toFile()).start()
        try {
            assertTrue(process.waitFor(20, TimeUnit.SECONDS), "Command timed out: $args")
            val output = Files.readString(log)
            assertEquals(0, process.exitValue(), output)
            return output
        } finally {
            process.destroyForcibly()
        }
    }
}
