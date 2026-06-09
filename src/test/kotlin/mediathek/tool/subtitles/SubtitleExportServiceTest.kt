package mediathek.tool.subtitles

import com.sun.net.httpserver.HttpServer
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.net.InetAddress
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

internal class SubtitleExportServiceTest {
    @TempDir
    lateinit var tmp: Path

    @Test
    fun downloadsAndExportsWebVttArtifacts() = runBlocking {
        val url = serve(
            """
            WEBVTT

            00:00:01.000 --> 00:00:02.500 align:center
            <c.textCyan>Hello</c>
            """.trimIndent(),
        )

        val result = SubtitleExportService.downloadAndExport(url, tmp.resolve("subtitle"))

        require(result is SubtitleExportResult.Success)
        assertEquals(listOf("SRT", "ASS", "TTML", "WebVTT"), result.successes)
        assertTrue(result.failures.isEmpty())
        assertTrue(Files.readString(tmp.resolve("subtitle.vtt")).contains("WEBVTT"))
        assertTrue(Files.readString(tmp.resolve("subtitle.ttml")).contains("tts:textAlign=\"center\""))
        assertTrue(Files.readString(tmp.resolve("subtitle.srt")).contains("00:00:01,000 --> 00:00:02,500"))
        assertTrue(Files.readString(tmp.resolve("subtitle.ass")).contains("Dialogue: 0,0:00:01.00,0:00:02.50"))
    }

    @Test
    fun reportsInvalidFormat() = runBlocking {
        val url = serve("not a subtitle")

        val result = SubtitleExportService.downloadAndExport(url, tmp.resolve("subtitle"))

        assertEquals(SubtitleExportResult.InvalidFormat, result)
    }

    @Test
    fun reportsDownloadFailure() = runBlocking {
        val result = SubtitleExportService.downloadAndExport(serve("download failed", status = 500), tmp.resolve("subtitle"))

        assertTrue(result is SubtitleExportResult.Failure)
    }

    private fun serve(content: String, status: Int = 200): String {
        val bytes = content.toByteArray(StandardCharsets.UTF_8)
        val server = HttpServer.create(InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0)
        server.createContext("/") { exchange ->
            exchange.sendResponseHeaders(status, bytes.size.toLong())
            exchange.responseBody.use { response ->
                response.write(bytes)
            }
            server.stop(0)
        }
        server.start()
        return "http://${server.address.hostString}:${server.address.port}/subtitle"
    }
}
