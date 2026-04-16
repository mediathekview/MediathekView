package mediathek.tool

import mediathek.config.StandardLocations
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path

internal class HlsStreamInfoLoggerTest {
    @TempDir
    lateinit var tempDir: Path

    private var previousPortableBaseDirectory: String? = null

    @AfterEach
    fun tearDown() {
        StandardLocations.portableBaseDirectory = previousPortableBaseDirectory
    }

    @Test
    fun writesNdjsonEntriesWithQualityField() {
        previousPortableBaseDirectory = StandardLocations.portableBaseDirectory
        StandardLocations.portableBaseDirectory = tempDir.toString()

        HlsStreamInfoLogger.appendEntry(
            httpStatusCode = 200,
            m3u8Url = "https://example.org/video/master.m3u8".toHttpUrl(),
            resolutionUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl(),
            quality = "HIGH_QUALITY",
            fileSize = 12_500_000L,
        )

        val outputPath = tempDir.resolve("hls-stream-info-data.ndjson")
        val content = Files.readString(outputPath)

        assertTrue(content.contains("\"quality\":\"HIGH_QUALITY\""))
        assertFalse(content.contains("\"resolution\":\"HIGH_QUALITY\""))
        assertFalse(content.contains("\"installationId\""))
        assertFalse(content.contains("["))
    }
}
