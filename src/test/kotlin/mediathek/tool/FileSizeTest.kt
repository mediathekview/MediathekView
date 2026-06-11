package mediathek.tool

import mediathek.config.StandardLocations
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path

internal class FileSizeTest {
    @TempDir
    lateinit var tempDir: Path

    private var previousPortableBaseDirectory: String? = null

    @BeforeEach
    fun setUp() {
        previousPortableBaseDirectory = StandardLocations.portableBaseDirectory
        StandardLocations.portableBaseDirectory = tempDir.toString()
    }

    @AfterEach
    fun tearDown() {
        StandardLocations.portableBaseDirectory = previousPortableBaseDirectory
    }

    @Test
    fun estimatesM3u8SizeViaHlsEstimator() {
        val size = FileSize.lookupFileSize(
            url = "https://example.org/video/master.m3u8".toHttpUrl(),
            forceFetch = true,
            quality = "TEST_ONLY",
            cachedHlsLookup = { _, _ -> null },
            directSizeLoader = { error("direct loader should not be used for m3u8") },
            hlsSizeLoader = { _, _ ->
                FileSize.HlsLookupResult(
                    byteLength = 12_500_000L,
                    resolutionUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl(),
                )
            },
            hlsLookupLogger = { _, result -> result },
        ).byteLength

        assertEquals(12_500_000L, size)
    }

    @Test
    fun returnsInvalidSizeForSmallM3u8Estimate() {
        val size = FileSize.lookupFileSize(
            url = "https://example.org/video/master.m3u8".toHttpUrl(),
            forceFetch = true,
            quality = "TEST_ONLY",
            cachedHlsLookup = { _, _ -> null },
            directSizeLoader = { error("direct loader should not be used for m3u8") },
            hlsSizeLoader = { _, _ ->
                FileSize.HlsLookupResult(
                    byteLength = 500_000L,
                    resolutionUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl(),
                )
            },
            hlsLookupLogger = { _, result -> result },
        ).byteLength

        assertEquals(FileSize.INVALID_SIZE.toLong(), size)
    }

    @Test
    fun usesHlsEstimatorWithoutSegmentProbeWhenSegmentProbeIsDisabled() {
        val size = FileSize.lookupFileSize(
            url = "https://example.org/video/master.m3u8".toHttpUrl(),
            forceFetch = true,
            quality = "TEST_ONLY",
            probeHlsSegments = false,
            cachedHlsLookup = { _, _ -> null },
            directSizeLoader = { error("direct loader should not be used for m3u8") },
            hlsSizeLoader = { _, probeSegments ->
                assertFalse(probeSegments)
                FileSize.HlsLookupResult(
                    byteLength = 23_000_000L,
                    resolutionUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl(),
                )
            },
            hlsLookupLogger = { _, result -> result },
        ).byteLength

        assertEquals(23_000_000L, size)
    }

    @Test
    fun usesCachedHlsResultWhenSegmentProbeIsDisabled() {
        val size = FileSize.lookupFileSize(
            url = "https://example.org/video/master.m3u8".toHttpUrl(),
            forceFetch = true,
            quality = "TEST_ONLY",
            probeHlsSegments = false,
            cachedHlsLookup = { _, _ ->
                FileSize.LookupResult(
                    byteLength = 23_000_000L,
                    resolutionUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl(),
                    quality = "TEST_ONLY",
                )
            },
            directSizeLoader = { error("direct loader should not be used for m3u8") },
            hlsSizeLoader = { _, _ -> error("HLS segment probe should not be used") },
            hlsLookupLogger = { _, result -> result },
        ).byteLength

        assertEquals(23_000_000L, size)
    }

    @Test
    fun skipsTelemetryForUnknownHlsLookupFailures() {
        val size = FileSize.lookupFileSize(
            url = "https://example.org/video/master.m3u8".toHttpUrl(),
            forceFetch = true,
            quality = "HIGH_QUALITY",
            cachedHlsLookup = { _, _ -> null },
            directSizeLoader = { error("direct loader should not be used for m3u8") },
            hlsSizeLoader = { _, _ -> throw IOException("network failure") },
            hlsLookupLogger = { _, result -> result },
        ).byteLength

        assertEquals(FileSize.INVALID_SIZE.toLong(), size)
        assertFalse(Files.exists(tempDir.resolve("hls-stream-info-data.ndjson")))
    }

    @Test
    fun returnsInvalidSizeForPrivateHlsUrl() {
        val size = FileSize.lookupFileSize(
            url = "http://127.0.0.1/video/master.m3u8".toHttpUrl(),
            forceFetch = true,
            quality = "HIGH_QUALITY",
        ).byteLength

        assertEquals(FileSize.INVALID_SIZE.toLong(), size)
    }

    @Test
    fun parsesLessThanOneMegabyteTextAsOneMegabyte() {
        assertEquals(1, FileSize.megabyteTextToInt("<1"))
        assertEquals(FileSize.ONE_MIB.toLong(), FileSize.megabyteTextToBytes("<1"))
    }
}
