package mediathek.tool

import kotlinx.coroutines.runBlocking
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.Protocol
import okhttp3.Request
import okhttp3.Response
import okhttp3.ResponseBody.Companion.toResponseBody
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class HlsPlaylistSizeEstimatorTest {
    private val estimator = HlsPlaylistSizeEstimator(segmentParallelism = 2)

    @Test
    fun selectsLargestVariantAndReportsAllAvailableBitrates() {
        val masterUrl = "https://example.org/master.m3u8".toHttpUrl()
        val playlists = mapOf(
            masterUrl to """
                #EXTM3U
                #EXT-X-STREAM-INF:BANDWIDTH=500000,RESOLUTION=640x360,CODECS="avc1.42e01e,mp4a.40.2"
                low.m3u8
                #EXT-X-STREAM-INF:BANDWIDTH=1500000,AVERAGE-BANDWIDTH=1400000,RESOLUTION=1280x720
                https://cdn.example.org/high.m3u8
            """.trimIndent(),
            "https://cdn.example.org/high.m3u8".toHttpUrl() to """
                #EXTM3U
                #EXTINF:4.0,
                seg-1.ts
                #EXTINF:4.0,
                seg-2.ts
            """.trimIndent(),
        )
        val segmentSizes = mapOf(
            "https://cdn.example.org/seg-1.ts".toHttpUrl() to 1_000L,
            "https://cdn.example.org/seg-2.ts".toHttpUrl() to 2_000L,
        )

        val result = runBlocking {
            estimator.estimate(
                playlistUrl = masterUrl,
                textLoader = { url -> playlists.getValue(url) },
                contentLengthLoader = { url -> segmentSizes.getValue(url) },
            )
        }

        assertEquals(2, result.availableVariants.size)
        assertEquals(listOf(1_500_000L, 500_000L), result.availableVariants.map { it.bandwidth })
        assertEquals(1_500_000L, result.selectedVariant.bandwidth)
        assertEquals("1280x720", result.selectedVariant.resolution)
        assertEquals(2, result.segmentCount)
        assertEquals(3_000L, result.totalBytes)
    }

    @Test
    fun estimatesDirectMediaPlaylistWithoutVariantMetadata() {
        val mediaUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl()
        val playlists = mapOf(
            mediaUrl to """
                #EXTM3U
                #EXTINF:5.0,
                media_0.ts
                #EXTINF:5.0,
                media_1.ts
            """.trimIndent(),
        )
        val segmentSizes = mapOf(
            "https://example.org/video/media_0.ts".toHttpUrl() to 123L,
            "https://example.org/video/media_1.ts".toHttpUrl() to 456L,
        )

        val result = runBlocking {
            estimator.estimate(
                playlistUrl = mediaUrl,
                textLoader = { url -> playlists.getValue(url) },
                contentLengthLoader = { url -> segmentSizes.getValue(url) },
            )
        }

        assertEquals(0, result.availableVariants.size)
        assertEquals(null, result.selectedVariant.bandwidth)
        assertEquals(2, result.segmentCount)
        assertEquals(579L, result.totalBytes)
    }

    @Test
    fun fallsBackToVariantBandwidthWhenSegmentLengthsAreUnavailable() {
        val masterUrl = "https://example.org/master.m3u8".toHttpUrl()
        val playlists = mapOf(
            masterUrl to """
                #EXTM3U
                #EXT-X-STREAM-INF:BANDWIDTH=800000,RESOLUTION=960x540
                stream.m3u8
            """.trimIndent(),
            "https://example.org/stream.m3u8".toHttpUrl() to """
                #EXTM3U
                #EXTINF:4.0,
                seg-1.ts
                #EXTINF:6.0,
                seg-2.ts
            """.trimIndent(),
        )

        val result = runBlocking {
            estimator.estimate(
                playlistUrl = masterUrl,
                textLoader = { url -> playlists.getValue(url) },
                contentLengthLoader = { url ->
                    throw IllegalStateException("Missing Content-Length for segment: $url")
                },
            )
        }

        assertEquals(2, result.segmentCount)
        assertEquals(1_000_000L, result.totalBytes)
    }

    @Test
    fun measuresBodyWhenContentLengthHeaderIsMissing() {
        val response = Response.Builder()
            .request(Request.Builder().url("https://example.org/segment.ts").get().build())
            .protocol(Protocol.HTTP_1_1)
            .code(200)
            .message("OK")
            .body("segment-data".toResponseBody())
            .build()

        response.use {
            assertEquals(12L, estimator.contentLengthOrBodyLength(it))
        }
    }
}
