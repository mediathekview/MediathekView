package mediathek.tool

import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import okhttp3.*
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.ResponseBody.Companion.toResponseBody
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.net.InetAddress
import java.net.UnknownHostException
import java.util.concurrent.atomic.AtomicInteger
import kotlin.time.Duration.Companion.milliseconds

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
    fun selectsRequestedHlsQualityVariant() {
        val masterUrl = "https://example.org/master.m3u8".toHttpUrl()
        val playlists = mapOf(
            masterUrl to """
                #EXTM3U
                #EXT-X-STREAM-INF:BANDWIDTH=2000000,RESOLUTION=768x432,CODECS="avc1.4d401e,mp4a.40.2"
                v432.m3u8
                #EXT-X-STREAM-INF:BANDWIDTH=4800000,RESOLUTION=1920x1080,CODECS="avc1.4d0028,mp4a.40.2"
                v1080.m3u8
                #EXT-X-STREAM-INF:BANDWIDTH=3600000,RESOLUTION=1280x720,CODECS="avc1.4d401f,mp4a.40.2"
                v720.m3u8
                #EXT-X-STREAM-INF:BANDWIDTH=1200000,RESOLUTION=640x360,CODECS="avc1.4d401e,mp4a.40.2"
                v360.m3u8
                #EXT-X-STREAM-INF:BANDWIDTH=5000000,RESOLUTION=1920x1080,CODECS="hev1.2.4.L123.B0,mp4a.40.2"
                v1080_h265.m3u8
            """.trimIndent(),
            "https://example.org/v1080.m3u8".toHttpUrl() to mediaPlaylist("high.ts"),
            "https://example.org/v720.m3u8".toHttpUrl() to mediaPlaylist("normal.ts"),
            "https://example.org/v360.m3u8".toHttpUrl() to mediaPlaylist("low.ts"),
        )
        val segmentSizes = mapOf(
            "https://example.org/high.ts".toHttpUrl() to 3_000L,
            "https://example.org/normal.ts".toHttpUrl() to 2_000L,
            "https://example.org/low.ts".toHttpUrl() to 1_000L,
        )

        fun estimate(quality: String) = runBlocking {
            estimator.estimate(
                playlistUrl = masterUrl,
                quality = quality,
                textLoader = { url -> playlists.getValue(url) },
                contentLengthLoader = { url -> segmentSizes.getValue(url) },
            )
        }

        assertEquals("1920x1080", estimate("HIGH_QUALITY").selectedVariant.resolution)
        assertEquals("1280x720", estimate("NORMAL").selectedVariant.resolution)
        assertEquals("640x360", estimate("LOW").selectedVariant.resolution)
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
    fun boundsConcurrentSegmentLengthLookups() {
        val mediaUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl()
        val segmentCount = 10
        val playlist = buildString {
            appendLine("#EXTM3U")
            repeat(segmentCount) { index ->
                appendLine("#EXTINF:4.0,")
                appendLine("media_$index.ts")
            }
        }
        val activeLookups = AtomicInteger(0)
        val maxActiveLookups = AtomicInteger(0)
        val lookupCount = AtomicInteger(0)
        val boundedEstimator = HlsPlaylistSizeEstimator(segmentParallelism = 3)

        val result = runBlocking {
            boundedEstimator.estimate(
                playlistUrl = mediaUrl,
                textLoader = { playlist },
                contentLengthLoader = {
                    val active = activeLookups.incrementAndGet()
                    maxActiveLookups.updateAndGet { current -> maxOf(current, active) }
                    lookupCount.incrementAndGet()
                    try {
                        delay(10.milliseconds)
                        100L
                    } finally {
                        activeLookups.decrementAndGet()
                    }
                },
            )
        }

        assertEquals(segmentCount, result.segmentCount)
        assertEquals(segmentCount * 100L, result.totalBytes)
        assertEquals(segmentCount, lookupCount.get())
        assertTrue(
            maxActiveLookups.get() <= 3,
            "segment length lookups exceeded configured parallelism: ${maxActiveLookups.get()}",
        )
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
    fun sumsCmafInitializationAndByteRangeSegmentLengths() {
        val mediaUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl()
        val playlists = mapOf(
            mediaUrl to """
                #EXTM3U
                #EXT-X-MAP:URI="init.mp4",BYTERANGE="1000@0"
                #EXTINF:4.0,
                #EXT-X-BYTERANGE:1200@1000
                medias/video.mp4
                #EXTINF:4.0,
                #EXT-X-BYTERANGE:1800@2200
                medias/video.mp4
                #EXTINF:4.0,
                #EXT-X-BYTERANGE:2400
                medias/video.mp4
            """.trimIndent(),
        )

        val result = runBlocking {
            estimator.estimate(
                playlistUrl = mediaUrl,
                textLoader = { url -> playlists.getValue(url) },
                contentLengthLoader = { url -> error("byte-range segment size should not be probed for $url") },
            )
        }

        assertEquals(3, result.segmentCount)
        assertEquals(6_400L, result.totalBytes)
    }

    @Test
    fun estimatesFromVariantBandwidthWithoutSegmentProbing() {
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
                probeSegments = false,
                textLoader = { url -> playlists.getValue(url) },
                contentLengthLoader = { error("segment sizes should not be requested") },
            )
        }

        assertEquals(2, result.segmentCount)
        assertEquals(1_000_000L, result.totalBytes)
    }

    @Test
    fun readsTotalLengthFromContentRangeWhenProbeReturnsPartialResponse() {
        val response = Response.Builder()
            .request(Request.Builder().url("https://example.org/segment.ts").get().build())
            .protocol(Protocol.HTTP_1_1)
            .code(206)
            .message("Partial Content")
            .header("Content-Range", "bytes 0-0/12345")
            .body("x".toResponseBody())
            .build()

        response.use {
            assertEquals(12_345L, estimator.contentLengthOrRangeLength(it))
        }
    }

    @Test
    fun rejectsPrivateVariantPlaylistUrls() {
        val masterUrl = "https://example.org/master.m3u8".toHttpUrl()
        val playlists = mapOf(
            masterUrl to """
                #EXTM3U
                #EXT-X-STREAM-INF:BANDWIDTH=1500000,RESOLUTION=1280x720
                http://127.0.0.1/private.m3u8
            """.trimIndent(),
        )

        val exception = assertThrows(IllegalArgumentException::class.java) {
            runBlocking {
                estimator.estimate(
                    playlistUrl = masterUrl,
                    textLoader = { url -> playlists.getValue(url) },
                    contentLengthLoader = { error("private variant should be rejected before size lookup") },
                )
            }
        }

        assertTrue(exception.message!!.contains("HLS URL host is not allowed"))
    }

    @Test
    fun rejectsPrivateSegmentUrls() {
        val mediaUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl()
        val playlists = mapOf(
            mediaUrl to """
                #EXTM3U
                #EXTINF:5.0,
                http://192.168.1.10/segment.ts
            """.trimIndent(),
        )

        val exception = assertThrows(IllegalArgumentException::class.java) {
            runBlocking {
                estimator.estimate(
                    playlistUrl = mediaUrl,
                    textLoader = { url -> playlists.getValue(url) },
                    contentLengthLoader = { error("private segment should be rejected before size lookup") },
                )
            }
        }

        assertTrue(exception.message!!.contains("HLS URL host is not allowed"))
    }

    @Test
    fun hlsSpecificClientRejectsPrivateResolvedAddresses() {
        val client = HlsEgressPolicy.clientFor(
            OkHttpClient.Builder()
                .dns(
                    Dns {
                        listOf(InetAddress.getByName("10.0.0.5"))
                    },
                )
                .build(),
        )

        val exception = assertThrows(UnknownHostException::class.java) {
            client.dns.lookup("cdn.example.org")
        }

        assertTrue(exception.message!!.contains("local or private address"))
    }

    private fun mediaPlaylist(segment: String): String = """
        #EXTM3U
        #EXTINF:4.0,
        $segment
    """.trimIndent()
}
