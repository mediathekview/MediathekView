package mediathek.tool

import kotlinx.coroutines.runBlocking
import mediathek.daten.FilmResolution
import mediathek.gui.dialog.download.DownloadQualitySupport
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal object ArteQualityFixture {
    const val URL = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/id/de/XQ/master.m3u8"
    val variants = listOf(432 to "avc1", 1080 to "avc1", 720 to "avc1", 432 to "hev1", 1080 to "hev1", 720 to "hev1")
    fun master(variants: List<Pair<Int, String>> = this.variants): String = buildString {
        appendLine("#EXTM3U")
        variants.forEachIndexed { index, (height, codec) ->
            appendLine("#EXT-X-STREAM-INF:BANDWIDTH=1000000,RESOLUTION=1920x$height,CODECS=\"$codec.1,mp4a.40.2\",AUDIO=\"audio_$index\"")
            appendLine("v$index.m3u8")
            appendLine("#EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"audio_$index\",NAME=\"Track $index\",LANGUAGE=\"de\",CHARACTERISTICS=\"public.accessibility.describes-video\",URI=\"a$index.m3u8\"")
        }
    }
}

class ArteHlsQualitySelectorTest {
    @Test
    fun `downloads probes and size estimates agree for six streams AVC fallback and reordered streams`() {
        val fixtures = listOf(
            ArteQualityFixture.variants to listOf(4, 2, 0),
            ArteQualityFixture.variants.take(3) to listOf(1, 2, 0),
            ArteQualityFixture.variants.reversed() to listOf(1, 3, 5),
            listOf(1080 to "hvc1", 1080 to "avc1", 720 to "avc1", 432 to "avc1") to listOf(0, 2, 3),
            listOf(720 to "hev1", 1080 to "avc1", 720 to "avc1", 432 to "avc1") to listOf(1, 2, 3),
            listOf(720 to "hev1", 1080 to "avc1", 432 to "avc1") to listOf(1, 2, 2),
        )
        val qualities = listOf(FilmResolution.Enum.HIGH_QUALITY, FilmResolution.Enum.NORMAL, FilmResolution.Enum.LOW)
        for ((variants, expected) in fixtures) {
            val master = ArteQualityFixture.master(variants)
            qualities.zip(expected).forEach { (quality, index) ->
                assertEquals(index, ArteHlsQualitySelector.selectProgram(master, quality))
                assertEquals(index, DownloadQualitySupport.selectArteProgram(ArteQualityFixture.URL, quality) {
                    ArteHlsQualitySelector.Playlist(it, master)
                })
                val estimate = runBlocking {
                    HlsPlaylistSizeEstimator().estimate(
                        playlistUrl = ArteQualityFixture.URL.toHttpUrl(), quality = quality.name,
                        textLoader = { if (it.toString() == ArteQualityFixture.URL) master else "#EXTM3U\n#EXTINF:10,\nsegment.ts\n#EXT-X-ENDLIST" },
                        contentLengthLoader = { 100L },
                    )
                }
                assertEquals(index, estimate.selectedVariant.programId)
                assertEquals("v$index.m3u8", estimate.selectedVariant.playlistUrl.pathSegments.last())
            }
        }
    }

    @Test
    fun `I frame entries do not change program IDs and missing metadata uses bounded AVC fallback`() {
        val master = ArteQualityFixture.master().replace("#EXTM3U", "#EXTM3U\n#EXT-X-I-FRAME-STREAM-INF:BANDWIDTH=100,URI=\"iframe.m3u8\"")
        assertEquals(4, ArteHlsQualitySelector.selectProgram(master, FilmResolution.Enum.HIGH_QUALITY))
        assertEquals(0, ArteHlsQualitySelector.selectProgram("#EXTM3U\n#EXT-X-STREAM-INF:BANDWIDTH=100\nonly.m3u8", FilmResolution.Enum.HIGH_QUALITY))
        assertNull(ArteHlsQualitySelector.selectProgram("#EXTM3U", FilmResolution.Enum.LOW))
        assertThrows(IllegalArgumentException::class.java) { ArteHlsQualitySelector.selectProgram("invalid", FilmResolution.Enum.LOW) }
    }

    @Test
    fun `probe failure retains AVC default and other providers do not fetch`() {
        for ((quality, index) in listOf(FilmResolution.Enum.HIGH_QUALITY to 1, FilmResolution.Enum.NORMAL to 2, FilmResolution.Enum.LOW to 0)) {
            assertEquals(index, DownloadQualitySupport.selectArteProgram(ArteQualityFixture.URL, quality) { error("offline") })
        }
        assertNull(DownloadQualitySupport.selectArteProgram("https://example.org/master.m3u8", FilmResolution.Enum.HIGH_QUALITY) { fail("Unexpected fetch") })
        assertNull(DownloadQualitySupport.selectArteProgram(ArteQualityFixture.URL, null) { fail("Unexpected fetch") })
    }
}
