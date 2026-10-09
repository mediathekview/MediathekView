package mediathek.gui.tabs.tab_downloads

import mediathek.daten.Country
import mediathek.daten.DatenDownload
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class DownloadSizeLookupServiceTest {
    @Test
    fun invalidateRemovesEntriesForFinishedDownloadUrls() {
        val matchingUrl = "https://example.invalid/video-a.mp4"
        val otherUrl = "https://example.invalid/video-b.mp4"
        val service = DownloadSizeLookupService(
            reloadTable = {},
            persistedLookupResults = listOf(
                lookupEntry(matchingUrl),
                lookupEntry(otherUrl),
            ),
        )

        service.invalidate(
            DatenDownload().apply {
                downloadUrl = matchingUrl
            }
        )

        val snapshot = service.snapshotLookupResults()

        assertEquals(1, snapshot.size)
        assertEquals(otherUrl, snapshot.single().url)
    }

    @Test
    fun invalidateIgnoresBlankDownloadUrls() {
        val url = "https://example.invalid/video.mp4"
        val service = DownloadSizeLookupService(
            reloadTable = {},
            persistedLookupResults = listOf(lookupEntry(url)),
        )

        service.invalidate(
            DatenDownload().apply {
                downloadUrl = ""
            }
        )

        assertTrue(service.snapshotLookupResults().any { entry -> entry.url == url })
    }

    @Test
    fun cacheKeepsSeparateEntriesForDifferentQualitiesOfSameUrl() {
        val url = "https://example.invalid/video.m3u8"
        val service = DownloadSizeLookupService(
            reloadTable = {},
            persistedLookupResults = listOf(
                lookupEntry(url, quality = "HIGH_QUALITY", byteLength = 30_000_000L),
                lookupEntry(url, quality = "LOW", byteLength = 10_000_000L),
            ),
        )

        val snapshot = service.snapshotLookupResults().sortedBy { it.quality }

        assertEquals(2, snapshot.size)
        assertEquals(listOf("HIGH_QUALITY", "LOW"), snapshot.map { it.quality })
        assertEquals(listOf(30_000_000L, 10_000_000L), snapshot.map { it.byteLength })
    }

    private fun lookupEntry(
        url: String,
        quality: String? = null,
        byteLength: Long = 123_000_000L,
    ): PersistentLookupCacheEntry =
        PersistentLookupCacheEntry(
            url = url,
            location = Country.DE,
            fetchSizeEnabled = true,
            probeHlsSegments = false,
            byteLength = byteLength,
            storedAtMillis = 1_800_000_000_000L,
            quality = quality,
        )
}
