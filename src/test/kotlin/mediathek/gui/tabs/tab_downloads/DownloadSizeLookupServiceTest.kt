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

    private fun lookupEntry(url: String): PersistentLookupCacheEntry =
        PersistentLookupCacheEntry(
            url = url,
            location = Country.DE,
            fetchSizeEnabled = true,
            probeHlsSegments = false,
            byteLength = 123_000_000L,
            storedAtMillis = 1_800_000_000_000L,
        )
}
