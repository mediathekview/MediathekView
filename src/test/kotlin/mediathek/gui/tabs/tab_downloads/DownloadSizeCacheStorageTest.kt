package mediathek.gui.tabs.tab_downloads

import mediathek.daten.Country
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path

internal class DownloadSizeCacheStorageTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun savesAndLoadsFreshEntries() {
        val storagePath = tempDir.resolve("download-size-cache.json")
        val now = 1_800_000_000_000L
        val snapshot = DownloadSizeCacheSnapshot(
            lookupResults = listOf(
                PersistentLookupCacheEntry(
                    url = "https://example.invalid/video.mp4",
                    location = Country.DE,
                    fetchSizeEnabled = true,
                    probeHlsSegments = false,
                    byteLength = 123_000_000L,
                    storedAtMillis = now,
                    httpStatusCode = 200,
                    resolutionUrl = "https://example.invalid/resolved.mp4",
                    quality = "NORMAL",
                )
            ),
            knownAboSizes = listOf(
                PersistentKnownAboSize(
                    key = "https://example.invalid/video.mp4",
                    byteLength = 123_000_000L,
                    storedAtMillis = now,
                )
            ),
        )

        DownloadSizeCacheStorage.save(snapshot, storagePath, nowMillis = now)

        val loaded = DownloadSizeCacheStorage.load(storagePath, nowMillis = now)

        assertEquals(snapshot, loaded)
    }

    @Test
    fun ignoresExpiredEntries() {
        val storagePath = tempDir.resolve("download-size-cache.json")
        val now = 1_800_000_000_000L
        val oldTimestamp = now - 31L * 24 * 60 * 60 * 1_000
        val snapshot = DownloadSizeCacheSnapshot(
            lookupResults = listOf(
                PersistentLookupCacheEntry(
                    url = "https://example.invalid/video.mp4",
                    location = Country.DE,
                    fetchSizeEnabled = true,
                    probeHlsSegments = false,
                    byteLength = 123_000_000L,
                    storedAtMillis = oldTimestamp,
                )
            ),
            knownAboSizes = listOf(
                PersistentKnownAboSize(
                    key = "https://example.invalid/video.mp4",
                    byteLength = 123_000_000L,
                    storedAtMillis = oldTimestamp,
                )
            ),
        )

        DownloadSizeCacheStorage.save(snapshot, storagePath, nowMillis = oldTimestamp)

        val loaded = DownloadSizeCacheStorage.load(storagePath, nowMillis = now)

        assertEquals(DownloadSizeCacheSnapshot(), loaded)
    }
}
