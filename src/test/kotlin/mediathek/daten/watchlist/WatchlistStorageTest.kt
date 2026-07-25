package mediathek.daten.watchlist

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path

internal class WatchlistStorageTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun writeAndReadRoundTripsWatchlist() {
        val storageFile = tempDir.resolve("watchlist.json")
        val entry = DatenWatchlistEntry().apply {
            id = "entry-1"
            name = "Tagesschau"
            sender = "ARD"
            thema = "Tagesschau"
            title = "Tagesschau 20:00 Uhr"
            seenUrlKeys.addAll(listOf("key-a", "key-b"))
        }
        val notification = WatchlistNotification(
            entryId = "entry-1",
            entryName = "Tagesschau",
            sender = "ARD",
            thema = "Tagesschau",
            title = "Tagesschau 20:00 Uhr",
            sendeDatum = "25.07.2026",
            urlNormalQuality = "https://example.org/tagesschau.mp4",
        )
        val snapshot = WatchlistStorage.Snapshot(
            entries = listOf(entry),
            notifications = listOf(notification),
            hasUnseenNotifications = true,
        )

        WatchlistStorage.write(storageFile, snapshot)

        val json = Files.readString(storageFile)
        assertTrue(json.contains("\"version\""))
        assertTrue(json.contains("\"hasUnseenNotifications\": true"))
        assertTrue(json.contains("\"seenUrlKeys\""))

        val restored = WatchlistStorage.read(storageFile)
        assertTrue(restored.hasUnseenNotifications)

        val restoredEntry = restored.entries.single()
        assertEquals("entry-1", restoredEntry.id)
        assertEquals("Tagesschau", restoredEntry.name)
        assertEquals("ARD", restoredEntry.sender)
        assertEquals("Tagesschau", restoredEntry.thema)
        assertEquals("Tagesschau 20:00 Uhr", restoredEntry.title)
        assertEquals(setOf("key-a", "key-b"), restoredEntry.seenUrlKeys)

        assertEquals(listOf(notification), restored.notifications)
    }

    @Test
    fun writeAndReadRoundTripsDefaults() {
        val storageFile = tempDir.resolve("watchlist.json")

        WatchlistStorage.write(storageFile, WatchlistStorage.Snapshot())

        val restored = WatchlistStorage.read(storageFile)
        assertFalse(restored.hasUnseenNotifications)
        assertTrue(restored.entries.isEmpty())
        assertTrue(restored.notifications.isEmpty())
    }

    @Test
    fun readReturnsEmptySnapshotWhenFileDoesNotExist() {
        val restored = WatchlistStorage.read(tempDir.resolve("missing.json"))

        assertFalse(restored.hasUnseenNotifications)
        assertTrue(restored.entries.isEmpty())
        assertTrue(restored.notifications.isEmpty())
    }

    @Test
    fun readReturnsEmptySnapshotWhenFileIsCorrupted() {
        val storageFile = tempDir.resolve("watchlist.json")
        Files.writeString(storageFile, "{ this is not valid json")

        val restored = WatchlistStorage.read(storageFile)

        assertFalse(restored.hasUnseenNotifications)
        assertTrue(restored.entries.isEmpty())
        assertTrue(restored.notifications.isEmpty())
    }

    @Test
    fun readIgnoresUnknownFutureFields() {
        val storageFile = tempDir.resolve("watchlist.json")
        Files.writeString(
            storageFile,
            """
            {
              "version": 99,
              "futureFlag": true,
              "hasUnseenNotifications": true,
              "entries": [
                { "name": "Heute", "sender": "ZDF", "thema": "heute", "futureField": "x" }
              ],
              "notifications": []
            }
            """.trimIndent(),
        )

        val restored = WatchlistStorage.read(storageFile)

        assertTrue(restored.hasUnseenNotifications)
        val restoredEntry = restored.entries.single()
        assertEquals("Heute", restoredEntry.name)
        assertEquals("ZDF", restoredEntry.sender)
        assertEquals("heute", restoredEntry.thema)
        assertEquals("", restoredEntry.title)
        assertTrue(restoredEntry.seenUrlKeys.isEmpty())
    }
}
