package mediathek.daten.watchlist

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.exists
import kotlin.io.path.readText

internal class WatchlistStorageTest {
    @TempDir
    lateinit var tempDir: Path

    private fun storageFile(): Path = tempDir.resolve("watchlist.json")

    @Test
    fun writeAndReadRoundTripsWatchlist() {
        val storageFile = storageFile()
        val entry = DatenWatchlistEntry(
            id = "entry-1",
            name = "Tagesschau",
            sender = "ARD",
            thema = "Tagesschau",
            title = "Tagesschau 20:00 Uhr",
            seenFilmIds = setOf("film-a", "film-b"),
        )
        val notification = WatchlistNotification(
            entryId = "entry-1",
            entryName = "Tagesschau",
            filmId = "film-c",
            sender = "ARD",
            thema = "Tagesschau",
            title = "Tagesschau 20:00 Uhr",
            sendeDatum = "25.07.2026",
            urlNormalQuality = "https://example.org/tagesschau.mp4",
        )

        WatchlistStorage.write(
            storageFile,
            WatchlistSnapshot(listOf(entry), listOf(notification), hasUnseenNotifications = true),
        )

        val json = storageFile.readText()
        assertTrue(json.contains("\"version\": 1"))
        assertTrue(json.contains("\"seenFilmIds\""))
        assertTrue(json.contains("\"filmId\": \"film-c\""))

        val restored = WatchlistStorage.read(storageFile)
        assertTrue(restored.hasUnseenNotifications)
        assertEquals(entry, restored.entries.single())
        assertEquals(notification, restored.notifications.single())
    }

    @Test
    fun writeLeavesNoTemporaryFilesBehind() {
        val storageFile = storageFile()

        WatchlistStorage.write(storageFile, WatchlistSnapshot())

        val leftovers = Files.list(tempDir).use { paths ->
            paths.filter { path -> path.fileName.toString().endsWith(".tmp") }.toList()
        }
        assertTrue(leftovers.isEmpty(), "expected no temporary files, found $leftovers")
    }

    @Test
    fun readReturnsEmptySnapshotWhenFileDoesNotExist() {
        val restored = WatchlistStorage.read(tempDir.resolve("missing.json"))

        assertTrue(restored.entries.isEmpty())
        assertTrue(restored.notifications.isEmpty())
        assertFalse(restored.hasUnseenNotifications)
    }

    @Test
    fun readRejectsUnsupportedFileVersionInsteadOfDowngradingIt() {
        val storageFile = storageFile()
        Files.writeString(
            storageFile,
            """
            {
              "version": 99,
              "entries": [ { "id": "future", "sender": "ZDF", "thema": "heute" } ]
            }
            """.trimIndent(),
        )

        assertThrows<UnsupportedWatchlistVersionException> { WatchlistStorage.read(storageFile) }
    }

    @Test
    fun readFailsLoudlyOnCorruptedFile() {
        val storageFile = storageFile()
        Files.writeString(storageFile, "{ this is not valid json")

        assertThrows<Exception> { WatchlistStorage.read(storageFile) }
    }

    @Test
    fun readIgnoresUnknownFutureFieldsOfSupportedVersion() {
        val storageFile = storageFile()
        Files.writeString(
            storageFile,
            """
            {
              "version": 1,
              "futureFlag": true,
              "hasUnseenNotifications": true,
              "entries": [
                { "id": "e1", "name": "Heute", "sender": "ZDF", "thema": "heute", "futureField": "x" }
              ]
            }
            """.trimIndent(),
        )

        val restored = WatchlistStorage.read(storageFile)

        val entry = restored.entries.single()
        assertEquals("e1", entry.id)
        assertEquals("ZDF", entry.sender)
        assertEquals("", entry.title)
        assertTrue(entry.seenFilmIds.isEmpty())
    }

    @Test
    fun quarantineMovesFileAsideAndKeepsPreviousQuarantines() {
        val storageFile = storageFile()
        Files.writeString(storageFile, "broken-1")

        val firstQuarantine = WatchlistStorage.quarantine(storageFile)
        assertFalse(storageFile.exists())
        assertEquals("broken-1", firstQuarantine.readText())

        Files.writeString(storageFile, "broken-2")
        val secondQuarantine = WatchlistStorage.quarantine(storageFile)

        assertNotEquals(firstQuarantine, secondQuarantine)
        assertEquals("broken-1", firstQuarantine.readText())
        assertEquals("broken-2", secondQuarantine.readText())
    }
}
