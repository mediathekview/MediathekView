package mediathek.daten.watchlist

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.sql.DriverManager
import kotlin.io.path.exists

internal class WatchlistDatabaseStorageTest {
    @TempDir
    lateinit var tempDir: Path

    private val databasePath: Path
        get() = tempDir.resolve("watchlist.db")

    @Test
    fun writeAndReadRoundTripsOrderedSnapshot() {
        val snapshot = completeSnapshot()

        WatchlistDatabaseStorage.write(databasePath, snapshot)

        assertEquals(snapshot, WatchlistDatabaseStorage.read(databasePath))
        openDatabase().use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery("PRAGMA user_version").use { resultSet ->
                    assertTrue(resultSet.next())
                    assertEquals(1, resultSet.getInt(1))
                }
                statement.executeQuery("PRAGMA quick_check").use { resultSet ->
                    assertTrue(resultSet.next())
                    assertEquals("ok", resultSet.getString(1))
                }
            }
        }
    }

    @Test
    fun failedReplacementLeavesPreviousSnapshotCommitted() {
        val original = completeSnapshot()
        WatchlistDatabaseStorage.write(databasePath, original)
        val duplicateIdSnapshot = original.copy(
            entries = listOf(
                original.entries.first(),
                original.entries.first().copy(name = "Duplicate"),
            ),
            notifications = emptyList(),
        )

        assertThrows<Exception> { WatchlistDatabaseStorage.write(databasePath, duplicateIdSnapshot) }

        assertEquals(original, WatchlistDatabaseStorage.read(databasePath))
    }

    @Test
    fun newerDatabaseSchemaIsLeftUntouched() {
        openDatabase().use { connection ->
            connection.createStatement().use { statement -> statement.executeUpdate("PRAGMA user_version=99") }
        }
        val originalBytes = Files.readAllBytes(databasePath)

        val exception = assertThrows<UnsupportedWatchlistVersionException> {
            WatchlistDatabaseStorage.read(databasePath)
        }

        assertEquals(databasePath, exception.protectedPath)
        assertArrayEquals(originalBytes, Files.readAllBytes(databasePath))
    }

    @Test
    fun unreadableDatabaseIsProtectedWithoutBeingQuarantined() {
        val invalidDatabase = "not a sqlite database".toByteArray()
        Files.write(databasePath, invalidDatabase)

        val exception = assertThrows<ProtectedWatchlistLoadException> {
            WatchlistDatabaseStorage.read(databasePath)
        }

        assertEquals(databasePath, exception.protectedPath)
        assertArrayEquals(invalidDatabase, Files.readAllBytes(databasePath))
        assertFalse(tempDir.resolve("watchlist.db.corrupt").exists())
    }

    @Test
    fun missingDatabaseCreatesEmptyCurrentDatabase() {
        val restored = WatchlistDatabaseStorage.read(databasePath)

        assertEquals(WatchlistSnapshot(), restored)
        assertTrue(databasePath.exists())
    }

    @Test
    fun targetedChangesPreserveUnrelatedSnapshotData() {
        val original = completeSnapshot()
        WatchlistDatabaseStorage.write(databasePath, original)

        val acknowledged = original.copy(hasUnseenNotifications = false)
        WatchlistDatabaseStorage.applyChange(
            databasePath,
            acknowledged,
            WatchlistChange.BadgeAcknowledged,
        )
        assertEquals(acknowledged, WatchlistDatabaseStorage.read(databasePath))

        val notificationToRemove = acknowledged.notifications.first()
        val withoutNotification = acknowledged.copy(notifications = acknowledged.notifications.drop(1))
        WatchlistDatabaseStorage.applyChange(
            databasePath,
            withoutNotification,
            WatchlistChange.NotificationRemoved(notificationToRemove.entryId, notificationToRemove.filmId),
        )
        assertEquals(withoutNotification, WatchlistDatabaseStorage.read(databasePath))

        val entryToRemove = original.entries.last()
        val withoutEntry = withoutNotification.copy(
            entries = withoutNotification.entries.dropLast(1),
            notifications = withoutNotification.notifications.filterNot { it.entryId == entryToRemove.id },
        )
        WatchlistDatabaseStorage.applyChange(
            databasePath,
            withoutEntry,
            WatchlistChange.EntriesRemoved(setOf(entryToRemove.id)),
        )
        assertEquals(withoutEntry, WatchlistDatabaseStorage.read(databasePath))
    }

    private fun completeSnapshot(): WatchlistSnapshot {
        val firstEntry = DatenWatchlistEntry(
            id = "entry-1",
            name = "Tagesschau",
            sender = "ARD",
            thema = "Tagesschau",
            seenFilmIds = linkedSetOf("film-b", "film-a"),
        )
        val secondEntry = DatenWatchlistEntry(
            id = "entry-2",
            name = "heute",
            sender = "ZDF",
            thema = "heute",
            title = "19:00",
            seenFilmIds = setOf("film-c"),
        )
        return WatchlistSnapshot(
            entries = listOf(firstEntry, secondEntry),
            notifications = listOf(
                notification(firstEntry, "film-d"),
                notification(secondEntry, "film-e"),
            ),
            hasUnseenNotifications = true,
        )
    }

    private fun notification(entry: DatenWatchlistEntry, filmId: String): WatchlistNotification =
        WatchlistNotification(
            entryId = entry.id,
            entryName = entry.name,
            filmId = filmId,
            sender = entry.sender,
            thema = entry.thema,
            title = "Episode $filmId",
            sendeDatum = "25.07.2026",
            urlNormalQuality = "https://example.org/$filmId.mp4",
        )

    private fun openDatabase() = DriverManager.getConnection("jdbc:sqlite:${databasePath.toAbsolutePath()}")
}
