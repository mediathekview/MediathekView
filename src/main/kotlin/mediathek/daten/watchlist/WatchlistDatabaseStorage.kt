/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten.watchlist

import mediathek.tool.FileUtils
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import java.nio.file.Files
import java.nio.file.Path
import java.sql.Connection
import kotlin.io.path.createDirectories
import kotlin.io.path.deleteIfExists
import kotlin.io.path.exists

internal object WatchlistDatabaseStorage : WatchlistPersistence {
    override fun read(storagePath: Path): WatchlistSnapshot {
        if (storagePath.exists()) {
            return protectedLoad(storagePath) { readDatabase(storagePath) }
        }

        return protectedLoad(storagePath) { installNewDatabase(storagePath, WatchlistSnapshot()) }
    }

    override fun write(storagePath: Path, snapshot: WatchlistSnapshot) {
        if (!storagePath.exists()) {
            installNewDatabase(storagePath, snapshot)
            return
        }

        rejectNewerSchemaWithoutWriting(storagePath)
        dataSource(storagePath).connection.use { connection ->
            bootstrapSchema(connection, storagePath)
            connection.inTransaction {
                replaceSnapshot(connection, snapshot)
            }
        }
    }

    private fun installNewDatabase(databasePath: Path, snapshot: WatchlistSnapshot): WatchlistSnapshot {
        val directory = databasePath.toAbsolutePath().parent
        directory.createDirectories()
        val temporaryPath = Files.createTempFile(directory, databasePath.fileName.toString() + ".", ".tmp")
        try {
            dataSource(temporaryPath).connection.use { connection ->
                connection.inTransaction {
                    createSchemaV1(connection)
                    replaceSnapshot(connection, snapshot)
                    setCurrentSchemaVersion(connection)
                }
            }

            check(validateDatabase(temporaryPath) == snapshot) {
                "Temporary watchlist database does not contain the complete snapshot"
            }
            checkpointDatabase(temporaryPath)
            check(!databasePath.exists()) { "Watchlist database appeared while it was being initialized" }
            FileUtils.moveAtomicallyWithFallback(temporaryPath, databasePath)
            return validateDatabase(databasePath).also { restored ->
                check(restored == snapshot) { "Installed watchlist database does not contain the complete snapshot" }
            }
        } finally {
            temporaryPath.deleteIfExists()
            temporaryPath.resolveSibling(temporaryPath.fileName.toString() + "-wal").deleteIfExists()
            temporaryPath.resolveSibling(temporaryPath.fileName.toString() + "-shm").deleteIfExists()
        }
    }

    private fun readDatabase(databasePath: Path): WatchlistSnapshot {
        rejectNewerSchemaWithoutWriting(databasePath)
        dataSource(databasePath).connection.use { connection ->
            bootstrapSchema(connection, databasePath)
            return readSnapshot(connection)
        }
    }

    private fun validateDatabase(databasePath: Path): WatchlistSnapshot {
        rejectNewerSchemaWithoutWriting(databasePath)
        dataSource(databasePath).connection.use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery("PRAGMA quick_check").use { resultSet ->
                    check(resultSet.next() && resultSet.getString(1) == "ok") {
                        "Watchlist database integrity check failed"
                    }
                }
            }
            return readSnapshot(connection)
        }
    }

    private fun checkpointDatabase(databasePath: Path) {
        dataSource(databasePath).connection.use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery("PRAGMA wal_checkpoint(TRUNCATE)").use { resultSet ->
                    check(resultSet.next() && resultSet.getInt(1) == 0) {
                        "Could not checkpoint watchlist database"
                    }
                }
            }
        }
    }

    private fun bootstrapSchema(connection: Connection, databasePath: Path) {
        val version = readSchemaVersion(connection)
        if (version > CURRENT_SCHEMA_VERSION) {
            throw UnsupportedWatchlistVersionException(version, databasePath)
        }
        if (version == CURRENT_SCHEMA_VERSION) {
            return
        }

        connection.inTransaction {
            createSchemaV1(connection)
            setCurrentSchemaVersion(connection)
        }
    }

    private fun rejectNewerSchemaWithoutWriting(databasePath: Path) {
        readOnlyDataSource(databasePath).connection.use { connection ->
            val version = readSchemaVersion(connection)
            if (version > CURRENT_SCHEMA_VERSION) {
                throw UnsupportedWatchlistVersionException(version, databasePath)
            }
        }
    }

    private fun createSchemaV1(connection: Connection) {
        connection.createStatement().use { statement ->
            statement.executeUpdate(CREATE_STATE_TABLE_SQL)
            statement.executeUpdate(CREATE_ENTRIES_TABLE_SQL)
            statement.executeUpdate(CREATE_SEEN_FILM_IDS_TABLE_SQL)
            statement.executeUpdate(CREATE_NOTIFICATIONS_TABLE_SQL)
        }
    }

    private fun replaceSnapshot(connection: Connection, snapshot: WatchlistSnapshot) {
        connection.createStatement().use { statement ->
            statement.executeUpdate("DELETE FROM watchlist_notifications")
            statement.executeUpdate("DELETE FROM watchlist_seen_film_ids")
            statement.executeUpdate("DELETE FROM watchlist_entries")
            statement.executeUpdate("DELETE FROM watchlist_state")
        }

        connection.prepareStatement(INSERT_STATE_SQL).use { statement ->
            statement.setBoolean(1, snapshot.hasUnseenNotifications)
            statement.executeUpdate()
        }
        connection.prepareStatement(INSERT_ENTRY_SQL).use { entryStatement ->
            connection.prepareStatement(INSERT_SEEN_FILM_ID_SQL).use { seenStatement ->
                snapshot.entries.forEachIndexed { position, entry ->
                    entryStatement.setString(1, entry.id)
                    entryStatement.setInt(2, position)
                    entryStatement.setString(3, entry.name)
                    entryStatement.setString(4, entry.sender)
                    entryStatement.setString(5, entry.thema)
                    entryStatement.setString(6, entry.title)
                    entryStatement.addBatch()

                    entry.seenFilmIds.forEach { filmId ->
                        seenStatement.setString(1, entry.id)
                        seenStatement.setString(2, filmId)
                        seenStatement.addBatch()
                    }
                }
                entryStatement.executeBatch()
                seenStatement.executeBatch()
            }
        }
        connection.prepareStatement(INSERT_NOTIFICATION_SQL).use { statement ->
            snapshot.notifications.forEachIndexed { position, notification ->
                statement.setInt(1, position)
                statement.setString(2, notification.entryId)
                statement.setString(3, notification.entryName)
                statement.setString(4, notification.filmId)
                statement.setString(5, notification.sender)
                statement.setString(6, notification.thema)
                statement.setString(7, notification.title)
                statement.setString(8, notification.sendeDatum)
                statement.setString(9, notification.urlNormalQuality)
                statement.addBatch()
            }
            statement.executeBatch()
        }
    }

    private fun readSnapshot(connection: Connection): WatchlistSnapshot {
        val seenFilmIds = mutableMapOf<String, MutableSet<String>>()
        connection.createStatement().use { statement ->
            statement.executeQuery(SELECT_SEEN_FILM_IDS_SQL).use { resultSet ->
                while (resultSet.next()) {
                    seenFilmIds.getOrPut(resultSet.getString("entry_id"), ::linkedSetOf)
                        .add(resultSet.getString("film_id"))
                }
            }
        }

        val entries = connection.createStatement().use { statement ->
            statement.executeQuery(SELECT_ENTRIES_SQL).use { resultSet ->
                buildList {
                    while (resultSet.next()) {
                        val id = resultSet.getString("id")
                        add(
                            DatenWatchlistEntry(
                                id = id,
                                name = resultSet.getString("name"),
                                sender = resultSet.getString("sender"),
                                thema = resultSet.getString("thema"),
                                title = resultSet.getString("title"),
                                seenFilmIds = seenFilmIds[id].orEmpty(),
                            )
                        )
                    }
                }
            }
        }
        val notifications = connection.createStatement().use { statement ->
            statement.executeQuery(SELECT_NOTIFICATIONS_SQL).use { resultSet ->
                buildList {
                    while (resultSet.next()) {
                        add(
                            WatchlistNotification(
                                entryId = resultSet.getString("entry_id"),
                                entryName = resultSet.getString("entry_name"),
                                filmId = resultSet.getString("film_id"),
                                sender = resultSet.getString("sender"),
                                thema = resultSet.getString("thema"),
                                title = resultSet.getString("title"),
                                sendeDatum = resultSet.getString("sende_datum"),
                                urlNormalQuality = resultSet.getString("url_normal_quality"),
                            )
                        )
                    }
                }
            }
        }
        val hasUnseenNotifications = connection.createStatement().use { statement ->
            statement.executeQuery(SELECT_STATE_SQL).use { resultSet ->
                resultSet.next() && resultSet.getBoolean(1)
            }
        }
        return WatchlistSnapshot(entries, notifications, hasUnseenNotifications)
    }

    private fun readSchemaVersion(connection: Connection): Int =
        connection.createStatement().use { statement ->
            statement.executeQuery("PRAGMA user_version").use { resultSet ->
                check(resultSet.next()) { "Watchlist database has no schema version" }
                resultSet.getInt(1)
            }
        }

    private fun setCurrentSchemaVersion(connection: Connection) {
        connection.createStatement().use { statement ->
            statement.executeUpdate("PRAGMA user_version=$CURRENT_SCHEMA_VERSION")
        }
    }

    private fun dataSource(databasePath: Path): SQLiteDataSource =
        SQLiteDataSource(
            SQLiteConfig().apply {
                setEncoding(SQLiteConfig.Encoding.UTF8)
                setLockingMode(SQLiteConfig.LockingMode.NORMAL)
                setJournalMode(SQLiteConfig.JournalMode.WAL)
                setSynchronous(SQLiteConfig.SynchronousMode.NORMAL)
                setBusyTimeout(BUSY_TIMEOUT_MILLIS)
                enforceForeignKeys(true)
                setSharedCache(false)
                enableLoadExtension(false)
                setPageSize(DATABASE_PAGE_SIZE)
            }
        ).also { source -> source.url = "jdbc:sqlite:${databasePath.toAbsolutePath()}" }

    private fun readOnlyDataSource(databasePath: Path): SQLiteDataSource =
        SQLiteDataSource(
            SQLiteConfig().apply {
                setEncoding(SQLiteConfig.Encoding.UTF8)
                setBusyTimeout(BUSY_TIMEOUT_MILLIS)
                enforceForeignKeys(true)
                setSharedCache(false)
                enableLoadExtension(false)
                setReadOnly(true)
            }
        ).also { source -> source.url = "jdbc:sqlite:${databasePath.toAbsolutePath()}" }

    private fun <T> Connection.inTransaction(block: () -> T): T {
        val previousAutoCommit = autoCommit
        autoCommit = false
        return try {
            val result = block()
            commit()
            result
        } catch (exception: Exception) {
            rollback()
            throw exception
        } finally {
            autoCommit = previousAutoCommit
        }
    }

    private inline fun <T> protectedLoad(path: Path, operation: () -> T): T =
        try {
            operation()
        } catch (exception: UnsupportedWatchlistVersionException) {
            throw exception
        } catch (exception: ProtectedWatchlistLoadException) {
            throw exception
        } catch (exception: Exception) {
            throw ProtectedWatchlistLoadException(path, "Could not load watchlist database", exception)
        }

    private const val CURRENT_SCHEMA_VERSION = 1
    private const val BUSY_TIMEOUT_MILLIS = 5_000
    private const val DATABASE_PAGE_SIZE = 4_096

    private const val CREATE_STATE_TABLE_SQL = """
        CREATE TABLE watchlist_state (
            singleton INTEGER PRIMARY KEY CHECK (singleton = 1),
            has_unseen_notifications INTEGER NOT NULL CHECK (has_unseen_notifications IN (0, 1))
        )
    """
    private const val CREATE_ENTRIES_TABLE_SQL = """
        CREATE TABLE watchlist_entries (
            id TEXT PRIMARY KEY,
            position INTEGER NOT NULL UNIQUE,
            name TEXT NOT NULL,
            sender TEXT NOT NULL,
            thema TEXT NOT NULL,
            title TEXT NOT NULL
        )
    """
    private const val CREATE_SEEN_FILM_IDS_TABLE_SQL = """
        CREATE TABLE watchlist_seen_film_ids (
            entry_id TEXT NOT NULL REFERENCES watchlist_entries(id) ON DELETE CASCADE,
            film_id TEXT NOT NULL,
            PRIMARY KEY (entry_id, film_id)
        )
    """
    private const val CREATE_NOTIFICATIONS_TABLE_SQL = """
        CREATE TABLE watchlist_notifications (
            position INTEGER PRIMARY KEY,
            entry_id TEXT NOT NULL,
            entry_name TEXT NOT NULL,
            film_id TEXT NOT NULL,
            sender TEXT NOT NULL,
            thema TEXT NOT NULL,
            title TEXT NOT NULL,
            sende_datum TEXT NOT NULL,
            url_normal_quality TEXT NOT NULL
        )
    """
    private const val INSERT_STATE_SQL =
        "INSERT INTO watchlist_state(singleton, has_unseen_notifications) VALUES (1, ?)"
    private const val INSERT_ENTRY_SQL =
        "INSERT INTO watchlist_entries(id, position, name, sender, thema, title) VALUES (?, ?, ?, ?, ?, ?)"
    private const val INSERT_SEEN_FILM_ID_SQL =
        "INSERT INTO watchlist_seen_film_ids(entry_id, film_id) VALUES (?, ?)"
    private const val INSERT_NOTIFICATION_SQL = """
        INSERT INTO watchlist_notifications(
            position, entry_id, entry_name, film_id, sender, thema, title, sende_datum, url_normal_quality
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    """
    private const val SELECT_STATE_SQL =
        "SELECT has_unseen_notifications FROM watchlist_state WHERE singleton = 1"
    private const val SELECT_ENTRIES_SQL =
        "SELECT id, name, sender, thema, title FROM watchlist_entries ORDER BY position"
    private const val SELECT_SEEN_FILM_IDS_SQL =
        "SELECT entry_id, film_id FROM watchlist_seen_film_ids ORDER BY entry_id, film_id"
    private const val SELECT_NOTIFICATIONS_SQL = """
        SELECT entry_id, entry_name, film_id, sender, thema, title, sende_datum, url_normal_quality
        FROM watchlist_notifications
        ORDER BY position
    """
}
