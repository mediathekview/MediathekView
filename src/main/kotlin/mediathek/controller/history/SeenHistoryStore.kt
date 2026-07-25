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

package mediathek.controller.history

import mediathek.tool.sql.SqlDatabaseConfig
import org.apache.logging.log4j.LogManager
import org.sqlite.SQLiteDataSource
import java.nio.file.Files
import java.nio.file.Path
import java.sql.*

internal class SeenHistoryStore(
    dataSource: SQLiteDataSource,
    dbPath: Path
) : AutoCloseable {
    private val connection: Connection
    private val insertStatement: PreparedStatement
    private val deleteStatement: PreparedStatement
    private val seenStatement: PreparedStatement
    private val loadUrlsStatement: PreparedStatement

    init {
        if (shouldInitializeDatabase(dbPath)) {
            createEmptyDatabase(dbPath)
        }

        connection = dataSource.connection
        performSqliteSetup()
        ensureSourceColumn()
        ensureUniqueUrlIndex()

        insertStatement = connection.prepareStatement(INSERT_SQL)
        deleteStatement = connection.prepareStatement(DELETE_SQL)
        seenStatement = connection.prepareStatement(SEEN_SQL)
        loadUrlsStatement = connection.prepareStatement(SELECT_URLS_SQL)
    }

    fun removeAllEntries() {
        connection.createStatement().use { statement ->
            statement.executeUpdate(DELETE_ALL_SQL)
        }
    }

    fun removeSeenUrl(source: SeenHistorySource, url: String) {
        deleteStatement.setString(1, source.name)
        deleteStatement.setString(2, url)
        deleteStatement.executeUpdate()
    }

    fun removeSeenUrls(source: SeenHistorySource, urls: Collection<String>) {
        connection.inTransaction {
            urls.forEach { url ->
                deleteStatement.setString(1, source.name)
                deleteStatement.setString(2, url)
                deleteStatement.addBatch()
            }
            deleteStatement.executeBatch()
            deleteStatement.clearBatch()
        }
    }

    fun insertSeenEntry(entry: SeenHistoryEntry): Boolean {
        insertStatement.setString(1, entry.source.name)
        insertStatement.setString(2, entry.theme)
        insertStatement.setString(3, entry.title)
        insertStatement.setString(4, entry.url)
        return insertStatement.executeUpdate() > 0
    }

    fun insertSeenEntries(entries: List<SeenHistoryEntry>) {
        connection.inTransaction {
            entries.forEach { entry ->
                insertStatement.setString(1, entry.source.name)
                insertStatement.setString(2, entry.theme)
                insertStatement.setString(3, entry.title)
                insertStatement.setString(4, entry.url)
                insertStatement.addBatch()
            }
            insertStatement.executeBatch()
            insertStatement.clearBatch()
        }
    }

    fun loadUrls(source: SeenHistorySource): Set<String> {
        loadUrlsStatement.setString(1, source.name)
        loadUrlsStatement.executeQuery().use { resultSet ->
            return buildSet {
                while (resultSet.next()) {
                    add(resultSet.getString(1))
                }
            }
        }
    }

    fun containsUrl(source: SeenHistorySource, url: String): Boolean {
        if (url.isBlank()) {
            return false
        }

        seenStatement.setString(1, source.name)
        seenStatement.setString(2, url)
        seenStatement.executeQuery().use { resultSet ->
            resultSet.next()
            return resultSet.getInt(1) != 0
        }
    }

    fun performMaintenance(shouldCompact: Boolean) {
        connection.createStatement().use { statement ->
            statement.executeUpdate(DELETE_LIVESTREAMS_SQL)
        }
        if (shouldCompact) {
            compactDatabase()
        }
    }

    fun compactDatabase() {
        logger.info("Compacting database")
        connection.createStatement().use { statement ->
            statement.executeUpdate(REINDEX_SQL)
            statement.executeUpdate(VACUUM_SQL)
        }
    }

    @Throws(SQLException::class)
    fun removeDuplicates() {
        connection.inTransaction {
            connection.createStatement().use { statement ->
                statement.executeUpdate(CREATE_TEMP_HISTORY_SQL)
                statement.executeUpdate(DELETE_ALL_SQL)
                statement.executeUpdate(RESTORE_UNIQUE_HISTORY_SQL)
                statement.executeUpdate(DROP_TEMP_HISTORY_SQL)
            }
        }
    }

    @Throws(SQLException::class)
    fun countDuplicateEntries(): Int {
        connection.createStatement().use { statement ->
            statement.executeQuery(COUNT_DUPLICATES_SQL).use { resultSet ->
                resultSet.next()
                return resultSet.getInt(1)
            }
        }
    }

    @Throws(SQLException::class)
    fun normalizeRecoveredDatabase(): Int {
        val duplicateCount = countDuplicateEntries()
        if (duplicateCount > 0) {
            removeDuplicates()
        }
        ensureUniqueUrlIndex()
        return duplicateCount
    }

    override fun close() {
        insertStatement.close()
        deleteStatement.close()
        seenStatement.close()
        loadUrlsStatement.close()
        connection.close()
    }

    @Throws(SQLException::class)
    private fun createEmptyDatabase(dbPath: Path) {
        val dbUrl = "jdbc:sqlite:${dbPath.toAbsolutePath()}"
        DriverManager.getConnection(dbUrl, SqlDatabaseConfig.config.toProperties()).use { connection ->
            connection.transactionIsolation = Connection.TRANSACTION_SERIALIZABLE
            connection.createStatement().use { statement ->
                basicSqliteSettings(statement)
                statement.executeUpdate(SeenHistoryMigrator.DROP_INDEX_STMT)
                statement.executeUpdate(SeenHistoryMigrator.DROP_LEGACY_URL_INDEX_STMT)
                statement.executeUpdate(SeenHistoryMigrator.DROP_TABLE_STMT)
                statement.executeUpdate(SeenHistoryMigrator.CREATE_TABLE_STMT)
                statement.executeUpdate(SeenHistoryMigrator.CREATE_INDEX_STMT)
            }
        }
    }

    private fun shouldInitializeDatabase(dbPath: Path): Boolean {
        if (!Files.exists(dbPath)) {
            return true
        }

        return try {
            Files.size(dbPath) == 0L
        } catch (_: Exception) {
            false
        }
    }

    private fun basicSqliteSettings(statement: Statement) {
        statement.executeUpdate(SeenHistoryMigrator.PRAGMA_ENCODING_STMT)
        statement.executeUpdate(SeenHistoryMigrator.PRAGMA_PAGE_SIZE)
    }

    private fun performSqliteSetup() {
        connection.createStatement().use { statement ->
            basicSqliteSettings(statement)
            val cpus = Runtime.getRuntime().availableProcessors() / 2
            statement.executeUpdate("PRAGMA threads=$cpus")
        }
    }

    @Throws(SQLException::class)
    private fun ensureSourceColumn() {
        if (hasSourceColumn()) {
            return
        }

        connection.createStatement().use { statement ->
            statement.executeUpdate(ADD_SOURCE_COLUMN_SQL)
        }
    }

    @Throws(SQLException::class)
    private fun hasSourceColumn(): Boolean {
        connection.createStatement().use { statement ->
            statement.executeQuery("PRAGMA table_info('seen_history')").use { resultSet ->
                while (resultSet.next()) {
                    if (resultSet.getString("name") == SOURCE_COLUMN_NAME) {
                        return true
                    }
                }
            }
        }
        return false
    }

    @Throws(SQLException::class)
    private fun ensureUniqueUrlIndex() {
        if (hasRequiredUniqueUrlIndex()) {
            return
        }

        connection.createStatement().use { statement ->
            statement.executeUpdate(SeenHistoryMigrator.DROP_INDEX_STMT)
            statement.executeUpdate(SeenHistoryMigrator.DROP_LEGACY_URL_INDEX_STMT)
        }

        try {
            connection.createStatement().use { statement ->
                statement.executeUpdate(SeenHistoryMigrator.CREATE_INDEX_STMT)
            }
        } catch (_: SQLException) {
            logger.info("Removing duplicate seen history entries before creating unique source URL index")
            removeDuplicates()
            connection.createStatement().use { statement ->
                statement.executeUpdate(SeenHistoryMigrator.DROP_INDEX_STMT)
                statement.executeUpdate(SeenHistoryMigrator.DROP_LEGACY_URL_INDEX_STMT)
                statement.executeUpdate(SeenHistoryMigrator.CREATE_INDEX_STMT)
            }
        }
    }

    @Throws(SQLException::class)
    private fun hasRequiredUniqueUrlIndex(): Boolean {
        connection.createStatement().use { statement ->
            statement.executeQuery("PRAGMA index_list('seen_history')").use { resultSet ->
                while (resultSet.next()) {
                    if (resultSet.getString("name") == UNIQUE_URL_INDEX_NAME) {
                        return resultSet.getInt("unique") == 1
                    }
                }
            }
        }

        return false
    }

    private fun <T> Connection.inTransaction(block: () -> T): T {
        val previousAutoCommit = autoCommit
        autoCommit = false
        return try {
            val result = block()
            commit()
            result
        } catch (ex: SQLException) {
            rollback()
            throw ex
        } finally {
            autoCommit = previousAutoCommit
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
        private const val INSERT_SQL = "INSERT OR IGNORE INTO seen_history(source,thema,titel,url) values (?,?,?,?)"
        private const val DELETE_SQL = "DELETE FROM seen_history WHERE source = ? AND url = ?"
        private const val SELECT_URLS_SQL = "SELECT url FROM seen_history WHERE source = ?"
        private const val SEEN_SQL = "SELECT COUNT(url) AS total FROM seen_history WHERE source = ? AND url = ?"
        private const val DELETE_ALL_SQL = "DELETE FROM seen_history"
        private const val DELETE_LIVESTREAMS_SQL = "DELETE FROM seen_history WHERE thema = 'Livestream'"
        private const val REINDEX_SQL = "REINDEX seen_history"
        private const val VACUUM_SQL = "VACUUM"
        private const val SOURCE_COLUMN_NAME = "source"
        private const val ADD_SOURCE_COLUMN_SQL =
            "ALTER TABLE seen_history ADD COLUMN source TEXT NOT NULL DEFAULT 'FILM'"
        private const val UNIQUE_URL_INDEX_NAME = "IDX_SEEN_HISTORY_SOURCE_URL"
        private const val CREATE_TEMP_HISTORY_SQL = """
            CREATE TABLE temp_history AS
            SELECT
                datum,
                source,
                thema,
                titel,
                url
            FROM (
                SELECT
                    datum,
                    source,
                    thema,
                    titel,
                    url,
                    ROW_NUMBER() OVER (PARTITION BY source, url ORDER BY datum DESC) as rn
                FROM
                    seen_history
            ) AS ranked_seen_history
            WHERE
                rn = 1
            ORDER BY
                datum
        """
        private const val RESTORE_UNIQUE_HISTORY_SQL = """
            INSERT INTO seen_history(datum,source,thema,titel,url)
            SELECT datum,source,thema,titel,url FROM temp_history
        """
        private const val DROP_TEMP_HISTORY_SQL = "DROP TABLE temp_history"
        private const val COUNT_DUPLICATES_SQL = """
            SELECT COUNT(*) FROM (
                SELECT source, url
                FROM seen_history
                GROUP BY source, url
                HAVING COUNT(*) > 1
            )
        """
    }
}
