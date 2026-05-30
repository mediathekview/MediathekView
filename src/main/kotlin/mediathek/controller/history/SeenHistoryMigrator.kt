package mediathek.controller.history

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import mediathek.config.StandardLocations
import mediathek.tool.FileUtils
import mediathek.tool.sql.SqlDatabaseConfig
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import org.apache.logging.log4j.LogManager
import org.sqlite.SQLiteDataSource
import java.io.InputStreamReader
import java.nio.file.Files
import java.nio.file.Path
import java.sql.Connection
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.util.LinkedHashMap

/**
 * Migrates the old history.txt into a sqlite database.
 */
class SeenHistoryMigrator(
    private val historyFilePath: Path,
    private val historyDbPath: Path
) {
    constructor() : this(
        StandardLocations.getSettingsDirectory().resolve("history.txt"),
        SqlDatabaseConfig.historyDbPath
    )

    fun needsMigration(): Boolean = Files.exists(historyFilePath)

    suspend fun migrate() = withContext(Dispatchers.IO) {
        logger.info("Start old history migration.")
        val historyEntries = readOldEntries()
        if (historyEntries.isNotEmpty()) {
            writeEntries(historyEntries)
        }

        FileUtils.moveToTrash(historyFilePath)
        logger.info("Finished old history migration.")
    }

    private fun writeEntries(historyEntries: List<MigratedSeenHistoryEntry>) {
        val dataSource = SQLiteDataSource(SqlDatabaseConfig.config).also {
            it.url = "jdbc:sqlite:${historyDbPath.toAbsolutePath()}"
        }

        dataSource.connection.use { connection ->
            connection.transactionIsolation = Connection.TRANSACTION_SERIALIZABLE
            connection.inTransaction {
                createStatement().use { statement ->
                    statement.queryTimeout = 30
                    statement.executeUpdate(PRAGMA_ENCODING_STMT)
                    statement.executeUpdate(PRAGMA_PAGE_SIZE)
                    statement.executeUpdate(DROP_INDEX_STMT)
                    statement.executeUpdate(DROP_TABLE_STMT)
                    statement.executeUpdate(CREATE_TABLE_STMT)
                    statement.executeUpdate(CREATE_INDEX_STMT)
                }

                prepareStatement(INSERT_STMT).use { insertStatement ->
                    historyEntries.forEach { entry ->
                        insertStatement.setObject(1, entry.date)
                        insertStatement.setString(2, entry.theme)
                        insertStatement.setString(3, entry.title)
                        insertStatement.setString(4, entry.url)
                        insertStatement.executeUpdate()
                    }
                }
            }
        }
    }

    private fun readOldEntries(): List<MigratedSeenHistoryEntry> {
        logger.trace("Reading old entries")
        val newestEntriesByUrl = LinkedHashMap<String, MigratedSeenHistoryEntry>()
        try {
            Files.newInputStream(historyFilePath).use { inputStream ->
                InputStreamReader(inputStream).buffered().useLines { lines ->
                    lines.forEach { entryLine ->
                        val historyEntry = parseHistoryEntry(entryLine) ?: return@forEach
                        newestEntriesByUrl.merge(
                            historyEntry.url,
                            historyEntry
                        ) { existing, replacement ->
                            if (replacement.date.isAfter(existing.date)) replacement else existing
                        }
                    }
                }
            }
        } catch (ex: Exception) {
            logger.error("readOldEntries()", ex)
        }

        logger.trace("historyEntries size: {}", newestEntriesByUrl.size)
        return newestEntriesByUrl.values.toList()
    }

    private fun parseHistoryEntry(entryLine: String): MigratedSeenHistoryEntry? {
        if (!entryLine.contains(LEGACY_ENTRY_SEPARATOR)) {
            return null
        }

        val urlSeparatorIndex = entryLine.lastIndexOf(LEGACY_ENTRY_SEPARATOR)
        if (urlSeparatorIndex <= 0) {
            return null
        }

        val metadata = entryLine.substring(0, urlSeparatorIndex)
        val url = entryLine.substring(urlSeparatorIndex + LEGACY_ENTRY_SEPARATOR.length).trim()
        if (url.startsWith("rtmp:") || url.toHttpUrlOrNull() == null) {
            return null
        }

        val firstFieldSeparatorIndex = metadata.indexOf(LEGACY_FIELD_SEPARATOR)
        if (firstFieldSeparatorIndex < 0) {
            return null
        }

        val secondFieldSeparatorIndex = metadata.indexOf(
            LEGACY_FIELD_SEPARATOR,
            firstFieldSeparatorIndex + LEGACY_FIELD_SEPARATOR.length
        )
        if (secondFieldSeparatorIndex < 0) {
            return null
        }

        val dateText = metadata.substring(0, firstFieldSeparatorIndex).trim()
        if (dateText.isBlank()) {
            return null
        }

        val date = try {
            LocalDate.parse(dateText, LEGACY_DATE_FORMATTER)
        } catch (_: DateTimeParseException) {
            return null
        }

        val theme = metadata.substring(
            firstFieldSeparatorIndex + LEGACY_FIELD_SEPARATOR.length,
            secondFieldSeparatorIndex
        ).trim()
        val title = metadata.substring(secondFieldSeparatorIndex + LEGACY_FIELD_SEPARATOR.length).trim()
        return MigratedSeenHistoryEntry(date, theme, title, url)
    }

    private fun <T> Connection.inTransaction(block: Connection.() -> T): T {
        val previousAutoCommit = autoCommit
        autoCommit = false
        return try {
            val result = block()
            commit()
            result
        } catch (ex: Exception) {
            rollback()
            throw ex
        } finally {
            autoCommit = previousAutoCommit
        }
    }

    private data class MigratedSeenHistoryEntry(
        val date: LocalDate,
        val theme: String,
        val title: String,
        val url: String
    )

    companion object {
        const val PRAGMA_ENCODING_STMT = "PRAGMA encoding='UTF-8'"
        const val PRAGMA_PAGE_SIZE = "PRAGMA page_size = 4096"
        const val CREATE_TABLE_STMT =
            "CREATE TABLE IF NOT EXISTS seen_history (id INTEGER PRIMARY KEY ASC, datum DATE NOT NULL DEFAULT (date('now')), thema TEXT, titel TEXT, url TEXT NOT NULL)"
        const val DROP_TABLE_STMT = "DROP TABLE IF EXISTS seen_history"
        const val INSERT_STMT = "INSERT OR IGNORE INTO seen_history(datum,thema,titel,url) values (?,?,?,?)"
        const val CREATE_INDEX_STMT = "CREATE UNIQUE INDEX IF NOT EXISTS IDX_SEEN_HISTORY_URL ON seen_history(url)"
        const val DROP_INDEX_STMT = "DROP INDEX IF EXISTS IDX_SEEN_HISTORY_URL"

        private val logger = LogManager.getLogger()
        private const val LEGACY_ENTRY_SEPARATOR = "  |###|  "
        private const val LEGACY_FIELD_SEPARATOR = " |#| "
        private val LEGACY_DATE_FORMATTER = DateTimeFormatter.ofPattern("d.MM.yyyy")
    }
}
