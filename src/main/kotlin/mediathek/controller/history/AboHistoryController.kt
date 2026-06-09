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

import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import mediathek.config.StandardLocations
import mediathek.gui.messages.history.AboHistoryChangedEvent
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import java.io.IOException
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException
import java.util.concurrent.ConcurrentHashMap

class AboHistoryController(
    private val legacyFilePath: Path = StandardLocations.getSettingsDirectory().resolve(LEGACY_FILENAME),
    databasePath: Path = StandardLocations.getSettingsDirectory().resolve(DATABASE_FILENAME),
    private val databaseDispatcher: CoroutineDispatcher = Dispatchers.IO.limitedParallelism(1)
) {
    private val dataSource = createDataSource(databasePath)
    private val urlCache = ConcurrentHashMap.newKeySet<String>()

    init {
        AboHistoryDatabaseBootstrapper(databasePath, dataSource).bootstrap()
        runOnDatabase {
            migrateLegacyFileIfPresent()
            reloadUrlCache()
        }
    }

    fun getDataList(): List<AboHistoryEntry> = runOnDatabase { loadEntries() }

    fun urlExists(urlFilm: String): Boolean = isSupportedUrl(urlFilm) && urlCache.contains(urlFilm)

    fun removeAll() {
        val changed = runOnDatabase { deleteAllEntries() }
        if (changed) {
            updateCacheAndNotify(
                cacheMutation = urlCache::clear
            )
        }
    }

    fun removeUrl(urlFilm: String) = removeUrls(listOf(urlFilm))

    fun removeUrls(urlsToRemove: Collection<String>): Int {
        val supportedUrls = urlsToRemove.normalizedSupportedUrls()
        if (supportedUrls.isEmpty()) {
            return 0
        }

        val removedCount = runOnDatabase { deleteEntries(supportedUrls) }
        if (removedCount > 0) {
            val removedUrlSet = supportedUrls.toSet()
            updateCacheAndNotify {
                urlCache.removeAll(removedUrlSet)
            }
        }
        return removedCount
    }

    fun add(entry: AboHistoryEntry) = add(listOf(entry))

    fun add(entries: List<AboHistoryEntry>) {
        val candidates = entries.filter(::isPersistable)
        if (candidates.isEmpty()) {
            return
        }

        val changed = runOnDatabase { insertEntries(candidates) }
        if (changed) {
            val addedUrls = candidates.asSequence().map { it.url }.toSet()
            updateCacheAndNotify {
                urlCache.addAll(addedUrls)
            }
        }
    }

    private fun migrateLegacyFileIfPresent() {
        if (Files.notExists(legacyFilePath)) {
            return
        }

        val importedEntries = readLegacyEntries() ?: return
        if (importedEntries.isNotEmpty()) {
            insertEntries(importedEntries)
        }
        archiveLegacyFile()
    }

    private fun readLegacyEntries(): List<AboHistoryEntry>? {
        return try {
            Files.newBufferedReader(legacyFilePath, StandardCharsets.UTF_8).use { reader ->
                generateSequence { reader.readLine() }
                    .map(String::trim)
                    .filter(String::isNotEmpty)
                    .mapNotNull(::parseLegacyLine)
                    .filter(::isPersistable)
                    .toList()
            }
        } catch (e: IOException) {
            logger.error("Could not import legacy abo history from {}", legacyFilePath, e)
            null
        }
    }

    private fun archiveLegacyFile() {
        val archivedPath = legacyFilePath.resolveSibling(legacyFilePath.fileName.toString() + ARCHIVED_SUFFIX)
        try {
            Files.move(legacyFilePath, archivedPath, StandardCopyOption.REPLACE_EXISTING)
        } catch (e: IOException) {
            logger.warn("Could not archive migrated abo history file {}", legacyFilePath, e)
        }
    }

    private fun createDataSource(dbPath: Path): SQLiteDataSource {
        val config = SQLiteConfig().apply {
            setEncoding(SQLiteConfig.Encoding.UTF8)
            setJournalMode(SQLiteConfig.JournalMode.WAL)
            setSynchronous(SQLiteConfig.SynchronousMode.NORMAL)
            setBusyTimeout(5_000)
            enforceForeignKeys(false)
            enableLoadExtension(false)
        }

        return SQLiteDataSource(config).also {
            it.url = "jdbc:sqlite:${dbPath.toAbsolutePath()}"
        }
    }

    private fun openConnection(): Connection =
        dataSource.connection.apply {
            transactionIsolation = Connection.TRANSACTION_SERIALIZABLE
        }

    private fun <T> runOnDatabase(block: suspend () -> T): T = runBlocking {
        withContext(databaseDispatcher) {
            block()
        }
    }

    private fun isPersistable(entry: AboHistoryEntry): Boolean = isSupportedUrl(entry.url)

    private fun isSupportedUrl(url: String): Boolean {
        if (url.isBlank()) {
            return false
        }
        if (url.startsWith("rtmp:", ignoreCase = true)) {
            return false
        }
        if (!(url.startsWith("http://", ignoreCase = true) || url.startsWith("https://", ignoreCase = true))) {
            return false
        }
        if (url.any(Char::isWhitespace)) {
            return false
        }

        return try {
            val uri = URI(url)
            uri.isAbsolute &&
                (uri.scheme.equals("http", ignoreCase = true) || uri.scheme.equals("https", ignoreCase = true)) &&
                !uri.host.isNullOrBlank()
        } catch (_: IllegalArgumentException) {
            false
        }
    }

    private fun loadEntries(): List<AboHistoryEntry> {
        return try {
            openConnection().use { connection ->
                connection.prepareStatement(SELECT_ALL_SQL).use { statement ->
                    statement.executeQuery().use { resultSet ->
                        buildList {
                            while (resultSet.next()) {
                                resultSet.toAboHistoryEntryOrNull()?.let(::add)
                            }
                        }
                    }
                }
            }
        } catch (e: SQLException) {
            logger.error("Could not load abo history entries", e)
            emptyList()
        }
    }

    private fun insertEntries(entries: List<AboHistoryEntry>): Boolean {
        return runLoggedDatabaseOperation(
            errorMessage = "Could not append abo history entries",
            fallback = false
        ) {
            openConnection().use { connection ->
                connection.inTransaction {
                    prepareStatement(INSERT_SQL).use { statement ->
                        entries.forEach { entry -> statement.bindInsertEntry(entry) }
                        countAffectedRows(statement.executeBatch()) > 0
                    }
                }
            }
        }
    }

    private fun deleteAllEntries(): Boolean {
        return runLoggedDatabaseOperation(
            errorMessage = "Could not update abo history",
            fallback = false
        ) {
            openConnection().use { connection ->
                connection.prepareStatement(DELETE_ALL_SQL).use { statement ->
                    statement.executeUpdate() > 0
                }
            }
        }
    }

    private fun deleteEntries(urlsToRemove: List<String>): Int {
        return runLoggedDatabaseOperation(
            errorMessage = "Could not delete abo history entries",
            fallback = 0
        ) {
            openConnection().use { connection ->
                connection.inTransaction {
                    prepareStatement(DELETE_SQL).use { statement ->
                        urlsToRemove.forEach { url -> statement.bindDeleteUrl(url) }
                        countAffectedRows(statement.executeBatch())
                    }
                }
            }
        }
    }

    private fun reloadUrlCache() {
        val loadedUrls = runLoggedDatabaseOperation(
            errorMessage = "Could not reload abo history cache",
            fallback = emptySet()
        ) {
            loadEntries().asSequence()
                .map(AboHistoryEntry::url)
                .toSet()
        }
        urlCache.clear()
        urlCache.addAll(loadedUrls)
    }

    private fun parseLegacyLine(line: String): AboHistoryEntry? {
        if (!line.contains(LEGACY_ENTRY_SEPARATOR)) {
            return null
        }

        val urlSeparatorIndex = line.lastIndexOf(LEGACY_ENTRY_SEPARATOR)
        if (urlSeparatorIndex <= 0) {
            return null
        }

        val metadata = line.substring(0, urlSeparatorIndex)
        val url = line.substring(urlSeparatorIndex + LEGACY_ENTRY_SEPARATOR.length).trim()

        val firstSeparatorIndex = metadata.indexOf(LEGACY_FIELD_SEPARATOR)
        if (firstSeparatorIndex < 0) {
            return null
        }

        val secondSeparatorIndex = metadata.indexOf(
            LEGACY_FIELD_SEPARATOR,
            firstSeparatorIndex + LEGACY_FIELD_SEPARATOR.length
        )
        if (secondSeparatorIndex < 0) {
            return null
        }

        val date = metadata.substring(0, firstSeparatorIndex).trim()
        val theme = metadata.substring(firstSeparatorIndex + LEGACY_FIELD_SEPARATOR.length, secondSeparatorIndex).trim()
        val title = metadata.substring(secondSeparatorIndex + LEGACY_FIELD_SEPARATOR.length).trim()

        return createEntryOrNull(date, theme, title, url)
    }

    private fun <T> Connection.inTransaction(block: Connection.() -> T): T {
        val previousAutoCommit = autoCommit
        autoCommit = false
        return try {
            val result = block()
            commit()
            result
        } catch (e: SQLException) {
            rollback()
            throw e
        } finally {
            autoCommit = previousAutoCommit
        }
    }

    private fun PreparedStatement.bindInsertEntry(entry: AboHistoryEntry) {
        setString(1, entry.formattedDate)
        setString(2, entry.theme)
        setString(3, entry.title)
        setString(4, entry.url)
        addBatch()
    }

    private fun PreparedStatement.bindDeleteUrl(url: String) {
        setString(1, url)
        addBatch()
    }

    private fun countAffectedRows(results: IntArray): Int =
        results.count { it > 0 || it == PreparedStatement.SUCCESS_NO_INFO }

    private fun Collection<String>.normalizedSupportedUrls(): List<String> =
        asSequence()
            .filter(::isSupportedUrl)
            .distinct()
            .toList()

    private fun updateCacheAndNotify(cacheMutation: () -> Unit) {
        cacheMutation()
        publishChangeEvent()
    }

    private fun <T> runLoggedDatabaseOperation(errorMessage: String, fallback: T, operation: () -> T): T {
        return try {
            operation()
        } catch (e: SQLException) {
            logger.error(errorMessage, e)
            fallback
        }
    }

    private fun ResultSet.toAboHistoryEntryOrNull(): AboHistoryEntry? =
        createEntryOrNull(
            getString("datum"),
            getString("thema"),
            getString("titel"),
            getString("url")
        )

    private fun createEntryOrNull(date: String, theme: String, title: String, url: String): AboHistoryEntry? {
        return AboHistoryEntry.parse(date, theme, title, url).also { entry ->
            if (entry == null) {
                logger.warn("Ignoring abo history entry with invalid date \"{}\" and URL \"{}\"", date, url)
            }
        }
    }

    private fun publishChangeEvent() {
        MessageBus.messageBus.publishAsync(AboHistoryChangedEvent())
    }

    private companion object {
        private val logger = LogManager.getLogger()
        private const val LEGACY_FILENAME = "downloadAbos.txt"
        private const val DATABASE_FILENAME = "abo-history.db"
        private const val ARCHIVED_SUFFIX = ".migrated"
        private const val LEGACY_ENTRY_SEPARATOR = "  |###|  "
        private const val LEGACY_FIELD_SEPARATOR = " |#| "
        private const val SELECT_ALL_SQL = """
            SELECT datum, thema, titel, url
            FROM abo_history
            ORDER BY id ASC
        """
        private const val INSERT_SQL = """
            INSERT OR IGNORE INTO abo_history(datum, thema, titel, url)
            VALUES (?, ?, ?, ?)
        """
        private const val DELETE_SQL = """
            DELETE FROM abo_history
            WHERE url = ?
        """
        private const val DELETE_ALL_SQL = "DELETE FROM abo_history"
    }
}
