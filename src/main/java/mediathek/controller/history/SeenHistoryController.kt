package mediathek.controller.history

import com.google.common.collect.Sets
import mediathek.config.Daten
import mediathek.daten.DatenFilm
import mediathek.gui.messages.history.DownloadHistoryChangedEvent
import mediathek.tool.sql.SqlDatabaseConfig
import org.apache.logging.log4j.LogManager
import org.sqlite.SQLiteDataSource
import java.nio.file.Files
import java.nio.file.Path
import java.sql.*
import kotlin.system.exitProcess

/**
 * Database based seen history controller.
 */
class SeenHistoryController : AutoCloseable {
    private var connection: Connection? = null
    private var insertStatement: PreparedStatement? = null
    private val dataSource: SQLiteDataSource = SqlDatabaseConfig.dataSource
    private var deleteStatement: PreparedStatement? = null
    private var seenStatement: PreparedStatement? = null
    private var manualInsertStatement: PreparedStatement? = null

    /**
     * Remove all entries from the database.
     */
    fun removeAll() {
        try {
            connection!!.createStatement().use { stmt -> stmt.executeUpdate("DELETE FROM seen_history") }
            sendChangeMessage()
        } catch (ex: SQLException) {
            logger.error("removeAll", ex)
        }
    }

    fun markUnseen(film: DatenFilm) {
        try {
            deleteStatement!!.setString(1, film.url)
            deleteStatement!!.executeUpdate()

            Daten.getInstance().listeBookmarkList.updateSeen(false, film)

            sendChangeMessage()
        } catch (ex: SQLException) {
            logger.error("markUnseen", ex)
        }
    }

    fun markUnseen(list: List<DatenFilm>) {
        try {
            for (film in list) {
                deleteStatement!!.setString(1, film.url)
                deleteStatement!!.executeUpdate()
            }

            Daten.getInstance().listeBookmarkList.updateSeen(false, list)

            sendChangeMessage()
        } catch (ex: SQLException) {
            logger.error("markUnseen", ex)
        }
    }

    fun markSeen(film: DatenFilm) {
        if (film.isLivestream) return
        if (hasBeenSeen(film)) return
        try {
            writeToDatabase(film)
            Daten.getInstance().listeBookmarkList.updateSeen(true, film)

            sendChangeMessage()
        } catch (ex: SQLException) {
            logger.error("markSeen single", ex)
        }
    }

    fun markSeen(list: List<DatenFilm>) {
        try {
            for (film in list) {
                //skip livestreams
                if (film.isLivestream) continue
                if (hasBeenSeen(film)) continue
                writeToDatabase(film)
            }

            // Update bookmarks with seen information
            Daten.getInstance().listeBookmarkList.updateSeen(true, list)

            //send one change for all...
            sendChangeMessage()
        } catch (ex: SQLException) {
            logger.error("markSeen", ex)
        }
    }

    fun writeManualEntry(thema: String?, title: String?, url: String?) {
        try {
            manualInsertStatement!!.setString(1, thema)
            manualInsertStatement!!.setString(2, title)
            manualInsertStatement!!.setString(3, url)
            manualInsertStatement!!.executeUpdate()
            sendChangeMessage()
        } catch (ex: SQLException) {
            logger.error("writeManualEntry", ex)
        }
    }

    /**
     * Load all URLs from database and store in memory.
     */
    fun prepareMemoryCache() {
        connection!!.createStatement().use { st ->
            st.executeQuery("SELECT url as url FROM seen_history").use { rs ->
                while (rs.next()) {
                    val url = rs.getString(1)
                    urlCache.add(url)
                }
            }
        }

        logger.trace("cache size: {}", urlCache.size)
        memCachePrepared = true
    }

    fun performMaintenance() {
        logger.trace("Start maintenance")

        try {
            connection!!.createStatement().use {
                it.executeUpdate("DELETE FROM seen_history WHERE thema = 'Livestream'")
                it.executeUpdate("REINDEX seen_history")
                it.executeUpdate("VACUUM")
            }
        }
        catch (e: SQLException) {
            logger.error("Failed to execute maintenance script", e)
        }
        logger.trace("Finished maintenance")
    }

    /**
     * thread-safe store for all database contained URLs.
     */
    private val urlCache = Sets.newConcurrentHashSet<String>()

    /**
     * Indicate whether the mem cache is ready or not
     */
    private var memCachePrepared: Boolean = false

    /**
     * Delete all data in memory cache
     */
    fun emptyMemoryCache() {
        urlCache.clear()
        memCachePrepared = false
    }

    /**
     * Check if film has been seen by using a in-memory cache.
     */
    fun hasBeenSeenFromCache(film: DatenFilm): Boolean {
        if (!memCachePrepared)
            prepareMemoryCache()

        return urlCache.contains(film.url)
    }

    fun hasBeenSeen(film: DatenFilm): Boolean {
        var result: Boolean

        try {
            seenStatement!!.setString(1, film.url)
            seenStatement!!.executeQuery().use {
                it.next()
                val total = it.getInt(1)
                result = total != 0
            }
        } catch (e: SQLException) {
            logger.error("SQL error:", e)
            result = false
        }

        return result
    }

    /**
     * Creates a empty database and all table, indices necessary for use.
     *
     * @throws SQLException Let the caller handle all errors
     */
    @Throws(SQLException::class)
    private fun createEmptyDatabase(dbPath: Path) {
        val dbUrl = "jdbc:sqlite:" + dbPath.toAbsolutePath().toString()
        DriverManager.getConnection(dbUrl, SqlDatabaseConfig.config.toProperties()).use { conn ->
            conn.transactionIsolation = Connection.TRANSACTION_SERIALIZABLE
            conn.createStatement().use { statement ->
                basicSqliteSettings(statement)
                // drop old tables and indices if existent
                statement.executeUpdate(SeenHistoryMigrator.DROP_INDEX_STMT)
                statement.executeUpdate(SeenHistoryMigrator.DROP_TABLE_STMT)
                // create tables and indices
                statement.executeUpdate(SeenHistoryMigrator.CREATE_TABLE_STMT)
                statement.executeUpdate(SeenHistoryMigrator.CREATE_INDEX_STMT)
            }
        }
    }

    private fun basicSqliteSettings(statement: Statement) {
        statement.executeUpdate(SeenHistoryMigrator.PRAGMA_ENCODING_STMT)
        statement.executeUpdate(SeenHistoryMigrator.PRAGMA_PAGE_SIZE)
    }

    /**
     * Write an entry to the database.
     *
     * @param film the film data to be written.
     * @throws SQLException .
     */
    @Throws(SQLException::class)
    private fun writeToDatabase(film: DatenFilm) {
        insertStatement!!.setString(1, film.thema)
        insertStatement!!.setString(2, film.title)
        insertStatement!!.setString(3, film.url)
        // write each entry into database
        insertStatement!!.executeUpdate()
    }

    /**
     * Send notification that the number of entries in the history has been changed.
     */
    private fun sendChangeMessage() {
        Daten.getInstance().messageBus.publishAsync(DownloadHistoryChangedEvent())
    }

    override fun close() {
        urlCache.clear()

        try {
            insertStatement?.close()
            deleteStatement?.close()
            seenStatement?.close()
            manualInsertStatement?.close()
            connection?.close()

            // at this stage we have closed everything and we don´t need the shutdown hook to cleanup
            if (shutdownThread != null)
                Runtime.getRuntime().removeShutdownHook(shutdownThread)
        } catch (ex: SQLException) {
            logger.error("close", ex)
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
        private const val INSERT_SQL = "INSERT INTO seen_history(thema,titel,url) values (?,?,?)"
        private const val DELETE_SQL = "DELETE FROM seen_history WHERE url = ?"
        private const val SEEN_SQL = "SELECT COUNT(url) AS total FROM seen_history WHERE url = ?"
        private const val MANUAL_INSERT_SQL = "INSERT INTO seen_history(thema, titel, url) VALUES (?,?,?)"
    }

    private fun performSqliteSetup() {
        connection!!.createStatement().use { statement ->
            basicSqliteSettings(statement)
            val cpus = Runtime.getRuntime().availableProcessors() / 2
            statement.executeUpdate("PRAGMA threads=$cpus")
        }
    }

    private var shutdownThread: SeenHistoryShutdownHook? = null

    /**
     * Close all database connections if they haven´t been closed already.
     * This allows SQLite to perform additional file cleanup like deletion of WAL and shared-memory files.
     */
    private fun installShutdownHook() {
        shutdownThread = SeenHistoryShutdownHook(connection)
        Runtime.getRuntime().addShutdownHook(shutdownThread)
    }

    init {
        try {
            if (!Files.exists(SqlDatabaseConfig.historyDbPath)) {
                // create new empty database
                createEmptyDatabase(SqlDatabaseConfig.historyDbPath)
            }

            // open and use database
            connection = dataSource.connection

            performSqliteSetup()

            insertStatement = connection?.prepareStatement(INSERT_SQL)
            deleteStatement = connection?.prepareStatement(DELETE_SQL)
            seenStatement = connection?.prepareStatement(SEEN_SQL)
            manualInsertStatement = connection?.prepareStatement(MANUAL_INSERT_SQL)

            installShutdownHook()
        } catch (ex: SQLException) {
            logger.error("ctor", ex)
            exitProcess(99)
        }
    }
}