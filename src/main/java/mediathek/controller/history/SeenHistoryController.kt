package mediathek.controller.history

import com.google.common.collect.Sets
import mediathek.config.Daten
import mediathek.daten.DatenFilm
import mediathek.gui.messages.history.DownloadHistoryChangedEvent
import mediathek.tool.SeenHistoryMigrator
import mediathek.tool.sql.SqlDatabaseConfig
import org.apache.logging.log4j.LogManager
import org.sqlite.SQLiteDataSource
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.sql.*
import kotlin.system.exitProcess

/**
 * Database based seen history controller.
 */
class SeenHistoryController : AutoCloseable {
    private var connection: Connection? = null
    private var insertStatement: PreparedStatement? = null
    private lateinit var dataSource: SQLiteDataSource
    private var deleteStatement: PreparedStatement? = null
    private var seenStatement: PreparedStatement? = null
    private var manualInsertStatement: PreparedStatement? = null

    /**
     * Setup the SQLite data source.
     *
     * @param dbPath   Path to database location
     */
    private fun setupDataSource(dbPath: Path) {
        dataSource = SQLiteDataSource(SqlDatabaseConfig.getConfig())
        dataSource.url = "jdbc:sqlite:" + dbPath.toAbsolutePath().toString()
    }

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
        if (memCachePrepared) {
            return urlCache.contains(film.url)
        } else
            throw IllegalStateException("Memory cache must be prepared to use this function!")
    }

    fun hasBeenSeen(film: DatenFilm): Boolean {
        var result: Boolean

        var rs: ResultSet? = null
        try {
            seenStatement!!.setString(1, film.url)
            rs = seenStatement!!.executeQuery()
            rs.next()
            val total = rs.getInt("total")
            result = total != 0
        } catch (e: SQLException) {
            logger.error("SQL error:", e)
            result = false
        } finally {
            try {
                rs?.close()
            } catch (ignore: SQLException) {
            }
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
        DriverManager.getConnection(dbUrl, SqlDatabaseConfig.getConfig().toProperties()).use { conn ->
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
        statement.executeUpdate("PRAGMA page_size = 4096")
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

    private fun initialize() {
        try {
            val historyDbPath = Paths.get(Daten.getSettingsDirectory_String()).resolve("history.db")
            setupDataSource(historyDbPath)
            if (!Files.exists(historyDbPath)) {
                // create new empty database
                createEmptyDatabase(historyDbPath)
            }

            // open and use database
            connection = dataSource.connection

            performSqliteSetup()

            insertStatement = connection?.prepareStatement(INSERT_SQL)
            deleteStatement = connection?.prepareStatement(DELETE_SQL)
            seenStatement = connection?.prepareStatement(SEEN_SQL)
            manualInsertStatement = connection?.prepareStatement(MANUAL_INSERT_SQL)
        } catch (ex: SQLException) {
            logger.error("ctor", ex)
            exitProcess(99)
        }
    }

    init {
        initialize()
    }
}