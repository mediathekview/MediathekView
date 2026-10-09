package mediathek.audiothek.repository

import kotlinx.coroutines.runBlocking
import mediathek.tool.sql.SqlDatabaseConfig
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path

class AudioRepositoryTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun `loadAudiothek returns sqlite export entries`() = runBlocking {
        val exportDatabase = tempDir.resolve("mv-audiothek.db")
        createNormalizedExportDatabase(exportDatabase)

        val repository = AudioRepository(
            sqliteExportSource = object : SqliteExportAudioSource() {
                override fun updateLocalDatabase(): SqliteExportDownloadStatus = SqliteExportDownloadStatus.NOT_MODIFIED
                override fun exportDatabasePath(): Path = exportDatabase
            },
        )

        val result = repository.loadAudiothek()

        assertEquals(SqliteExportDownloadStatus.NOT_MODIFIED, result.downloadStatus)
        assertEquals(2, result.dataset.entries.size)
        assertEquals("Remote Titel", result.dataset.entries[0].title)
        assertEquals("Export Kategorie", result.dataset.entries[0].genre)
        assertEquals("Export Programmset", result.dataset.entries[0].theme)
        assertEquals("Export Extra", result.dataset.entries[1].title)
        assertEquals("Export Kategorie", result.dataset.entries[1].genre)
        assertEquals("Export Programmset", result.dataset.entries[1].theme)
        assertEquals("Export Sender", result.dataset.entries[1].channel)
        assertEquals(3, result.dataset.entries[1].sizeMb)
        assertNotNull(result.dataset.entries[1].audioUrl)
        assertNotNull(result.dataset.entries[1].websiteUrl)
    }

    @Test
    fun `loadAudiothek reports updated when sqlite export updated`() = runBlocking {
        val exportDatabase = tempDir.resolve("mv-audiothek.db")
        createNormalizedExportDatabase(exportDatabase)

        val repository = AudioRepository(
            sqliteExportSource = object : SqliteExportAudioSource() {
                override fun updateLocalDatabase(): SqliteExportDownloadStatus = SqliteExportDownloadStatus.DOWNLOADED
                override fun exportDatabasePath(): Path = exportDatabase
            },
        )

        val result = repository.loadAudiothek()

        assertEquals(SqliteExportDownloadStatus.DOWNLOADED, result.downloadStatus)
        assertTrue(result.hasUpdatedSource())
        assertNull(result.reloadMessage())
    }

    @Test
    fun `loadAudiothek keeps existing sqlite export when update fails`() = runBlocking {
        val exportDatabase = tempDir.resolve("mv-audiothek.db")
        createNormalizedExportDatabase(exportDatabase)

        val repository = AudioRepository(
            sqliteExportSource = object : SqliteExportAudioSource() {
                override fun updateLocalDatabase(): SqliteExportDownloadStatus = SqliteExportDownloadStatus.FAILED
                override fun exportDatabasePath(): Path = exportDatabase
            },
        )

        val result = repository.loadAudiothek()

        assertEquals(SqliteExportDownloadStatus.FAILED, result.downloadStatus)
        assertEquals(2, result.dataset.entries.size)
        assertFalse(result.hasUpdatedSource())
        assertEquals("Es konnte keine neue Datei geladen werden.\nDie vorhandene wird weiter verwendet.", result.reloadMessage())
    }

    private fun createNormalizedExportDatabase(path: Path) {
        SqlDatabaseConfig.createDataSource(path).connection.use { connection ->
            connection.autoCommit = false
            connection.createStatement().use { statement ->
                statement.execute("CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL)")
                statement.execute("CREATE TABLE categories (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL)")
                statement.execute(
                    """
                    CREATE TABLE program_sets (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        category_id INTEGER NOT NULL,
                        title TEXT NOT NULL
                    )
                    """.trimIndent()
                )
                statement.execute(
                    """
                    CREATE TABLE items (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        program_set_id INTEGER NOT NULL,
                        title TEXT NOT NULL,
                        page_url TEXT,
                        sender TEXT,
                        publish_date TEXT,
                        description TEXT,
                        duration_seconds INTEGER,
                        preferred_audio_asset TEXT,
                        preferred_audio_asset_file_size_bytes INTEGER
                    )
                    """.trimIndent()
                )
            }

            connection.prepareStatement("INSERT INTO metadata(key, value) VALUES(?, ?)").use { insert ->
                insert.setString(1, "created_at_utc")
                insert.setString(2, "2026-04-22T09:00:00Z")
                insert.executeUpdate()
            }
            connection.prepareStatement("INSERT INTO categories(title) VALUES(?)").use { insert ->
                insert.setString(1, "Export Kategorie")
                insert.executeUpdate()
            }
            connection.prepareStatement("INSERT INTO program_sets(category_id, title) VALUES(?, ?)").use { insert ->
                insert.setLong(1, 1)
                insert.setString(2, "Export Programmset")
                insert.executeUpdate()
            }
            connection.prepareStatement(
                """
                INSERT INTO items(
                    program_set_id,
                    title,
                    page_url,
                    sender,
                    publish_date,
                    description,
                    duration_seconds,
                    preferred_audio_asset,
                    preferred_audio_asset_file_size_bytes
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """.trimIndent()
            ).use { insert ->
                insert.setLong(1, 1)
                insert.setString(2, "Remote Titel")
                insert.setString(3, "https://example.invalid/remote")
                insert.setString(4, "Export Sender")
                insert.setString(5, "2026-04-22")
                insert.setString(6, "Should be ignored as duplicate")
                insert.setInt(7, 120)
                insert.setString(8, "https://example.invalid/remote.mp3")
                insert.setLong(9, 2 * 1024 * 1024L)
                insert.executeUpdate()

                insert.setLong(1, 1)
                insert.setString(2, "Export Extra")
                insert.setString(3, "https://example.invalid/export-extra")
                insert.setString(4, "Export Sender")
                insert.setString(5, "2026-04-21")
                insert.setString(6, "Export Beschreibung")
                insert.setInt(7, 181)
                insert.setString(8, "https://example.invalid/export-extra.mp3")
                insert.setLong(9, 3 * 1024 * 1024L)
                insert.executeUpdate()
            }
            connection.commit()
        }
    }

}
