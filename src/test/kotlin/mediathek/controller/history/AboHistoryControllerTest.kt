package mediathek.controller.history

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.sql.DriverManager

internal class AboHistoryControllerTest {
    private lateinit var tempDirectory: Path
    private lateinit var legacyFile: Path
    private lateinit var databaseFile: Path

    @BeforeEach
    fun setUp() {
        tempDirectory = Files.createTempDirectory("abo-history-controller-test")
        legacyFile = tempDirectory.resolve("downloadAbos.txt")
        databaseFile = tempDirectory.resolve("abo-history.db")
    }

    @AfterEach
    fun tearDown() {
        Files.deleteIfExists(tempDirectory.resolve("downloadAbos.txt.migrated"))
        Files.deleteIfExists(legacyFile)
        Files.deleteIfExists(databaseFile)
        Files.deleteIfExists(tempDirectory.resolve("abo-history.db-shm"))
        Files.deleteIfExists(tempDirectory.resolve("abo-history.db-wal"))
        Files.deleteIfExists(tempDirectory)
    }

    @Test
    fun migratesLegacyTextFileOnStartup() {
        Files.writeString(
            legacyFile,
            """
                02.11.2020 |#| Thema A |#| Titel A  |###|  https://example.org/a.mp4
                02.11.2020 |#| Thema B |#| Titel B  |###|  https://example.org/b.mp4
                02.11.2020 |#| Thema B |#| Titel B  |###|  https://example.org/b.mp4
                invalid line
                not-a-date |#| Thema C |#| Titel C  |###|  https://example.org/c.mp4
                02.11.2020 |#| Thema C |#| Titel C  |###|  rtmp://example.org/c
            """.trimIndent(),
            StandardCharsets.UTF_8,
        )

        val controller = AboHistoryController(legacyFile, databaseFile)

        assertTrue(Files.exists(databaseFile))
        assertFalse(Files.exists(legacyFile))
        assertTrue(Files.exists(tempDirectory.resolve("downloadAbos.txt.migrated")))

        assertEquals(2, controller.getDataList().size)
        assertTrue(controller.urlExists("https://example.org/a.mp4"))
        assertTrue(controller.urlExists("https://example.org/b.mp4"))
        assertFalse(controller.urlExists("https://example.org/c.mp4"))
    }

    @Test
    fun ignoresDatabaseRowsWithInvalidDates() {
        AboHistoryController(legacyFile, databaseFile)

        DriverManager.getConnection("jdbc:sqlite:${databaseFile.toAbsolutePath()}").use { connection ->
            connection.createStatement().use { statement ->
                statement.executeUpdate(
                    """
                        INSERT INTO abo_history(datum, thema, titel, url) VALUES
                        ('02.11.2020', 'Thema A', 'Titel A', 'https://example.org/a.mp4'),
                        ('not-a-date', 'Thema B', 'Titel B', 'https://example.org/b.mp4')
                    """.trimIndent()
                )
            }
        }

        val controller = AboHistoryController(legacyFile, databaseFile)
        val entries = controller.getDataList()

        assertEquals(1, entries.size)
        assertEquals("https://example.org/a.mp4", entries.single().url)
        assertFalse(controller.urlExists("https://example.org/b.mp4"))
    }

    @Test
    fun persistsEntriesInSqliteAcrossControllerInstances() {
        val controller = AboHistoryController(legacyFile, databaseFile)
        controller.add(historyEntry("Thema A", "Titel A", "https://example.org/a.mp4"))
        controller.add(historyEntry("Thema A", "Titel A", "https://example.org/a.mp4"))
        controller.add(historyEntry("Thema B", "Titel B", "https://example.org/b.mp4"))

        assertEquals(2, controller.getDataList().size)
        assertTrue(controller.urlExists("https://example.org/a.mp4"))

        val reloadedController = AboHistoryController(legacyFile, databaseFile)
        assertEquals(2, reloadedController.getDataList().size)

        reloadedController.removeUrl("https://example.org/a.mp4")
        assertFalse(reloadedController.urlExists("https://example.org/a.mp4"))
        assertEquals(1, reloadedController.getDataList().size)

        reloadedController.removeAll()
        assertTrue(reloadedController.getDataList().isEmpty())
    }

    @Test
    fun removesMultipleEntriesAtOnce() {
        val controller = AboHistoryController(legacyFile, databaseFile)
        controller.add(historyEntry("Thema A", "Titel A", "https://example.org/a.mp4"))
        controller.add(historyEntry("Thema B", "Titel B", "https://example.org/b.mp4"))
        controller.add(historyEntry("Thema C", "Titel C", "https://example.org/c.mp4"))

        val removedCount = controller.removeUrls(
            listOf(
                "https://example.org/a.mp4",
                "https://example.org/c.mp4",
                "https://example.org/a.mp4",
            ),
        )

        assertEquals(2, removedCount)
        assertFalse(controller.urlExists("https://example.org/a.mp4"))
        assertTrue(controller.urlExists("https://example.org/b.mp4"))
        assertFalse(controller.urlExists("https://example.org/c.mp4"))
        assertEquals(1, controller.getDataList().size)
    }

    @Test
    fun bootstrapsSchemaVersionForFutureMigrations() {
        AboHistoryController(legacyFile, databaseFile)

        DriverManager.getConnection("jdbc:sqlite:${databaseFile.toAbsolutePath()}").use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery("PRAGMA user_version").use { resultSet ->
                    assertTrue(resultSet.next())
                    assertEquals(AboHistoryDatabaseBootstrapper.CURRENT_SCHEMA_VERSION, resultSet.getInt(1))
                }
            }
        }
    }

    private fun historyEntry(theme: String, title: String, url: String): AboHistoryEntry =
        requireNotNull(AboHistoryEntry.parse("02.11.2020", theme, title, url))
}
