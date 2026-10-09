package mediathek.controller.history

import mediathek.tool.sql.SqlDatabaseConfig
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.sqlite.SQLiteDataSource
import java.nio.file.Files
import java.nio.file.Path
import java.sql.DriverManager

class SeenHistoryStoreTest {
    private lateinit var tempDirectory: Path
    private lateinit var databaseFile: Path
    private lateinit var dataSource: SQLiteDataSource
    private var store: SeenHistoryStore? = null

    @BeforeEach
    fun setUp() {
        tempDirectory = Files.createTempDirectory("seen-history-store-test")
        databaseFile = tempDirectory.resolve("history.db")

        dataSource = SQLiteDataSource(SqlDatabaseConfig.config).apply {
            url = "jdbc:sqlite:${databaseFile.toAbsolutePath()}"
        }
        store = SeenHistoryStore(dataSource, databaseFile)
    }

    @AfterEach
    fun tearDown() {
        store?.close()
        Files.deleteIfExists(databaseFile)
        Files.deleteIfExists(tempDirectory.resolve("history.db-shm"))
        Files.deleteIfExists(tempDirectory.resolve("history.db-wal"))
        Files.deleteIfExists(tempDirectory)
    }

    @Test
    fun storesObservedLiveUrlShapesWithoutNormalizingThemAway() {
        val entries = listOf(
            SeenHistoryEntry(
                source = SeenHistorySource.FILM,
                theme = "Clangers",
                title = "Das Leuchten der Kristallbaume",
                url = "http://tvdlzdf-a.akamaihd.net/de/tivi/21/10/211014_das_leuchten_der_kristallbaeume_cln/1/211014_das_leuchten_der_kristallbaeume_cln_2360k_p35v15.mp4"
            ),
            SeenHistoryEntry(
                source = SeenHistorySource.FILM,
                theme = "Sternstunde Religion",
                title = "Wahre Wunder",
                url = "https://srf-vod-amd.akamaized.net/ch/hls/sternstundereligion/2023/04/sternstundereligion_20230406_163022_18331211_v_webcast_h264_,q40,q10,q20,q30,q50,q60,.mp4.csmil/index-f1-v1-a1.m3u8"
            ),
            SeenHistoryEntry(
                source = SeenHistorySource.FILM,
                theme = "24 STUNDEN BAYERN",
                title = "Michael Alexander Rinz, Gast im Studio",
                url = "http://cdn-storage.br.de/MUJIuUOVBwQIbtChb6OHu7ODifWH_-bd/_-iS/_A8g_2vf_U1S/15691981-f81c-4872-b978-4bb9682d3f4f_2.mp3"
            ),
            SeenHistoryEntry(
                source = SeenHistorySource.FILM,
                theme = "Euromaxx",
                title = "Die Geschichte des Berliner Hotels Adlon",
                url = "http://tv-download.dw.com/dwtv_video/flv/emd/emd20170823_adlon_sd_vp6.flv"
            ),
            SeenHistoryEntry(
                source = SeenHistorySource.FILM,
                theme = "Station 19",
                title = "Familienbande (Staffel 4, Folge 3)",
                url = "QualityLevels(1999872)/Manifest(video,format=m3u8"
            )
        )

        store!!.insertSeenEntries(entries + entries.take(2))

        val storedUrls = store!!.loadUrls(SeenHistorySource.FILM)

        assertEquals(entries.size, storedUrls.size)
        entries.forEach { entry ->
            assertTrue(store!!.containsUrl(SeenHistorySource.FILM, entry.url))
            assertTrue(storedUrls.contains(entry.url))
        }
    }

    @Test
    fun loadsOnlyUrlsForRequestedSource() {
        val sharedUrl = "https://example.org/shared.mp4"
        store!!.insertSeenEntries(
            listOf(
                SeenHistoryEntry(
                    source = SeenHistorySource.FILM,
                    theme = "Film Thema",
                    title = "Film Titel",
                    url = sharedUrl
                ),
                SeenHistoryEntry(
                    source = SeenHistorySource.AUDIOTHEK,
                    theme = "Audio Thema",
                    title = "Audio Titel",
                    url = sharedUrl
                ),
                SeenHistoryEntry(
                    source = SeenHistorySource.AUDIOTHEK,
                    theme = "Audio Thema 2",
                    title = "Audio Titel 2",
                    url = "https://example.org/audio-only.mp3"
                )
            )
        )

        assertEquals(setOf(sharedUrl), store!!.loadUrls(SeenHistorySource.FILM))
        assertEquals(
            setOf(sharedUrl, "https://example.org/audio-only.mp3"),
            store!!.loadUrls(SeenHistorySource.AUDIOTHEK)
        )
        assertTrue(store!!.containsUrl(SeenHistorySource.FILM, sharedUrl))
        assertTrue(store!!.containsUrl(SeenHistorySource.AUDIOTHEK, sharedUrl))
    }

    @Test
    fun storesSourceForNewSeenHistoryEntries() {
        val filmEntry = SeenHistoryEntry(
            source = SeenHistorySource.FILM,
            theme = "Film Thema",
            title = "Film Titel",
            url = "https://example.org/film.mp4"
        )
        val audioEntry = SeenHistoryEntry(
            source = SeenHistorySource.AUDIOTHEK,
            theme = "Audio Thema",
            title = "Audio Titel",
            url = "https://example.org/audio.mp3"
        )

        store!!.insertSeenEntries(listOf(filmEntry, audioEntry))

        DriverManager.getConnection("jdbc:sqlite:${databaseFile.toAbsolutePath()}").use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery("SELECT source FROM seen_history WHERE url = 'https://example.org/film.mp4'").use { resultSet ->
                    resultSet.next()
                    assertEquals("FILM", resultSet.getString("source"))
                }
                statement.executeQuery("SELECT source FROM seen_history WHERE url = 'https://example.org/audio.mp3'").use { resultSet ->
                    resultSet.next()
                    assertEquals("AUDIOTHEK", resultSet.getString("source"))
                }
            }
        }
    }

    @Test
    fun newDatabaseIncludesSourceColumn() {
        assertTrue(sourceColumnExists())
    }

    private fun sourceColumnExists(): Boolean {
        DriverManager.getConnection("jdbc:sqlite:${databaseFile.toAbsolutePath()}").use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery("PRAGMA table_info('seen_history')").use { resultSet ->
                    while (resultSet.next()) {
                        if (resultSet.getString("name") == "source") {
                            return true
                        }
                    }
                }
            }
        }
        return false
    }

    @Test
    fun addsSourceColumnToExistingLegacyDatabase() {
        store?.close()
        store = null

        DriverManager.getConnection(
            "jdbc:sqlite:${databaseFile.toAbsolutePath()}",
            SqlDatabaseConfig.config.toProperties()
        ).use { connection ->
            connection.createStatement().use { statement ->
                statement.executeUpdate("DROP INDEX IF EXISTS IDX_SEEN_HISTORY_URL")
                statement.executeUpdate("DROP TABLE IF EXISTS seen_history")
                statement.executeUpdate(
                    "CREATE TABLE seen_history (" +
                        "id INTEGER PRIMARY KEY ASC, " +
                        "datum DATE NOT NULL DEFAULT (date('now')), " +
                        "thema TEXT, " +
                        "titel TEXT, " +
                        "url TEXT NOT NULL" +
                        ")"
                )
                statement.executeUpdate(
                    "INSERT INTO seen_history(datum,thema,titel,url) VALUES " +
                        "('2024-03-01','Thema','Titel','https://example.org/legacy.mp4')"
                )
            }
        }

        store = SeenHistoryStore(dataSource, databaseFile)

        DriverManager.getConnection("jdbc:sqlite:${databaseFile.toAbsolutePath()}").use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery("SELECT source FROM seen_history WHERE url = 'https://example.org/legacy.mp4'").use { resultSet ->
                    resultSet.next()
                    assertEquals("FILM", resultSet.getString("source"))
                }
            }
        }
    }

    @Test
    fun bootstrapsLegacyDatabaseWithDuplicateUrlsWithoutFailing() {
        store?.close()
        store = null

        DriverManager.getConnection(
            "jdbc:sqlite:${databaseFile.toAbsolutePath()}",
            SqlDatabaseConfig.config.toProperties()
        ).use { connection ->
            connection.createStatement().use { statement ->
                statement.executeUpdate("DROP INDEX IF EXISTS IDX_SEEN_HISTORY_URL")
                statement.executeUpdate("DROP TABLE IF EXISTS seen_history")
                statement.executeUpdate(
                    "CREATE TABLE seen_history (" +
                        "id INTEGER PRIMARY KEY ASC, " +
                        "datum DATE NOT NULL DEFAULT (date('now')), " +
                        "thema TEXT, " +
                        "titel TEXT, " +
                        "url TEXT NOT NULL" +
                        ")"
                )
                statement.executeUpdate(
                    "INSERT INTO seen_history(datum,thema,titel,url) VALUES " +
                        "('2024-03-01','Thema alt','Titel alt','https://example.org/duplicate.mp4')," +
                        "('2024-03-05','Thema neu','Titel neu','https://example.org/duplicate.mp4')," +
                        "('2024-02-01','Thema solo','Titel solo','https://example.org/unique.m3u8')"
                )
            }
        }

        store = SeenHistoryStore(dataSource, databaseFile)

        val storedUrls = store!!.loadUrls(SeenHistorySource.FILM)
        assertEquals(2, storedUrls.size)
        assertTrue(storedUrls.contains("https://example.org/duplicate.mp4"))
        assertTrue(storedUrls.contains("https://example.org/unique.m3u8"))

        DriverManager.getConnection("jdbc:sqlite:${databaseFile.toAbsolutePath()}").use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery(
                    "SELECT COUNT(*) FROM seen_history WHERE url = 'https://example.org/duplicate.mp4'"
                ).use { resultSet ->
                    resultSet.next()
                    assertEquals(1, resultSet.getInt(1))
                }

                statement.executeQuery(
                    "SELECT datum, source, thema, titel FROM seen_history WHERE url = 'https://example.org/duplicate.mp4'"
                ).use { resultSet ->
                    resultSet.next()
                    assertEquals("2024-03-05", resultSet.getString(1))
                    assertEquals("FILM", resultSet.getString(2))
                    assertEquals("Thema neu", resultSet.getString(3))
                    assertEquals("Titel neu", resultSet.getString(4))
                }

                statement.executeQuery(
                    "PRAGMA index_list('seen_history')"
                ).use { resultSet ->
                    var foundUniqueUrlIndex = false
                    while (resultSet.next()) {
                        if (resultSet.getString("name") == "IDX_SEEN_HISTORY_SOURCE_URL") {
                            foundUniqueUrlIndex = true
                            assertEquals(1, resultSet.getInt("unique"))
                        }
                    }
                    assertTrue(foundUniqueUrlIndex)
                }
            }
        }
    }

    @Test
    fun upgradesLegacyNonUniqueUrlIndexToUniqueIndex() {
        store?.close()
        store = null

        DriverManager.getConnection(
            "jdbc:sqlite:${databaseFile.toAbsolutePath()}",
            SqlDatabaseConfig.config.toProperties()
        ).use { connection ->
            connection.createStatement().use { statement ->
                statement.executeUpdate("DROP INDEX IF EXISTS IDX_SEEN_HISTORY_URL")
                statement.executeUpdate("DROP TABLE IF EXISTS seen_history")
                statement.executeUpdate(
                    "CREATE TABLE seen_history (" +
                        "id INTEGER PRIMARY KEY ASC, " +
                        "datum DATE NOT NULL DEFAULT (date('now')), " +
                        "thema TEXT, " +
                        "titel TEXT, " +
                        "url TEXT NOT NULL" +
                        ")"
                )
                statement.executeUpdate("CREATE INDEX IDX_SEEN_HISTORY_URL ON seen_history(url)")
                statement.executeUpdate(
                    "INSERT INTO seen_history(datum,thema,titel,url) VALUES " +
                        "('2024-03-01','Thema alt','Titel alt','https://example.org/duplicate.mp4')," +
                        "('2024-03-05','Thema neu','Titel neu','https://example.org/duplicate.mp4')," +
                        "('2024-02-01','Thema solo','Titel solo','https://example.org/unique.m3u8')"
                )
            }
        }

        store = SeenHistoryStore(dataSource, databaseFile)

        DriverManager.getConnection("jdbc:sqlite:${databaseFile.toAbsolutePath()}").use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery(
                    "SELECT COUNT(*) FROM seen_history WHERE url = 'https://example.org/duplicate.mp4'"
                ).use { resultSet ->
                    resultSet.next()
                    assertEquals(1, resultSet.getInt(1))
                }

                statement.executeQuery("PRAGMA index_list('seen_history')").use { resultSet ->
                    var foundUniqueUrlIndex = false
                    while (resultSet.next()) {
                        if (resultSet.getString("name") == "IDX_SEEN_HISTORY_SOURCE_URL") {
                            foundUniqueUrlIndex = true
                            assertEquals(1, resultSet.getInt("unique"))
                        }
                    }
                    assertTrue(foundUniqueUrlIndex)
                }
            }
        }
    }

    @Test
    fun duplicateCleanupPreservesSourceFromNewestEntry() {
        store?.close()
        store = null

        DriverManager.getConnection(
            "jdbc:sqlite:${databaseFile.toAbsolutePath()}",
            SqlDatabaseConfig.config.toProperties()
        ).use { connection ->
            connection.createStatement().use { statement ->
                statement.executeUpdate("DROP INDEX IF EXISTS IDX_SEEN_HISTORY_URL")
                statement.executeUpdate("DROP TABLE IF EXISTS seen_history")
                statement.executeUpdate(
                    "CREATE TABLE seen_history (" +
                        "id INTEGER PRIMARY KEY ASC, " +
                        "datum DATE NOT NULL DEFAULT (date('now')), " +
                        "source TEXT NOT NULL DEFAULT 'FILM', " +
                        "thema TEXT, " +
                        "titel TEXT, " +
                        "url TEXT NOT NULL" +
                        ")"
                )
                statement.executeUpdate(
                    "INSERT INTO seen_history(datum,source,thema,titel,url) VALUES " +
                        "('2024-03-01','FILM','Thema alt','Titel alt','https://example.org/duplicate.mp4')," +
                        "('2024-03-05','AUDIOTHEK','Thema neu','Titel neu','https://example.org/duplicate.mp4')"
                )
            }
        }

        store = SeenHistoryStore(dataSource, databaseFile)

        DriverManager.getConnection("jdbc:sqlite:${databaseFile.toAbsolutePath()}").use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery(
                    "SELECT source, datum, thema, titel FROM seen_history WHERE url = 'https://example.org/duplicate.mp4' ORDER BY source"
                ).use { resultSet ->
                    resultSet.next()
                    assertEquals("AUDIOTHEK", resultSet.getString("source"))
                    assertEquals("2024-03-05", resultSet.getString("datum"))
                    assertEquals("Thema neu", resultSet.getString("thema"))
                    assertEquals("Titel neu", resultSet.getString("titel"))
                    resultSet.next()
                    assertEquals("FILM", resultSet.getString("source"))
                    assertEquals("2024-03-01", resultSet.getString("datum"))
                    assertEquals("Thema alt", resultSet.getString("thema"))
                    assertEquals("Titel alt", resultSet.getString("titel"))
                }
            }
        }
    }

    @Test
    fun sourceAndUrlUniquelyIdentifySeenEntry() {
        val filmEntry = SeenHistoryEntry(
            source = SeenHistorySource.FILM,
            theme = "Film Thema",
            title = "Film Titel",
            url = "https://example.org/shared.mp4"
        )
        val audioEntry = SeenHistoryEntry(
            source = SeenHistorySource.AUDIOTHEK,
            theme = "Audio Thema",
            title = "Audio Titel",
            url = "https://example.org/shared.mp4"
        )

        store!!.insertSeenEntry(filmEntry)
        store!!.insertSeenEntry(audioEntry)

        DriverManager.getConnection("jdbc:sqlite:${databaseFile.toAbsolutePath()}").use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery("SELECT COUNT(*) FROM seen_history WHERE url = 'https://example.org/shared.mp4'").use { resultSet ->
                    resultSet.next()
                    assertEquals(2, resultSet.getInt(1))
                }
            }
        }
    }
}
