package mediathek.audiothek.repository

import mediathek.config.Konstanten
import mediathek.tool.sql.SqlDatabaseConfig
import okhttp3.*
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.ResponseBody.Companion.toResponseBody
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.tukaani.xz.LZMA2Options
import org.tukaani.xz.XZOutputStream
import java.io.ByteArrayOutputStream
import java.nio.file.Files
import java.nio.file.Path
import java.sql.Statement
import java.util.*
import java.util.concurrent.atomic.AtomicReference

class SqliteExportAudioSourceTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun `updateLocalDatabase performs a fresh download`() {
        val capturedEtag = AtomicReference<String?>()
        val responseBody = createCompressedDatabaseArchive(
            databasePath = tempDir.resolve("source-v1.db"),
            categoryTitle = "Wissen",
            programSetTitle = "Radiowissen",
            itemTitle = "Radiowissen Folge 1",
            createdAtUtc = "2026-04-22T10:00:00Z",
        )
        val client = OkHttpClient.Builder()
            .addInterceptor(
                responseInterceptor { request ->
                    capturedEtag.set(request.header("If-None-Match"))
                    responseSpec(
                        request = request,
                        code = 200,
                        body = responseBody,
                        eTag = "\"v1\"",
                    )
                }
            )
            .build()

        val source = testSource(client)

        assertEquals(SqliteExportDownloadStatus.DOWNLOADED, source.updateLocalDatabase())
        assertNull(capturedEtag.get())
        assertTrue(Files.exists(tempDir.resolve("mv-audiothek.db")))
        assertFalse(Files.exists(tempDir.resolve("mv-audiothek.db.xz")))
        assertEquals("\"v1\"", readMetadataProperties()["etag"])

        val dataset = source.loadDataset()
        assertNotNull(dataset)
        assertEquals("Radiowissen Folge 1", dataset!!.entries.single().title)
    }

    @Test
    fun `updateLocalDatabase handles HTTP 304 not modified`() {
        val firstClient = OkHttpClient.Builder()
            .addInterceptor(
                responseInterceptor { request ->
                    responseSpec(
                        request = request,
                        code = 200,
                        body = createCompressedDatabaseArchive(
                            databasePath = tempDir.resolve("source-v1.db"),
                            categoryTitle = "Wissen",
                            programSetTitle = "Radiowissen",
                            itemTitle = "Radiowissen Folge 1",
                            createdAtUtc = "2026-04-22T10:00:00Z",
                        ),
                        eTag = "\"v1\"",
                    )
                }
            )
            .build()
        val source = testSource(firstClient)
        assertEquals(SqliteExportDownloadStatus.DOWNLOADED, source.updateLocalDatabase())
        val existingBytes = Files.readAllBytes(tempDir.resolve("mv-audiothek.db"))

        val seenEtag = AtomicReference<String?>()
        val notModifiedSource = testSource(
            OkHttpClient.Builder()
                .addInterceptor(
                    responseInterceptor { request ->
                        seenEtag.set(request.header("If-None-Match"))
                        responseSpec(request = request, code = 304)
                    }
                )
                .build()
        )

        assertEquals(SqliteExportDownloadStatus.NOT_MODIFIED, notModifiedSource.updateLocalDatabase())
        assertEquals("\"v1\"", seenEtag.get())
        assertArrayEquals(existingBytes, Files.readAllBytes(tempDir.resolve("mv-audiothek.db")))
    }

    @Test
    fun `updateLocalDatabase replaces the local file when the ETag changed`() {
        val source = testSource(
            OkHttpClient.Builder()
                .addInterceptor(
                    responseInterceptor { request ->
                        responseSpec(
                            request = request,
                            code = 200,
                            body = createCompressedDatabaseArchive(
                                databasePath = tempDir.resolve("source-v1.db"),
                                categoryTitle = "Wissen",
                                programSetTitle = "Radiowissen",
                                itemTitle = "Radiowissen Folge 1",
                                createdAtUtc = "2026-04-22T10:00:00Z",
                            ),
                            eTag = "\"v1\"",
                        )
                    }
                )
                .build()
        )
        assertEquals(SqliteExportDownloadStatus.DOWNLOADED, source.updateLocalDatabase())

        val updatedSource = testSource(
            OkHttpClient.Builder()
                .addInterceptor(
                    responseInterceptor { request ->
                        assertEquals("\"v1\"", request.header("If-None-Match"))
                        responseSpec(
                            request = request,
                            code = 200,
                            body = createCompressedDatabaseArchive(
                                databasePath = tempDir.resolve("source-v2.db"),
                                categoryTitle = "Wissen",
                                programSetTitle = "Radiowissen",
                                itemTitle = "Radiowissen Folge 2",
                                createdAtUtc = "2026-04-23T10:00:00Z",
                            ),
                            eTag = "\"v2\"",
                        )
                    }
                )
                .build()
        )

        assertEquals(SqliteExportDownloadStatus.DOWNLOADED, updatedSource.updateLocalDatabase())
        val dataset = updatedSource.loadDataset()
        assertNotNull(dataset)
        assertEquals("Radiowissen Folge 2", dataset!!.entries.single().title)
        assertEquals("\"v2\"", readMetadataProperties()["etag"])
    }

    @Test
    fun `updateLocalDatabase preserves the previous file when the download fails`() {
        val source = testSource(
            OkHttpClient.Builder()
                .addInterceptor(
                    responseInterceptor { request ->
                        responseSpec(
                            request = request,
                            code = 200,
                            body = createCompressedDatabaseArchive(
                                databasePath = tempDir.resolve("source-v1.db"),
                                categoryTitle = "Wissen",
                                programSetTitle = "Radiowissen",
                                itemTitle = "Radiowissen Folge 1",
                                createdAtUtc = "2026-04-22T10:00:00Z",
                            ),
                            eTag = "\"v1\"",
                        )
                    }
                )
                .build()
        )
        assertEquals(SqliteExportDownloadStatus.DOWNLOADED, source.updateLocalDatabase())
        val previousDatabaseBytes = Files.readAllBytes(tempDir.resolve("mv-audiothek.db"))
        val previousMetadata = readMetadataProperties()

        val failingSource = testSource(
            OkHttpClient.Builder()
                .addInterceptor(responseInterceptor { request -> responseSpec(request = request, code = 500) })
                .build()
        )

        assertEquals(SqliteExportDownloadStatus.FAILED, failingSource.updateLocalDatabase())
        assertArrayEquals(previousDatabaseBytes, Files.readAllBytes(tempDir.resolve("mv-audiothek.db")))
        assertFalse(Files.exists(tempDir.resolve("mv-audiothek.db.xz")))
        assertEquals(previousMetadata, readMetadataProperties())
        assertEquals("Radiowissen Folge 1", failingSource.loadDataset()!!.entries.single().title)
    }

    private fun testSource(client: OkHttpClient): SqliteExportAudioSource =
        object : SqliteExportAudioSource(client) {
            override fun exportDatabasePath(): Path = tempDir.resolve("mv-audiothek.db")
            override fun metadataFilePath(): Path = tempDir.resolve("download.properties")
            override fun publicDatabaseUrl(): String = Konstanten.AUDIOTHEK_DB_DOWNLOAD_URL.toString()
        }

    private fun createCompressedDatabaseArchive(
        databasePath: Path,
        categoryTitle: String,
        programSetTitle: String,
        itemTitle: String,
        createdAtUtc: String,
    ): ByteArray {
        createNormalizedDatabase(databasePath, categoryTitle, programSetTitle, itemTitle, createdAtUtc)
        val output = ByteArrayOutputStream()
        XZOutputStream(output, LZMA2Options()).use { xzOutput ->
            Files.newInputStream(databasePath).use { input -> input.copyTo(xzOutput) }
        }
        return output.toByteArray()
    }

    private fun createNormalizedDatabase(
        databasePath: Path,
        categoryTitle: String,
        programSetTitle: String,
        itemTitle: String,
        createdAtUtc: String,
    ) {
        SqlDatabaseConfig.createDataSource(databasePath).connection.use { connection ->
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
                insert.setString(2, createdAtUtc)
                insert.executeUpdate()
            }
            connection.prepareStatement("INSERT INTO categories(title) VALUES(?)", Statement.RETURN_GENERATED_KEYS).use { insert ->
                insert.setString(1, categoryTitle)
                insert.executeUpdate()
                insert.generatedKeys.use { keys ->
                    assertTrue(keys.next())
                    val categoryId = keys.getLong(1)
                    connection.prepareStatement(
                        "INSERT INTO program_sets(category_id, title) VALUES(?, ?)",
                        Statement.RETURN_GENERATED_KEYS,
                    ).use { programInsert ->
                        programInsert.setLong(1, categoryId)
                        programInsert.setString(2, programSetTitle)
                        programInsert.executeUpdate()
                        programInsert.generatedKeys.use { programKeys ->
                            assertTrue(programKeys.next())
                            val programSetId = programKeys.getLong(1)
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
                            ).use { itemInsert ->
                                val itemSlug = itemTitle.lowercase().replace(' ', '-')
                                itemInsert.setLong(1, programSetId)
                                itemInsert.setString(2, itemTitle)
                                itemInsert.setString(3, "https://example.invalid/$itemSlug")
                                itemInsert.setString(4, "Wissen")
                                itemInsert.setString(5, "2026-04-22T10:00:00+02:00")
                                itemInsert.setString(6, "Beschreibung")
                                itemInsert.setInt(7, 180)
                                itemInsert.setString(8, "https://example.invalid/$itemSlug.mp3")
                                itemInsert.setLong(9, 2 * 1024 * 1024L)
                                itemInsert.executeUpdate()
                            }
                        }
                    }
                }
            }
            connection.commit()
        }
    }

    private fun responseInterceptor(handler: (Request) -> Response) = Interceptor { chain ->
        handler(chain.request())
    }

    private fun responseSpec(
        request: Request,
        code: Int,
        body: ByteArray = ByteArray(0),
        eTag: String? = null,
    ): Response {
        val builder = Response.Builder()
            .request(request)
            .protocol(Protocol.HTTP_1_1)
            .code(code)
            .message("HTTP $code")
            .body(body.toResponseBody("application/octet-stream".toMediaType()))
        eTag?.let { builder.header("ETag", it) }
        return builder.build()
    }

    private fun readMetadataProperties(): Map<String, String> {
        val properties = Properties()
        Files.newInputStream(tempDir.resolve("download.properties")).use(properties::load)
        return properties.stringPropertyNames().associateWith(properties::getProperty)
    }
}
