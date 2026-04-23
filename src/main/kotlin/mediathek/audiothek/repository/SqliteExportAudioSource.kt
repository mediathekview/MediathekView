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

package mediathek.audiothek.repository

import mediathek.audiothek.model.AudioDataset
import mediathek.audiothek.model.AudioEntry
import mediathek.config.Konstanten
import mediathek.tool.http.MVHttpClient
import mediathek.tool.sql.SqlDatabaseConfig
import okhttp3.OkHttpClient
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import org.tukaani.xz.XZInputStream
import java.io.IOException
import java.io.InputStream
import java.net.URI
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.sql.Connection
import java.time.Instant
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.ZoneId
import java.util.*
import kotlin.math.ceil

open class SqliteExportAudioSource(
    private val client: OkHttpClient = MVHttpClient.getInstance().httpClient,
) {
    private val logger = LogManager.getLogger(SqliteExportAudioSource::class.java)

    fun loadDataset(): AudioDataset? {
        val exportPath = exportDatabasePath()
        if (!Files.exists(exportPath)) {
            return null
        }

        SqlDatabaseConfig.createDataSource(exportPath).connection.use { connection ->
            val createdAt = connection.readMetadataInstant("created_at_utc")
            val entries = connection.loadAudioEntries()
            if (entries.isEmpty()) {
                return null
            }
            return AudioDataset(
                createdAtLocal = createdAt?.atZone(ZoneId.systemDefault())?.toLocalDateTime(),
                entries = entries,
            )
        }
    }

    open fun updateLocalDatabase(): SqliteExportDownloadStatus {
        val exportPath = exportDatabasePath()
        val metadata = readDownloadMetadata()
        val sourceUrl = publicDatabaseUrl()
        val request = Request.Builder()
            .url(sourceUrl)
            .apply {
                metadata?.eTag?.let { header("If-None-Match", it) }
            }
            .get()
            .build()

        logger.trace("Starting MediathekView Audiothek download from {}", sourceUrl)

        try {
            client.newCall(request).execute().use { response ->
                when (response.code) {
                    304 -> {
                        if (!Files.exists(exportPath)) {
                            logger.warn(
                                "MediathekView Audiothek returned HTTP 304 for {}, but no local database is available",
                                sourceUrl,
                            )
                            return SqliteExportDownloadStatus.FAILED
                        }
                        logger.trace("MediathekView Audiothek not modified (HTTP 304)")
                        return SqliteExportDownloadStatus.NOT_MODIFIED
                    }
                }

                if (!response.isSuccessful) {
                    logger.warn(
                        "Failed to download MediathekView Audiothek from {}: HTTP {}",
                        sourceUrl,
                        response.code,
                    )
                    return SqliteExportDownloadStatus.FAILED
                }

                val body = response.body
                if (body.contentLength() == 0L) {
                    logger.warn("Failed to download MediathekView Audiothek from {}: empty response body", sourceUrl)
                    return SqliteExportDownloadStatus.FAILED
                }

                val tempCompressedPath = createSiblingTempFile(compressedArchiveTempTarget(exportPath))
                val tempDatabasePath = createSiblingTempFile(exportPath)
                try {
                    Files.createDirectories(exportPath.parent)
                    body.byteStream().use { input ->
                        Files.newOutputStream(tempCompressedPath).use { output -> input.copyTo(output) }
                    }

                    if (Files.size(tempCompressedPath) <= 0L) {
                        throw IOException("Downloaded MediathekView Audiothek archive is empty")
                    }

                    decompressArchiveToDatabase(tempCompressedPath, tempDatabasePath)
                    validateDatabase(tempDatabasePath)
                    replaceFileAtomically(tempDatabasePath, exportPath)
                    writeDownloadMetadata(
                        SqliteExportDownloadMetadata(
                            sourceUrl = sourceUrl,
                            eTag = response.header("ETag"),
                        )
                    )
                    logger.info("Successfully updated MediathekView Audiothek at {}", exportPath)
                    return SqliteExportDownloadStatus.DOWNLOADED
                } finally {
                    Files.deleteIfExists(tempCompressedPath)
                    Files.deleteIfExists(tempDatabasePath)
                }
            }
        } catch (error: Exception) {
            logger.warn("Failed to update MediathekView Audiothek from {}", sourceUrl, error)
            return SqliteExportDownloadStatus.FAILED
        }
    }

    protected open fun exportDatabasePath(): Path = defaultExportDatabasePath()
    protected open fun metadataFilePath(): Path = defaultMetadataFilePath()
    protected open fun publicDatabaseUrl(): String = PUBLIC_AUDIO_DATABASE_URL

    private fun Connection.loadAudioEntries(): List<AudioEntry> =
        prepareStatement(
            """
            SELECT c.title AS category_title,
                   ps.title AS program_set_title,
                   i.title,
                   i.page_url,
                   i.sender,
                   i.publish_date,
                   i.description,
                   i.duration_seconds,
                   i.preferred_audio_asset,
                   i.preferred_audio_asset_file_size_bytes
            FROM items i
            JOIN program_sets ps ON ps.id = i.program_set_id
            JOIN categories c ON c.id = ps.category_id
            ORDER BY c.id, ps.id, i.id
            """.trimIndent()
        ).use { statement ->
            statement.executeQuery().use { resultSet ->
                buildList {
                    while (resultSet.next()) {
                        val sender = resultSet.getString("sender").orEmpty()
                        val categoryTitle = resultSet.getString("category_title").orEmpty()
                        val programSetTitle = resultSet.getString("program_set_title").orEmpty()
                        add(
                            AudioEntry(
                                channel = sender,
                                genre = categoryTitle,
                                theme = programSetTitle.ifBlank { categoryTitle },
                                title = resultSet.getString("title").orEmpty(),
                                durationMinutes = resultSet.getNullableInt("duration_seconds")?.let(::secondsToMinutes),
                                sizeMb = resultSet.getNullableLong("preferred_audio_asset_file_size_bytes")?.let(::bytesToMegabytes),
                                description = resultSet.getString("description").orEmpty(),
                                audioUrl = parseUri(resultSet.getString("preferred_audio_asset")),
                                websiteUrl = parseUri(resultSet.getString("page_url")),
                                isNew = false,
                                isPodcast = true,
                                isDuplicate = false,
                                publishedAt = resultSet.getString("publish_date")?.let(::parsePublishDate),
                            )
                        )
                    }
                }
            }
        }

    private fun Connection.readMetadataInstant(key: String): Instant? =
        prepareStatement(
            """
            SELECT value
            FROM metadata
            WHERE key = ?
            """.trimIndent()
        ).use { statement ->
            statement.setString(1, key)
            statement.executeQuery().use { resultSet ->
                if (!resultSet.next()) {
                    return null
                }
                return resultSet.getString("value")
                    ?.trim()
                    ?.takeIf(String::isNotEmpty)
                    ?.let(Instant::parse)
            }
        }

    private fun java.sql.ResultSet.getNullableInt(columnLabel: String): Int? {
        val value = getInt(columnLabel)
        return if (wasNull()) null else value
    }

    private fun java.sql.ResultSet.getNullableLong(columnLabel: String): Long? {
        val value = getLong(columnLabel)
        return if (wasNull()) null else value
    }

    private fun parsePublishDate(value: String): LocalDateTime? =
        runCatching { OffsetDateTime.parse(value).toLocalDateTime() }.getOrNull()

    private fun parseUri(value: String?): URI? =
        value
            ?.trim()
            ?.takeIf(String::isNotEmpty)
            ?.let { runCatching { URI(it) }.getOrNull() }

    private fun readDownloadMetadata(): SqliteExportDownloadMetadata? {
        val metadataFile = metadataFilePath()
        if (!Files.exists(metadataFile)) {
            return null
        }

        val properties = Properties()
        Files.newInputStream(metadataFile).use(properties::load)
        val sourceUrl = properties.getProperty(KEY_SOURCE_URL)?.takeIf(String::isNotBlank) ?: return null
        return SqliteExportDownloadMetadata(
            sourceUrl = sourceUrl,
            eTag = properties.getProperty(KEY_ETAG)?.takeIf(String::isNotBlank),
        )
    }

    private fun writeDownloadMetadata(metadata: SqliteExportDownloadMetadata) {
        val metadataFile = metadataFilePath()
        Files.createDirectories(metadataFile.parent)
        val properties = Properties().apply {
            setProperty(KEY_SOURCE_URL, metadata.sourceUrl)
            metadata.eTag?.let { setProperty(KEY_ETAG, it) }
        }
        Files.newOutputStream(metadataFile).use { properties.store(it, "MediathekView Audiothek download cache") }
    }

    private fun createSiblingTempFile(targetPath: Path): Path {
        Files.createDirectories(targetPath.parent)
        return Files.createTempFile(targetPath.parent, "${targetPath.fileName}.", ".part")
    }

    private fun compressedArchiveTempTarget(exportPath: Path): Path =
        exportPath.resolveSibling("${exportPath.fileName}.xz")

    private fun decompressArchiveToDatabase(compressedPath: Path, databasePath: Path) {
        Files.newInputStream(compressedPath).use { compressedInput ->
            XZInputStream(compressedInput).use { decompressedInput ->
                writeInputStream(databasePath, decompressedInput)
            }
        }
        if (Files.size(databasePath) <= 0L) {
            throw IOException("Decompressed MediathekView Audiothek database is empty")
        }
    }

    private fun writeInputStream(targetPath: Path, inputStream: InputStream) {
        Files.newOutputStream(targetPath).use { output -> inputStream.copyTo(output) }
    }

    private fun validateDatabase(databasePath: Path) {
        SqlDatabaseConfig.createDataSource(databasePath).connection.use { connection ->
            connection.createStatement().use { statement ->
                statement.executeQuery("PRAGMA quick_check").use { resultSet ->
                    check(resultSet.next() && resultSet.getString(1) == "ok") {
                        "Downloaded MediathekView Audiothek failed SQLite quick_check"
                    }
                }
                statement.executeQuery(
                    """
                    SELECT COUNT(*)
                    FROM sqlite_master
                    WHERE type = 'table'
                      AND name IN ('metadata', 'categories', 'program_sets', 'items')
                    """.trimIndent()
                ).use { resultSet ->
                    check(resultSet.next() && resultSet.getInt(1) == 4) {
                        "Downloaded MediathekView Audiothek does not contain the expected schema"
                    }
                }
            }
        }
    }

    private fun replaceFileAtomically(source: Path, target: Path) {
        try {
            Files.move(source, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source, target, StandardCopyOption.REPLACE_EXISTING)
        }
    }

    private fun secondsToMinutes(totalSeconds: Int): Int {
        if (totalSeconds <= 0) {
            return 0
        }
        return (totalSeconds / 60).coerceAtLeast(1)
    }

    private fun bytesToMegabytes(bytes: Long): Int {
        if (bytes <= 0L) {
            return 0
        }
        return ceil(bytes / 1024.0 / 1024.0).toInt().coerceAtLeast(1)
    }

    companion object {
        private const val DOWNLOAD_METADATA_FILENAME = "mv-audiothek-db-download.properties"
        private const val KEY_SOURCE_URL = "sourceUrl"
        private const val KEY_ETAG = "etag"

        private val PUBLIC_AUDIO_DATABASE_URL = Konstanten.AUDIOTHEK_DB_DOWNLOAD_URL.toString()

        private fun defaultExportDatabasePath(): Path =
            AudiothekPaths.defaultSqliteExportPath()

        private fun defaultMetadataFilePath(): Path =
            AudiothekPaths.defaultAudiothekCachePath().resolve(DOWNLOAD_METADATA_FILENAME)
    }
}

enum class SqliteExportDownloadStatus {
    DOWNLOADED,
    NOT_MODIFIED,
    FAILED,
}

private data class SqliteExportDownloadMetadata(
    val sourceUrl: String,
    val eTag: String?,
)
