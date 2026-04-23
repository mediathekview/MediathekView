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

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import mediathek.audiothek.model.AudioDataset
import mediathek.audiothek.model.AudioEntry
import mediathek.tool.http.MVHttpClient
import okhttp3.OkHttpClient
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import org.tukaani.xz.XZInputStream
import java.io.ByteArrayInputStream
import java.io.InputStream
import java.util.*
import java.util.concurrent.TimeUnit

class AudioRepository(
    private val client: OkHttpClient = MVHttpClient.getInstance().httpClient.newBuilder()
        .connectTimeout(5, TimeUnit.SECONDS)
        .readTimeout(5, TimeUnit.SECONDS)
        .writeTimeout(5, TimeUnit.SECONDS)
        .addNetworkInterceptor { chain ->
            val requestWithoutUserAgent = chain.request().newBuilder()
                .removeHeader("User-Agent")
                .build()
            chain.proceed(requestWithoutUserAgent)
        }
        .build(),
    private val resolver: AudioSourceResolver = AudioSourceResolver(client),
    private val parser: AudioParser = AudioParser(),
    private val cache: AudioDownloadCache = AudioDownloadCache(),
    private val sqliteExportSource: SqliteExportAudioSource = SqliteExportAudioSource(),
) {
    private val logger = LogManager.getLogger(AudioRepository::class.java)

    suspend fun loadAudiothek(useCachedOnDownloadFailure: Boolean = false): AudioLoadResult = withContext(Dispatchers.IO) {
        val cachedMetadata = cache.readMetadata()
        val sourceUrls = resolver.resolveSourceUrls(cachedMetadata?.sourceUrl)
        var lastError: Exception? = null

        for ((index, sourceUrl) in sourceUrls.withIndex()) {
            val request = Request.Builder()
                .url(sourceUrl)
                .apply {
                    cachedMetadata?.eTag?.let { header("If-None-Match", it) }
                    cachedMetadata?.lastModified?.let { header("If-Modified-Since", it) }
                }
                .get()
                .build()

            try {
                client.newCall(request).execute().use { response ->
                    when (response.code) {
                        304 -> {
                            if (!cache.hasCachedAudio()) {
                                error("Audiothek cache miss after HTTP 304 for $sourceUrl")
                            }
                            logger.info("Audiothek unchanged, no new audio file downloaded for {}", sourceUrl)
                            return@withContext cachedResult(sourceUrl, AudioDownloadStatus.NOT_MODIFIED)
                        }

                        !in 200..299 -> {
                            if (cache.hasCachedAudio()) {
                                logger.warn(
                                    "Audiothek download skipped after HTTP {} for {}, using cached audio file",
                                    response.code,
                                    sourceUrl
                                )
                                return@withContext cachedResult(sourceUrl, AudioDownloadStatus.USED_CACHE_AFTER_FAILURE)
                            }
                        }
                    }

                    if (!response.isSuccessful) {
                        error("Failed to load audiothek data from $sourceUrl: HTTP ${response.code}")
                    }

                    val bodyBytes = response.body.bytes()
                    val cachedBodyBytes = cache.readCachedAudioBytes()
                    if (cachedBodyBytes != null && Arrays.equals(cachedBodyBytes, bodyBytes)) {
                        logger.info("Audiothek unchanged, downloaded content matches cached audio file for {}", sourceUrl)
                        return@withContext cachedResult(sourceUrl, AudioDownloadStatus.NOT_MODIFIED)
                    }

                    cache.write(
                        sourceUrl = sourceUrl,
                        eTag = response.header("ETag"),
                        lastModified = response.header("Last-Modified"),
                        body = bodyBytes
                    )

                    return@withContext mergeSqliteExportEntries(
                        p2toolsDataset = parseAudioDataset(sourceUrl, ByteArrayInputStream(bodyBytes)),
                        downloadStatus = AudioDownloadStatus.DOWNLOADED,
                    )
                }
            } catch (error: Exception) {
                lastError = error
                logger.warn(
                    "Audiothek download failed for {} ({} von {}), versuche nächste Quelle",
                    sourceUrl,
                    index + 1,
                    sourceUrls.size,
                    error
                )
            }
        }

        if (useCachedOnDownloadFailure && cache.hasCachedAudio()) {
            val cachedSourceUrl = cachedMetadata?.sourceUrl ?: sourceUrls.firstOrNull().orEmpty()
            logger.warn("Audiothek download failed for all known sources, using cached audio file", lastError)
            return@withContext cachedResult(cachedSourceUrl, AudioDownloadStatus.USED_CACHE_AFTER_FAILURE)
        }

        throw lastError ?: error("Keine Audiothek-Quelle verfügbar")
    }

    private fun cachedResult(sourceUrl: String, status: AudioDownloadStatus): AudioLoadResult =
        mergeSqliteExportEntries(
            p2toolsDataset = loadCachedDataset(sourceUrl),
            downloadStatus = status,
        )

    private fun loadCachedDataset(sourceUrl: String): AudioDataset =
        cache.openCachedAudio().use { parseAudioDataset(sourceUrl, it) }

    private fun parseAudioDataset(sourceUrl: String, body: InputStream): AudioDataset =
        openPayloadStream(sourceUrl, body).use { parser.parse(it, sourceUrl) }

    private fun mergeSqliteExportEntries(
        p2toolsDataset: AudioDataset,
        downloadStatus: AudioDownloadStatus,
    ): AudioLoadResult {
        val sqliteUpdateStatus = sqliteExportSource.updateLocalDatabase()
        if (sqliteUpdateStatus == SqliteExportDownloadStatus.FAILED) {
            logger.warn("MediathekView Audiothek update failed, continuing with the previous local database if available")
        }
        val sqliteDataset = runCatching { sqliteExportSource.loadDataset() }
            .onFailure { logger.warn("Failed to load MediathekView Audiothek data", it) }
            .getOrNull()
            ?: return AudioLoadResult(
                dataset = p2toolsDataset,
                downloadStatus = downloadStatus,
                sqliteDownloadStatus = sqliteUpdateStatus,
            )

        val mergedEntries = LinkedHashMap<String, AudioEntry>()
        val mergeStats = AudioMergeStats()
        sqliteDataset.entries.forEach { entry ->
            val previous = mergedEntries.putIfAbsent(audioEntryMergeKey(entry), entry)
            if (previous == null) {
                mergeStats.keptFromSqlite++
            } else {
                mergeStats.duplicateSqliteRows++
            }
        }
        p2toolsDataset.entries.forEach { entry ->
            val previous = mergedEntries.putIfAbsent(audioEntryMergeKey(entry), entry)
            if (previous == null) {
                mergeStats.addedFromP2Tools++
            } else {
                if (previous.sourceLabel == AudioSourceLabels.MEDIATHEK_VIEW) {
                    mergeStats.skippedP2ToolsBecauseSqlite++
                } else {
                    mergeStats.duplicateP2ToolsRows++
                }
            }
        }

        logger.info(
            "Audiothek merge: sqlite primary entries={}, kept from sqlite={}, duplicate sqlite rows={}, p2tools entries={}, added from p2tools={}, p2tools rows skipped because sqlite already had them={}, duplicate p2tools rows={}",
            sqliteDataset.entries.size,
            mergeStats.keptFromSqlite,
            mergeStats.duplicateSqliteRows,
            p2toolsDataset.entries.size,
            mergeStats.addedFromP2Tools,
            mergeStats.skippedP2ToolsBecauseSqlite,
            mergeStats.duplicateP2ToolsRows,
        )

        return AudioLoadResult(
            dataset = p2toolsDataset.withEntriesAndSqliteMetadata(
                entries = mergedEntries.values.toList(),
                sqliteDataset = sqliteDataset,
            ),
            downloadStatus = downloadStatus,
            sqliteDownloadStatus = sqliteUpdateStatus,
        )
    }

    private fun audioEntryMergeKey(entry: AudioEntry): String {
        entry.audioUrl?.toString()?.takeIf(String::isNotBlank)?.let { return "audio:$it" }
        entry.websiteUrl?.toString()?.takeIf(String::isNotBlank)?.let { return "website:$it" }
        return buildString {
            append(entry.channel.trim())
            append('|')
            append(entry.title.trim())
            append('|')
            append(entry.publishedAt?.toString().orEmpty())
            append('|')
            append(entry.durationMinutes?.toString().orEmpty())
        }
    }

    private fun openPayloadStream(sourceUrl: String, body: InputStream): InputStream =
        if (sourceUrl.endsWith(".xz")) XZInputStream(body) else body
}

data class AudioLoadResult(
    val dataset: AudioDataset,
    val downloadStatus: AudioDownloadStatus,
    val sqliteDownloadStatus: SqliteExportDownloadStatus,
) {
    fun hasUpdatedSource(): Boolean =
        downloadStatus == AudioDownloadStatus.DOWNLOADED || sqliteDownloadStatus == SqliteExportDownloadStatus.DOWNLOADED

    fun reloadMessage(): String? = when {
        hasUpdatedSource() -> null
        downloadStatus == AudioDownloadStatus.USED_CACHE_AFTER_FAILURE ->
            "Es konnte keine neue Datei geladen werden.\nDie zwischengespeicherte wird weiter verwendet."
        else ->
            "Es konnte keine neue Datei geladen werden.\nDie vorhandene ist bereits aktuell."
    }
}

enum class AudioDownloadStatus {
    DOWNLOADED,
    NOT_MODIFIED,
    USED_CACHE_AFTER_FAILURE
}

private data class AudioMergeStats(
    var keptFromSqlite: Int = 0,
    var duplicateSqliteRows: Int = 0,
    var addedFromP2Tools: Int = 0,
    var skippedP2ToolsBecauseSqlite: Int = 0,
    var duplicateP2ToolsRows: Int = 0,
)
