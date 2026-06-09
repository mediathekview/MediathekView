package mediathek.tool

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.config.Konstanten
import mediathek.config.StandardLocations
import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.http.MVHttpClient
import mediathek.tool.timer.TimerPool
import okhttp3.HttpUrl
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import java.time.Instant
import kotlin.time.Duration.Companion.seconds

object HlsStreamInfoLogger {
    private val logger = LogManager.getLogger()
    private val json = Json {
        explicitNulls = false
        encodeDefaults = true
    }
    private val ioLock = Any()

    init {
        TimerPool.scheduleWithFixedDelay(
            ::uploadPendingEntriesSafely,
            UPLOAD_INTERVAL_SECONDS.seconds,
            UPLOAD_INTERVAL_SECONDS.seconds,
        )
    }

    fun appendEntry(
        httpStatusCode: Int?,
        m3u8Url: HttpUrl,
        resolutionUrl: HttpUrl?,
        quality: String?,
        fileSize: Long,
    ) {
        val entry = HlsStreamInfoEvent(
            timestamp = Instant.now().toString(),
            appVersion = Konstanten.MVVERSION.toString(),
            platform = "${SystemUtils.OS_NAME}/${SystemUtils.OS_ARCH}",
            country = ApplicationConfiguration.getInstance().geographicLocation.name,
            httpStatus = httpStatusCode,
            m3u8Url = m3u8Url.toString(),
            resolutionUrl = resolutionUrl?.toString(),
            quality = quality,
            fileSize = fileSize,
        )

        try {
            appendNdjsonEntry(json.encodeToString(entry))
        } catch (exception: IOException) {
            logger.debug("Could not append HLS stream info data to {}", outputPath, exception)
            return
        }

        if (currentEndpoint != null) {
            TimerPool.execute(::uploadPendingEntriesSafely)
        }
    }

    private fun appendNdjsonEntry(entry: String) {
        synchronized(ioLock) {
            Files.writeString(
                outputPath,
                "$entry\n",
                StandardOpenOption.CREATE,
                StandardOpenOption.WRITE,
                StandardOpenOption.APPEND,
            )
            trimSpoolIfNeededLocked()
        }
    }

    private fun trimSpoolIfNeededLocked() {
        val lines = readAllSpoolLinesLocked()
        if (lines.size <= MAX_SPOOL_ENTRIES) {
            return
        }

        Files.write(outputPath, lines.takeLast(MAX_SPOOL_ENTRIES))
    }

    private fun uploadPendingEntriesSafely() {
        runCatching { uploadPendingEntries() }
            .onFailure { exception -> logger.debug("Could not upload HLS stream info data from {}", outputPath, exception) }
    }

    private fun uploadPendingEntries() {
        val endpoint = currentEndpoint ?: return
        val pendingBatch = synchronized(ioLock) { readPendingBatchLocked() }
        if (pendingBatch.events.isEmpty()) {
            if (pendingBatch.linesConsumed > 0) {
                synchronized(ioLock) { discardLeadingLinesLocked(pendingBatch.linesConsumed) }
            }
            return
        }

        logger.info(
            "Uploading {} HLS stream info event(s) to {} from {}",
            pendingBatch.events.size,
            endpoint,
            outputPath,
        )

        val request = Request.Builder()
            .url(endpoint)
            .header("User-Agent", ApplicationConfiguration.getInstance().userAgent)
            .header(Konstanten.HLS_STREAM_INFO_TOKEN_HEADER, Konstanten.HLS_STREAM_INFO_TOKEN)
            .post(json.encodeToString(HlsStreamInfoEventBatch(events = pendingBatch.events)).toRequestBody(JSON_MEDIA_TYPE))
            .build()

        MVHttpClient.httpClient.newCall(request).execute().use { response ->
            if (!response.isSuccessful) {
                throw IOException("HTTP ${response.code} for $endpoint")
            }
            logger.info(
                "Uploaded {} HLS stream info event(s) to {} with HTTP {}",
                pendingBatch.events.size,
                endpoint,
                response.code,
            )
        }

        synchronized(ioLock) {
            discardLeadingLinesLocked(pendingBatch.linesConsumed)
        }
    }

    private fun readPendingBatchLocked(): PendingBatch {
        val lines = readAllSpoolLinesLocked()
        if (lines.isEmpty()) {
            return PendingBatch.EMPTY
        }

        val batchLines = lines.take(UPLOAD_BATCH_SIZE)
        val events = buildList(batchLines.size) {
            for (line in batchLines) {
                val trimmed = line.trim()
                if (trimmed.isEmpty()) {
                    continue
                }

                val event = runCatching { json.decodeFromString<HlsStreamInfoEvent>(trimmed) }
                    .onFailure { exception -> logger.debug("Dropping unreadable HLS stream info entry from {}", outputPath, exception) }
                    .getOrNull()
                if (event != null && isUploadable(event)) {
                    add(event)
                }
            }
        }
        return PendingBatch(linesConsumed = batchLines.size, events = events)
    }

    private fun isUploadable(event: HlsStreamInfoEvent): Boolean {
        if (event.httpStatus != null && event.httpStatus !in 100..599) {
            logger.debug("Dropping HLS stream info entry with invalid httpStatus from {}", outputPath)
            return false
        }

        if (event.quality != null && event.quality !in VALID_QUALITIES) {
            logger.debug("Dropping HLS stream info entry with invalid quality from {}", outputPath)
            return false
        }

        if (event.fileSize < INVALID_FILE_SIZE) {
            logger.debug("Dropping HLS stream info entry with invalid fileSize from {}", outputPath)
            return false
        }

        if (event.fileSize == INVALID_FILE_SIZE.toLong() && event.httpStatus !in VALID_INVALID_SIZE_HTTP_STATUSES) {
            logger.debug("Dropping HLS stream info entry with invalid fileSize/httpStatus combination from {}", outputPath)
            return false
        }

        return true
    }

    private fun discardLeadingLinesLocked(linesToDiscard: Int) {
        if (linesToDiscard <= 0) {
            return
        }

        val remainingLines = readAllSpoolLinesLocked().drop(linesToDiscard)
        if (remainingLines.isEmpty()) {
            Files.deleteIfExists(outputPath)
            return
        }

        Files.write(outputPath, remainingLines)
    }

    private fun readAllSpoolLinesLocked(): List<String> {
        if (Files.notExists(outputPath)) {
            return emptyList()
        }
        return Files.readAllLines(outputPath)
    }

    private val outputPath: Path
        get() = StandardLocations.getSettingsDirectory().resolve("hls-stream-info-data.ndjson")

    private val currentEndpoint: HttpUrl?
        get() = Konstanten.HLS_STREAM_INFO_UPLOAD_URL

    @Serializable
    private data class HlsStreamInfoEvent(
        val timestamp: String,
        val appVersion: String,
        val platform: String,
        val country: String,
        val httpStatus: Int? = null,
        val m3u8Url: String,
        val resolutionUrl: String? = null,
        val quality: String? = null,
        val fileSize: Long,
    )

    @Serializable
    private data class HlsStreamInfoEventBatch(
        val schemaVersion: Int = SCHEMA_VERSION,
        val sentAt: String = Instant.now().toString(),
        val events: List<HlsStreamInfoEvent>,
    )

    private data class PendingBatch(
        val linesConsumed: Int,
        val events: List<HlsStreamInfoEvent>,
    ) {
        companion object {
            val EMPTY = PendingBatch(linesConsumed = 0, events = emptyList())
        }
    }

    private const val SCHEMA_VERSION = 1
    private const val INVALID_FILE_SIZE: Byte = -1
    private const val MAX_SPOOL_ENTRIES = 1_000
    private const val UPLOAD_BATCH_SIZE = 100
    private const val UPLOAD_INTERVAL_SECONDS = 60L
    private val JSON_MEDIA_TYPE = "application/json".toMediaType()
    private val VALID_QUALITIES = setOf("LOW", "NORMAL", "HIGH_QUALITY")
    private val VALID_INVALID_SIZE_HTTP_STATUSES = setOf(403, 404)
}
