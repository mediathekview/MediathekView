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

package mediathek.audiothek.download

import kotlinx.coroutines.*
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.audiothek.model.AudioEntry
import mediathek.config.StandardLocations
import mediathek.tool.FileUtils
import mediathek.tool.http.MVHttpClient
import okhttp3.*
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.io.InputStream
import java.io.InterruptedIOException
import java.io.OutputStream
import java.net.SocketException
import java.net.URI
import java.nio.channels.FileChannel
import java.nio.file.*
import java.util.*
import kotlin.io.path.exists

class PersistentAudioDownloadManager(
    private val httpClientProvider: () -> OkHttpClient,
    private val onDownloadCompleted: (AudioDownloadTaskSnapshot) -> Unit,
    private val onDownloadFailed: (AudioDownloadTaskSnapshot) -> Unit
) {
    private val logger = LogManager.getLogger(PersistentAudioDownloadManager::class.java)
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    private val tasks = linkedMapOf<String, ManagedTask>()
    private val listeners = mutableListOf<(List<AudioDownloadTaskSnapshot>) -> Unit>()
    private val storePath = StandardLocations.getSettingsDirectory().resolve("audiothek-downloads.json")
    private var lastPersistNanos = 0L
    private var lastNotifyNanos = 0L

    init {
        loadPersistedTasks()
    }

    fun addListener(listener: (List<AudioDownloadTaskSnapshot>) -> Unit) {
        listeners += listener
        listener(tasks.values.map { it.snapshot })
    }

    fun snapshot(taskId: String): AudioDownloadTaskSnapshot? {
        return tasks[taskId]?.snapshot
    }

    fun enqueue(entry: AudioEntry, targetFile: Path) {
        val task = ManagedTask(
            snapshot = AudioDownloadTaskSnapshot(
                id = UUID.randomUUID().toString(),
                audioName = entry.title.ifBlank { "(ohne Titel)" },
                channel = entry.channel,
                theme = entry.theme,
                audioUrl = entry.audioUrl?.toString().orEmpty(),
                targetFile = targetFile.toString(),
                tempFile = buildTempFilePath(targetFile).toString(),
                state = AudioDownloadTaskState.PAUSED
            )
        )
        tasks[task.snapshot.id] = task
        persistAndNotify(force = true)
        resume(task.snapshot.id)
    }

    fun resume(taskId: String) {
        val task = tasks[taskId] ?: return
        if (task.snapshot.audioUrl.isBlank() || task.job?.isActive == true) {
            return
        }

        task.stopMode = StopMode.NONE
        task.snapshot = task.snapshot.copy(
            state = AudioDownloadTaskState.DOWNLOADING,
            errorMessage = null
        )
        persistAndNotify(force = true)

        task.job = scope.launch {
            try {
                runDownload(task)
            } catch (_: CancellationException) {
                when (task.stopMode) {
                    StopMode.PAUSE, StopMode.SHUTDOWN, StopMode.CANCEL -> handleExpectedStop(task)
                    StopMode.NONE -> task.updateSnapshot(task.snapshot.copy(state = AudioDownloadTaskState.PAUSED))
                }
            } catch (ex: Exception) {
                if (isExpectedStop(task, ex)) {
                    handleExpectedStop(task)
                } else {
                    logger.warn("Audio download failed for {}", task.snapshot.audioUrl, ex)
                    task.updateSnapshot(
                        task.snapshot.copy(
                            state = AudioDownloadTaskState.FAILED,
                            errorMessage = ex.message ?: ex::class.java.simpleName
                        )
                    )
                    onDownloadFailed(task.snapshot)
                }
            } finally {
                task.activeCall = null
                task.job = null
                task.stopMode = StopMode.NONE
            }
        }
    }

    fun pause(taskId: String) {
        stopTask(taskId, StopMode.PAUSE) { copy(state = AudioDownloadTaskState.PAUSED) }
    }

    fun cancel(taskId: String) {
        stopTask(taskId, StopMode.CANCEL, AudioDownloadTaskSnapshot::asCancelled)
    }

    fun remove(taskId: String) {
        val task = tasks[taskId] ?: return
        if (task.job?.isActive == true) {
            return
        }
        deleteIfExists(Path.of(task.snapshot.tempFile))
        tasks.remove(taskId)
        persistAndNotify(force = true)
    }

    suspend fun shutdown() {
        for (task in tasks.values) {
            if (task.job?.isActive == true) {
                task.stopMode = StopMode.SHUTDOWN
                task.activeCall?.cancel()
                task.job?.cancel()
            } else if (task.snapshot.state == AudioDownloadTaskState.DOWNLOADING) {
                task.snapshot = task.snapshot.copy(state = AudioDownloadTaskState.PAUSED)
            }
        }
        persistAndNotify(force = true)
        for (task in tasks.values) {
            task.job?.cancelAndJoin()
        }
    }

    private fun runDownload(task: ManagedTask) {
        val snapshot = task.snapshot
        val audioUrl = URI.create(snapshot.audioUrl)
        val targetFile = resolveFollowedPath(Path.of(snapshot.targetFile))
        val tempFile = resolveFollowedPath(Path.of(snapshot.tempFile))
        ensureParentDirectoryExists(targetFile)

        val existingBytes = sizeIfExists(tempFile)?.coerceAtLeast(0L) ?: 0L
        if (existingBytes != snapshot.downloadedBytes) {
            task.updateSnapshot(
                snapshot.copy(
                    targetFile = targetFile.toString(),
                    tempFile = tempFile.toString(),
                    downloadedBytes = existingBytes
                )
            )
        }

        executeDownloadWithFallback(task, audioUrl.toString(), tempFile, targetFile)
    }

    private fun executeDownloadWithFallback(task: ManagedTask, url: String, tempFile: Path, targetFile: Path) {
        val httpClient = httpClientProvider()
        try {
            executeDownload(task, url, tempFile, targetFile, httpClient)
        } catch (ex: Exception) {
            if (task.stopMode != StopMode.NONE || ex !is IOException) {
                throw ex
            }
            logger.info("Audiothek-Download fehlgeschlagen, versuche HTTP/1.1-Fallback für {}", url, ex)
            val http11Client = httpClient.newBuilder()
                .protocols(listOf(Protocol.HTTP_1_1))
                .build()
            executeDownload(task, url, tempFile, targetFile, http11Client)
        }
    }

    constructor(
        onDownloadCompleted: (AudioDownloadTaskSnapshot) -> Unit,
        onDownloadFailed: (AudioDownloadTaskSnapshot) -> Unit,
    ) : this({ MVHttpClient.httpClient }, onDownloadCompleted, onDownloadFailed)

    private fun executeDownload(task: ManagedTask, url: String, tempFile: Path, targetFile: Path, client: OkHttpClient) {
        val downloadedBytes = task.snapshot.downloadedBytes
        val request = Request.Builder()
            .url(url)
            .get()
            .apply {
                if (downloadedBytes > 0L) {
                    header("Range", "bytes=$downloadedBytes-")
                }
            }
            .build()

        val call = client.newCall(request)
        task.activeCall = call
        call.execute().use { response ->
            when {
                response.code == 416 && downloadedBytes > 0L -> {
                    deleteIfExists(tempFile)
                    task.updateSnapshot(task.snapshot.copy(downloadedBytes = 0L, totalBytes = null))
                    executeDownload(task, url, tempFile, targetFile, client)
                }
                response.isSuccessful -> {
                    val body = response.body
                    if (response.code == 200 && downloadedBytes > 0L) {
                        deleteIfExists(tempFile)
                        task.updateSnapshot(
                            task.snapshot.copy(
                                downloadedBytes = 0L,
                                totalBytes = determineTotalBytes(response, body, 0L)
                            )
                        )
                        executeDownload(task, url, tempFile, targetFile, client)
                        return
                    }
                    streamResponseToFile(task, response, body, tempFile)
                    moveDownloadedFile(tempFile, targetFile)
                    task.updateSnapshot(
                        task.snapshot.copy(
                            state = AudioDownloadTaskState.COMPLETED,
                            downloadedBytes = task.snapshot.totalBytes ?: task.snapshot.downloadedBytes,
                            errorMessage = null
                        )
                    )
                    onDownloadCompleted(task.snapshot)
                }
                else -> error("Download fehlgeschlagen: HTTP ${response.code}")
            }
        }
    }

    private fun streamResponseToFile(task: ManagedTask, response: Response, body: ResponseBody, tempFile: Path) {
        val existingBytes = task.snapshot.downloadedBytes
        val totalBytes = determineTotalBytes(response, body, existingBytes)
        task.updateSnapshot(task.snapshot.copy(totalBytes = totalBytes))

        val options = if (existingBytes > 0L) {
            arrayOf(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
        } else {
            arrayOf(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
        }

        body.byteStream().use { input ->
            Files.newOutputStream(tempFile, *options).buffered().use { output ->
                copyStream(task, input, output, existingBytes, totalBytes)
            }
        }
    }

    private fun copyStream(
        task: ManagedTask,
        input: InputStream,
        output: OutputStream,
        startBytes: Long,
        totalBytes: Long?
    ) {
        val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
        var downloadedBytes = startBytes
        while (true) {
            val read = input.read(buffer)
            if (read < 0) {
                break
            }
            output.write(buffer, 0, read)
            downloadedBytes += read
            task.updateSnapshot(
                task.snapshot.copy(
                    downloadedBytes = downloadedBytes,
                    totalBytes = totalBytes
                ),
                persist = shouldPersistProgress()
            )
        }
    }

    private fun determineTotalBytes(
        response: Response,
        body: ResponseBody,
        downloadedBytes: Long
    ): Long? {
        response.header("Content-Range")
            ?.substringAfterLast('/')
            ?.toLongOrNull()
            ?.let { return it }
        val contentLength = body.contentLength().takeIf { it >= 0L } ?: return null
        return if (response.code == 206) downloadedBytes + contentLength else contentLength
    }

    private fun updateTask(task: ManagedTask, snapshot: AudioDownloadTaskSnapshot, persist: Boolean = true) {
        task.snapshot = snapshot
        persistAndNotify(force = persist)
    }

    private fun ManagedTask.updateSnapshot(
        snapshot: AudioDownloadTaskSnapshot,
        persist: Boolean = true
    ) = updateTask(this, snapshot, persist)

    private fun stopTask(
        taskId: String,
        stopMode: StopMode,
        stateChange: AudioDownloadTaskSnapshot.() -> AudioDownloadTaskSnapshot
    ) {
        val task = tasks[taskId] ?: return
        task.stopMode = stopMode
        task.activeCall?.cancel()
        task.job?.cancel()
        if (task.job == null) {
            task.updateSnapshot(task.snapshot.stateChange())
        }
    }

    private fun isExpectedStop(task: ManagedTask, exception: Exception): Boolean {
        return task.stopMode != StopMode.NONE &&
            (exception is SocketException || exception is InterruptedIOException)
    }

    private fun handleExpectedStop(task: ManagedTask) {
        when (task.stopMode) {
            StopMode.PAUSE, StopMode.SHUTDOWN -> task.updateSnapshot(task.snapshot.copy(state = AudioDownloadTaskState.PAUSED))
            StopMode.CANCEL -> {
                deleteIfExists(Path.of(task.snapshot.tempFile))
                task.updateSnapshot(task.snapshot.asCancelled())
            }
            StopMode.NONE -> Unit
        }
    }

    private fun persistAndNotify(force: Boolean) {
        if (force || shouldPersistProgress()) {
            persist()
        }
        if (force || shouldNotifyProgress()) {
            val snapshots = tasks.values.map { it.snapshot }
            listeners.forEach { it(snapshots) }
        }
    }

    private fun shouldPersistProgress(): Boolean {
        val now = System.nanoTime()
        if (now - lastPersistNanos >= PROGRESS_PERSIST_INTERVAL_NANOS) {
            lastPersistNanos = now
            return true
        }
        return false
    }

    private fun shouldNotifyProgress(): Boolean {
        val now = System.nanoTime()
        if (now - lastNotifyNanos >= PROGRESS_NOTIFY_INTERVAL_NANOS) {
            lastNotifyNanos = now
            return true
        }
        return false
    }

    private fun persist() {
        runCatching {
            ensureParentDirectoryExists(storePath)
            Files.writeString(storePath, JSON.encodeToString(tasks.values.map { it.snapshot }))
        }.onFailure {
            logger.warn("Failed to persist audiothek downloads", it)
        }
    }

    private fun loadPersistedTasks() {
        if (!storePath.exists()) {
            return
        }

        val snapshots = runCatching {
            JSON.decodeFromString<List<AudioDownloadTaskSnapshot>>(Files.readString(storePath))
        }.getOrElse {
            logger.warn("Failed to read persisted audiothek downloads", it)
            emptyList()
        }

        snapshots
            .asSequence()
            .filterNot(AudioDownloadTaskSnapshot::shouldBeDiscardedOnStartup)
            .map(AudioDownloadTaskSnapshot::asRestoredSnapshot)
            .forEach { snapshot -> tasks[snapshot.id] = ManagedTask(snapshot) }
        persistAndNotify(force = true)
    }

    private fun buildTempFilePath(targetFile: Path): Path {
        return targetFile.resolveSibling(targetFile.fileName.toString() + ".audiothek.part")
    }

    private fun deleteIfExists(path: Path) {
        runCatching {
            withFileRetry(path, "delete") {
                Files.deleteIfExists(path)
            }
        }
            .onFailure { logger.debug("Failed to delete {}", path, it) }
    }

    private fun moveDownloadedFile(tempFile: Path, targetFile: Path) {
        ensureParentDirectoryExists(targetFile)
        withFileRetry(targetFile, "move") {
            FileUtils.moveAtomicallyWithFallback(tempFile, targetFile)
        }
    }

    private fun ensureParentDirectoryExists(path: Path) {
        val parent = path.parent ?: return
        withFileRetry(parent, "create-directories") {
            Files.createDirectories(parent)
        }
    }

    private fun resolveFollowedPath(path: Path): Path {
        val normalized = path.toAbsolutePath().normalize()
        val parent = normalized.parent ?: return normalized
        val resolvedParent = runCatching { parent.toRealPath() }.getOrElse { parent }
        return resolvedParent.resolve(normalized.fileName.toString())
    }

    private fun <T> withFileRetry(path: Path, operation: String, action: () -> T): T {
        var lastFailure: Exception? = null
        repeat(FILE_OPERATION_RETRY_COUNT) { attempt ->
            try {
                return action()
            } catch (ex: Exception) {
                if (!isRetryableFileException(ex) || attempt == FILE_OPERATION_RETRY_COUNT - 1) {
                    throw ex
                }
                lastFailure = ex
                logger.debug(
                    "Retrying file operation {} for {} ({}/{})",
                    operation,
                    path,
                    attempt + 1,
                    FILE_OPERATION_RETRY_COUNT,
                    ex
                )
                Thread.sleep(FILE_OPERATION_RETRY_DELAY_MILLIS)
            }
        }
        throw lastFailure ?: IllegalStateException("File operation retry failed for $operation: $path")
    }

    private fun isRetryableFileException(ex: Exception): Boolean {
        return ex is FileSystemException || ex is AccessDeniedException || ex is IOException
    }

    private data class ManagedTask(
        var snapshot: AudioDownloadTaskSnapshot,
        var job: Job? = null,
        var activeCall: Call? = null,
        var stopMode: StopMode = StopMode.NONE
    )

    private enum class StopMode {
        NONE,
        PAUSE,
        CANCEL,
        SHUTDOWN
    }

    companion object {
        private const val PROGRESS_PERSIST_INTERVAL_NANOS = 1_000_000_000L
        private const val PROGRESS_NOTIFY_INTERVAL_NANOS = 250_000_000L
        private const val FILE_OPERATION_RETRY_COUNT = 3
        private const val FILE_OPERATION_RETRY_DELAY_MILLIS = 150L
        private val JSON = Json {
            ignoreUnknownKeys = true
            prettyPrint = true
        }
    }
}

@Serializable
data class AudioDownloadTaskSnapshot(
    val id: String,
    val audioName: String,
    val channel: String,
    val theme: String,
    val audioUrl: String,
    val targetFile: String,
    val tempFile: String,
    val downloadedBytes: Long = 0L,
    val totalBytes: Long? = null,
    val state: AudioDownloadTaskState,
    val errorMessage: String? = null
)

@Serializable
enum class AudioDownloadTaskState {
    DOWNLOADING,
    PAUSED,
    COMPLETED,
    CANCELLED,
    FAILED
}

private fun sizeIfExists(path: Path): Long? {
    if (!path.exists()) {
        return null
    }

    return try {
        FileChannel.open(path, StandardOpenOption.READ).use { it.size() }
    } catch (_: NoSuchFileException) {
        null
    }
}

private fun AudioDownloadTaskSnapshot.asCancelled(): AudioDownloadTaskSnapshot {
    return copy(
        state = AudioDownloadTaskState.CANCELLED,
        downloadedBytes = 0L,
        totalBytes = null,
        errorMessage = null
    )
}

private fun AudioDownloadTaskSnapshot.asRestoredSnapshot(): AudioDownloadTaskSnapshot = copy(
    state = when (state) {
        AudioDownloadTaskState.DOWNLOADING -> AudioDownloadTaskState.PAUSED
        else -> state
    }
)

private fun AudioDownloadTaskSnapshot.shouldBeDiscardedOnStartup(): Boolean = when (state) {
    AudioDownloadTaskState.COMPLETED, AudioDownloadTaskState.FAILED -> true
    else -> false
}
