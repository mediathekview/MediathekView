package mediathek.config

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import org.apache.logging.log4j.LogManager
import kotlin.time.Duration
import kotlin.time.Duration.Companion.milliseconds

internal class DownloadStateSaveQueue(
    private val save: () -> Unit,
    private val debounce: Duration = 250.milliseconds,
) {
    private val logger = LogManager.getLogger(DownloadStateSaveQueue::class.java)
    private val requests = Channel<Request>(Channel.UNLIMITED)
    private val requestLock = Any()
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    private var saveQueued = false
    private var stopped = false

    init {
        scope.launch {
            for (request in requests) {
                when (request) {
                    Request.Save -> {
                        delay(debounce)
                        synchronized(requestLock) {
                            saveQueued = false
                        }
                        runCatching(save).onFailure { logger.error("Could not save download state", it) }
                    }
                    is Request.Stop -> {
                        request.done.complete(Unit)
                        break
                    }
                }
            }
        }
    }

    fun requestSave() {
        synchronized(requestLock) {
            if (!stopped && !saveQueued) {
                saveQueued = true
                check(requests.trySend(Request.Save).isSuccess)
            }
        }
    }

    fun stopAndFlush() {
        val done = synchronized(requestLock) {
            if (stopped) return
            stopped = true
            CompletableDeferred<Unit>().also { check(requests.trySend(Request.Stop(it)).isSuccess) }
        }
        runBlocking { done.await() }
    }

    private sealed interface Request {
        data object Save : Request
        data class Stop(val done: CompletableDeferred<Unit>) : Request
    }
}
