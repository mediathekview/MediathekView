package mediathek.controller.starter

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import mediathek.daten.DatenFilm
import org.apache.logging.log4j.LogManager

internal class DownloadSeenHistoryWriter(private val write: (List<DatenFilm>) -> Unit) {
    private val logger = LogManager.getLogger(DownloadSeenHistoryWriter::class.java)
    private val requests = Channel<Request>(Channel.UNLIMITED)
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)

    init {
        scope.launch {
            for (request in requests) {
                when (request) {
                    is Request.MarkSeen -> runCatching { write(request.films) }
                        .onFailure { logger.error("Could not mark downloaded films as seen", it) }
                    is Request.Flush -> request.done.complete(Unit)
                }
            }
        }
    }

    fun markSeen(films: List<DatenFilm>) {
        if (films.isNotEmpty()) {
            check(requests.trySend(Request.MarkSeen(films.toList())).isSuccess)
        }
    }

    fun flush() {
        val done = CompletableDeferred<Unit>()
        check(requests.trySend(Request.Flush(done)).isSuccess)
        runBlocking { done.await() }
    }

    private sealed interface Request {
        data class MarkSeen(val films: List<DatenFilm>) : Request
        data class Flush(val done: CompletableDeferred<Unit>) : Request
    }
}
