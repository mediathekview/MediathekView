package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.sync.Semaphore
import kotlinx.coroutines.sync.withPermit

internal suspend fun <T, R> Iterable<T>.mapConcurrently(
    concurrency: Int = DEFAULT_ONLINE_SEARCH_DETAIL_CONCURRENCY,
    transform: suspend (T) -> R,
): List<R> {
    require(concurrency > 0) { "concurrency must be positive" }
    val semaphore = Semaphore(concurrency)
    return coroutineScope {
        map { item ->
            async {
                semaphore.withPermit {
                    transform(item)
                }
            }
        }.awaitAll()
    }
}

internal const val DEFAULT_ONLINE_SEARCH_DETAIL_CONCURRENCY = 4
