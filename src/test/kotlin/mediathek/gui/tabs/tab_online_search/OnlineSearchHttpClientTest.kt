package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import okhttp3.Interceptor
import okhttp3.OkHttpClient
import org.junit.jupiter.api.Test
import java.io.IOException

class OnlineSearchHttpClientTest {
    @Test
    fun `cancelling get cancels underlying OkHttp call`() = runBlocking {
        val requestStarted = CompletableDeferred<Unit>()
        val callCancelled = CompletableDeferred<Unit>()
        val client = OkHttpClient.Builder()
            .addInterceptor(
                Interceptor { chain ->
                    requestStarted.complete(Unit)
                    while (!chain.call().isCanceled()) {
                        Thread.sleep(10)
                    }
                    callCancelled.complete(Unit)
                    throw IOException("cancelled")
                },
            )
            .build()
        val httpClient = OkHttpOnlineSearchHttpClient(client)

        val request = async(Dispatchers.Default) {
            httpClient.get("https://example.invalid/test")
        }
        withTimeout(2_000) { requestStarted.await() }

        request.cancelAndJoin()

        withTimeout(2_000) { callCancelled.await() }
    }
}
