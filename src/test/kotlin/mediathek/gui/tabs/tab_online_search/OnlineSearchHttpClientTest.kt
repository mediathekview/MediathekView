package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import mediathek.config.Konstanten
import okhttp3.Interceptor
import okhttp3.OkHttpClient
import okhttp3.Protocol
import okhttp3.Response
import okhttp3.ResponseBody.Companion.toResponseBody
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.io.IOException

class OnlineSearchHttpClientTest {
    @Test
    fun `get sends configured online search user agent`() = runBlocking {
        val client = OkHttpClient.Builder()
            .addInterceptor(
                Interceptor { chain ->
                    assertEquals(Konstanten.JSOUP_USER_AGENT, chain.request().header("User-Agent"))
                    assertEquals("value", chain.request().header("X-Test"))
                    Response.Builder()
                        .request(chain.request())
                        .protocol(Protocol.HTTP_1_1)
                        .code(200)
                        .message("OK")
                        .body("ok".toResponseBody())
                        .build()
                },
            )
            .build()
        val httpClient = OkHttpOnlineSearchHttpClient(client)

        val body = httpClient.get(
            "https://example.invalid/test",
            mapOf(
                "User-Agent" to "ignored",
                "X-Test" to "value",
            ),
        )

        assertEquals("ok", body)
    }

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
