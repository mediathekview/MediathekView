package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.suspendCancellableCoroutine
import mediathek.tool.http.MVHttpClient
import okhttp3.Callback
import okhttp3.Call
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.Response
import java.io.IOException
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException

interface OnlineSearchHttpClient {
    suspend fun get(url: String, headers: Map<String, String> = emptyMap()): String
}

class OnlineSearchHttpException(
    val statusCode: Int,
    message: String,
) : IOException("Online search request failed: HTTP $statusCode $message") {
    val isAuthorizationFailure: Boolean
        get() = statusCode == 401 || statusCode == 403
}

class OkHttpOnlineSearchHttpClient(
    private val client: OkHttpClient,
) : OnlineSearchHttpClient {
    override suspend fun get(url: String, headers: Map<String, String>): String = suspendCancellableCoroutine { continuation ->
        val request = Request.Builder()
            .url(url)
            .apply { headers.forEach { (name, value) -> header(name, value) } }
            .get()
            .build()
        val call = client.newCall(request)
        continuation.invokeOnCancellation { call.cancel() }
        call.enqueue(object : Callback {
            override fun onFailure(call: Call, e: IOException) {
                if (continuation.isActive) {
                    continuation.resumeWithException(e)
                }
            }

            override fun onResponse(call: Call, response: Response) {
                response.use {
                    if (!it.isSuccessful) {
                        if (continuation.isActive) {
                            continuation.resumeWithException(
                                OnlineSearchHttpException(it.code, it.message),
                            )
                        }
                        return
                    }
                    val body = it.body.string()
                    if (continuation.isActive) {
                        continuation.resume(body)
                    }
                }
            }
        })
    }
}

object MvOnlineSearchHttpClient : OnlineSearchHttpClient by OkHttpOnlineSearchHttpClient(MVHttpClient.httpClient)
