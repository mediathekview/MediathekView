package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.suspendCancellableCoroutine
import mediathek.config.Konstanten
import mediathek.tool.http.MVHttpClient
import okhttp3.*
import java.io.IOException
import java.time.Duration
import java.time.Instant
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException

interface OnlineSearchHttpClient {
    suspend fun get(url: String, headers: Map<String, String> = emptyMap()): String
}

class OnlineSearchHttpException(
    val statusCode: Int,
    message: String,
    val url: String? = null,
    val retryAfter: Duration? = null,
) : IOException("Online search request failed: HTTP $statusCode $message${url?.let { " for $it" }.orEmpty()}") {
    val isAuthorizationFailure: Boolean
        get() = statusCode == 401 || statusCode == 403

    val isRateLimit: Boolean
        get() = statusCode == 429

    val isUnavailableItem: Boolean
        get() = statusCode == 404 || statusCode == 410
}

class OkHttpOnlineSearchHttpClient(
    private val client: OkHttpClient,
) : OnlineSearchHttpClient {
    override suspend fun get(url: String, headers: Map<String, String>): String = suspendCancellableCoroutine { continuation ->
        val request = Request.Builder()
            .url(url)
            .apply { headers.forEach { (name, value) -> header(name, value) } }
            .header("User-Agent", Konstanten.JSOUP_USER_AGENT)
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
                                OnlineSearchHttpException(
                                    statusCode = it.code,
                                    message = it.message,
                                    url = url,
                                    retryAfter = it.header("Retry-After")?.toRetryAfterDuration(),
                                ),
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

private fun String.toRetryAfterDuration(): Duration? {
    val trimmed = trim()
    trimmed.toLongOrNull()?.let { seconds -> return Duration.ofSeconds(seconds) }
    return runCatching {
        val retryAt = ZonedDateTime.parse(trimmed, DateTimeFormatter.RFC_1123_DATE_TIME).toInstant()
        Duration.between(Instant.now(), retryAt).takeUnless { it.isNegative }
    }.getOrNull()
}
