package mediathek.tool

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.tool.http.MVHttpClient
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.IOException


object Ipify {
    private val json = Json { ignoreUnknownKeys = true }
    private val client = MVHttpClient.getInstance().httpClient

    /**
     * Get the public ip address through ipify's api.
     * @return The public ip address.
     * @throws IOException If there is an IO error.
     */
    @get:Throws(IOException::class)
    val publicIp: String
        get() = getPublicIp()

    @Throws(IOException::class)
    fun getPublicIp(httpClient: OkHttpClient = client): String =
        getUrl("https://api64.ipify.org?format=json", httpClient)

    @Throws(IOException::class)
    fun getUrl(url: String, httpClient: OkHttpClient = client): String {
        val request = Request.Builder()
            .url(url)
            .header("Accept", "application/json")
            .get()
            .build()

        httpClient.newCall(request).execute().use { response ->
            if (!response.isSuccessful) {
                throw IOException("Failed to fetch public IP: HTTP ${response.code}")
            }

            return json.decodeFromString(IpifyResponse.serializer(), response.body.string()).ip
        }
    }

    @Serializable
    private data class IpifyResponse(
        val ip: String,
    )
}
