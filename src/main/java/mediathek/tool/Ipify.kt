package mediathek.tool

import java.io.IOException
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse


object Ipify {
    /**
     * Get the public ip address through ipify's api.
     * @return The public ip address.
     * @throws IOException If there is an IO error.
     */
    @JvmStatic
    @get:Throws(IOException::class)
    val publicIp: String
        get() {
            val client: HttpClient = HttpClient.newBuilder()
                .version(HttpClient.Version.HTTP_2)
                .followRedirects(HttpClient.Redirect.ALWAYS)
                .build()

            val request = HttpRequest.newBuilder()
                .uri(URI.create("https://api64.ipify.org"))
                .build()

            val response = client.send(request, HttpResponse.BodyHandlers.ofString())
            return response.body()
        }
}