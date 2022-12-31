package mediathek.tool

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.net.URI
import java.net.URL
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
            val ipify = URL("https://api64.ipify.org")
            val conn = ipify.openConnection()
            var ip: String
            conn.getInputStream().use { inputStream ->
                InputStreamReader(inputStream).use { isr ->
                    BufferedReader(isr).use { reader ->
                        ip = reader.readLine()
                    }
                }
            }
            return ip
        }

    @JvmStatic
    @get:Throws(IOException::class)
    val publicIpNew: String
        get() {
            val client: HttpClient = HttpClient.newBuilder()
                .version(HttpClient.Version.HTTP_2)
                .followRedirects(HttpClient.Redirect.ALWAYS)
                .build()

            val request = HttpRequest.newBuilder()
                .uri(URI.create("https://api64.ipify.org"))
                .build()

            val response = client.send(request, HttpResponse.BodyHandlers.ofString());
            return response.body()
        }
}