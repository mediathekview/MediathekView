package mediathek.tool

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.net.URL

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
}