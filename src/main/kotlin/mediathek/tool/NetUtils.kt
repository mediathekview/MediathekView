package mediathek.tool

import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import java.io.IOException
import java.net.InetSocketAddress
import java.net.Socket
import kotlin.time.Duration

class NetUtils {
    companion object {
        /**
         * Check if an address is reachable via network.
         * Replaces InetAddress.isReachable which is unreliable.
         * @param addr url
         * @param timeout connect timeout
         * @return true if reachable, otherwise false
         */
        fun isReachable(addr: String, timeout: Duration): Boolean {
            return try {
                Socket().use { soc ->
                    // use HTTPS port
                    val timeoutMillis = timeout.inWholeMilliseconds.coerceIn(0, Int.MAX_VALUE.toLong()).toInt()
                    soc.connect(InetSocketAddress(addr, 443), timeoutMillis)
                }
                true
            } catch (_: IOException) {
                false
            }
        }

        /**
         * Check if string may be an URL.
         * @param str The string to be checked.
         * @return true if string is an URL, otherwise false
         */
        fun isUrl(str: String): Boolean = str.toHttpUrlOrNull() != null
    }
}
