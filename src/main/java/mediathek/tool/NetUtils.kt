package mediathek.tool

import java.io.IOException
import java.net.InetSocketAddress
import java.net.Socket

class NetUtils {
    companion object {
        /**
         * Check if an address is reachable via network.
         * Replaces InetAddress.isReachable which is unreliable.
         * @param addr url
         * @param timeout Timeout in milliseconds
         * @return true if reachable, otherwise false
         */
        @JvmStatic
        fun isReachable(addr: String, timeout: Int): Boolean {
            return try {
                Socket().use { soc ->
                    // use HTTPS port
                    soc.connect(InetSocketAddress(addr, 443), timeout)
                }
                true
            } catch (ex: IOException) {
                false
            }
        }

        /**
         * Check if string may be an URL.
         * @param str The string to be checked.
         * @return true if string is an URL, otherwise false
         */
        @JvmStatic
        fun isUrl(str: String) : Boolean {
            //TODO it may be better to really check if we are a valid URL. use HttpUrl?
            return str.startsWith("http") || str.startsWith("www")
        }
    }
}