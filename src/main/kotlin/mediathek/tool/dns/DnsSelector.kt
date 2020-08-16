package mediathek.tool.dns

import okhttp3.Dns
import org.apache.logging.log4j.LogManager
import java.net.Inet4Address
import java.net.Inet6Address
import java.net.InetAddress

class DnsSelector(private val preferenceMode: IPvPreferenceMode) : Dns {
    /**
     * return IP adresses based on preferred mode.
     */
    override fun lookup(hostname: String): List<InetAddress> {
        var addresses = Dns.SYSTEM.lookup(hostname)

        addresses = when (preferenceMode) {
            IPvPreferenceMode.IPV6_FIRST -> addresses.sortedBy { Inet4Address::class.java.isInstance(it) }
            IPvPreferenceMode.IPV4_FIRST -> addresses.sortedBy { Inet6Address::class.java.isInstance(it) }
            IPvPreferenceMode.IPV6_ONLY -> addresses.filter { Inet6Address::class.java.isInstance(it) }
            IPvPreferenceMode.IPV4_ONLY -> addresses.filter { Inet4Address::class.java.isInstance(it) }
            IPvPreferenceMode.SYSTEM -> addresses
        }

        logger.trace("Dns ($hostname): " + addresses.joinToString(", ") { it.toString() })

        return addresses
    }

    companion object {
        private val logger = LogManager.getLogger(DnsSelector::class.java.name)
    }
}