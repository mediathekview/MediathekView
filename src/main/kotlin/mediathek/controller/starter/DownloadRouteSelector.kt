package mediathek.controller.starter

import okhttp3.Call
import okhttp3.Connection
import okhttp3.ConnectionPool
import okhttp3.Dns
import okhttp3.EventListener
import java.net.InetAddress
import java.net.Proxy

/** Keeps route changes local to one download, without pinning CDN addresses. */
internal class DownloadRouteSelector(
    private val delegate: Dns,
    private val pool: ConnectionPool,
    private val onConnected: (String?, String) -> Unit,
    private val onSwitched: (String) -> Unit,
) : Dns {
    private val resolved = mutableMapOf<String, List<InetAddress>>()
    private val avoided = mutableMapOf<String, MutableSet<InetAddress>>()
    private var currentHost: String? = null
    private var currentAddress: InetAddress? = null
    private var switches = 0

    val eventListenerFactory = EventListener.Factory {
        object : EventListener() {
            override fun connectionAcquired(call: Call, connection: Connection) {
                val route = connection.route()
                if (route.proxy.type() == Proxy.Type.DIRECT) {
                    route.socketAddress.address?.let { address ->
                        recordConnection(route.address.url.host, address)
                    }
                }
            }
        }
    }

    @Synchronized
    override fun lookup(hostname: String): List<InetAddress> {
        val addresses = delegate.lookup(hostname)
        resolved[hostname] = addresses
        val preferred = addresses.filterNot { it in avoided[hostname].orEmpty() }
        return preferred.ifEmpty { addresses }
    }

    @Synchronized
    internal fun recordConnection(hostname: String, address: InetAddress) {
        if (hostname != currentHost || address != currentAddress) {
            val previous = currentAddress?.hostAddress.takeIf { hostname == currentHost }
            onConnected(previous, address.hostAddress)
        }
        currentHost = hostname
        currentAddress = address
    }

    @Synchronized
    fun connectedAddress(): String? = currentAddress?.hostAddress

    @Synchronized
    fun canSwitch(): Boolean {
        val host = currentHost ?: return false
        val address = currentAddress ?: return false
        return switches < MAX_ROUTE_SWITCHES &&
            resolved[host].orEmpty().any { it != address && it !in avoided[host].orEmpty() }
    }

    @Synchronized
    fun recordTimeout(): Boolean = switchRoute()

    @Synchronized
    fun switchRoute(): Boolean {
        if (!canSwitch()) return false
        val host = checkNotNull(currentHost)
        val address = checkNotNull(currentAddress)
        avoided.getOrPut(host) { mutableSetOf() }.add(address)
        switches++
        pool.evictAll()
        onSwitched(address.hostAddress)
        return true
    }

    private companion object {
        const val MAX_ROUTE_SWITCHES = 2
    }
}
