package mediathek.controller.starter

import okhttp3.ConnectionPool
import okhttp3.Dns
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.net.InetAddress

internal class DownloadRouteSelectorTest {
    @Test
    fun firstTimeoutMovesTheConnectedAddressOutOfDnsResults() {
        val first = address("192.0.2.1")
        val second = address("192.0.2.2")
        val third = address("192.0.2.3")
        val connections = mutableListOf<Pair<String?, String>>()
        val switches = mutableListOf<String>()
        val selector = DownloadRouteSelector(
            Dns { listOf(first, second, third) },
            ConnectionPool(),
            { previous, address -> connections.add(previous to address) },
            switches::add,
        )

        assertEquals(listOf(first, second, third), selector.lookup("example.test"))
        selector.recordConnection("example.test", first)
        assertTrue(selector.recordTimeout())
        assertEquals(listOf(second, third), selector.lookup("example.test"))
        selector.recordConnection("example.test", second)
        assertTrue(selector.recordTimeout())
        assertEquals(listOf(third), selector.lookup("example.test"))
        selector.recordConnection("example.test", third)
        assertFalse(selector.canSwitch())
        assertEquals(
            listOf(null to first.hostAddress, first.hostAddress to second.hostAddress, second.hostAddress to third.hostAddress),
            connections,
        )
        assertEquals(listOf(first.hostAddress, second.hostAddress), switches)
    }

    @Test
    fun noAlternativeLeavesTheOnlyAddressAvailable() {
        val only = address("192.0.2.1")
        val selector = DownloadRouteSelector(Dns { listOf(only) }, ConnectionPool(), { _, _ -> }, {})
        selector.lookup("example.test")
        selector.recordConnection("example.test", only)

        assertFalse(selector.canSwitch())
        assertFalse(selector.recordTimeout())
        assertFalse(selector.recordTimeout())
        assertEquals(listOf(only), selector.lookup("example.test"))
    }

    private fun address(value: String): InetAddress = InetAddress.getByName(value)
}
