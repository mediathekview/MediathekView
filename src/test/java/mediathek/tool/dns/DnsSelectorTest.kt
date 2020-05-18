package mediathek.tool.dns

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class DnsSelectorTest {
    private val hostName = "www.mediathekview.de"
    @Test
    fun lookup() {
        var lookup = DnsSelector(IPvPreferenceMode.IPV4_ONLY)
        var result = lookup.lookup(hostName)
        assertEquals(result.size,1);

        lookup = DnsSelector(IPvPreferenceMode.IPV6_ONLY)
        result = lookup.lookup(hostName)
        assertEquals(result.size,1);
    }
}