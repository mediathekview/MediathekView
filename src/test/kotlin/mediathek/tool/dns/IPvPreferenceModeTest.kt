package mediathek.tool.dns

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class IPvPreferenceModeTest {

    @Test
    fun fromString() {
        val r = IPvPreferenceMode.fromString("ip_v6_only")
        assertEquals(IPvPreferenceMode.IPV6_ONLY, r)
    }
}