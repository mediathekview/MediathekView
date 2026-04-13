package mediathek.config

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class MVConfigTest {
    @Test
    fun enum_find_invalid() {
        val res = MVConfig.Configs.find("blaXblubb")
        assertFalse(res)
    }

    @Test
    fun enum_find_valid() {
        val res = MVConfig.Configs.find(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN.cValue)
        assertTrue(res)
    }
}
