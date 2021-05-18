package mediathek.config;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MVConfigTest {

    @Test
    void enum_find_invalid() {
        var res = MVConfig.Configs.find("blaXblubb");
        assertFalse(res);
    }

    @Test
    void enum_find_valid() {
        var res = MVConfig.Configs.find(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN.cValue);
        assertTrue(res);
    }
}