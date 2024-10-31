package mediathek.tool;

import org.junit.jupiter.api.Test;

import java.util.EnumSet;

import static org.junit.jupiter.api.Assertions.assertFalse;

class LanguageCodeTest {
    /**
     * Test if all codes can be converted to 3 letter code and there is no exception.
     */
    @Test
    public void testConversion() {
        for (var code : EnumSet.allOf(LanguageCode.class)) {
            var out = code.getISO3Language();
            assertFalse(out.isEmpty());
        }

    }
}