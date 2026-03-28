/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool;

import org.junit.jupiter.api.Test;

import java.util.EnumSet;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class LanguageCodeTest {
    /**
     * Test if all codes can be converted to 3 letter code and there is no exception.
     */
    @Test
    void testConversion() {
        for (var code : EnumSet.allOf(LanguageCode.class)) {
            var out = code.getISO3Language();
            assertFalse(out.isEmpty());
        }
    }

    @Test
    void testSelectedIso3Mappings() {
        assertEquals("deu", LanguageCode.de.getISO3Language());
        assertEquals("eng", LanguageCode.en.getISO3Language());
        assertEquals("fra", LanguageCode.fr.getISO3Language());
        assertEquals("ita", LanguageCode.it.getISO3Language());
        assertEquals("spa", LanguageCode.es.getISO3Language());
    }
}
