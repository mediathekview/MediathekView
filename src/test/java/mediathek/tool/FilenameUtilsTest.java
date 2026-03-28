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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class FilenameUtilsTest {
    @Test
    void trailing_test_with_leading_whitespace() {
        var testStr = " betrifft: ... ";
        var result = FilenameUtils.removeWindowsTrailingDots(testStr);
        Assertions.assertEquals(" betrifft:", result);
    }

    @Test
    void trailing_test_without_leading_whitespace() {
        var testStr = "betrifft: ...";
        var result = FilenameUtils.removeWindowsTrailingDots(testStr);
        assertEquals("betrifft:", result);
    }

    @Test
    void test_remove_starting_dots() {
        var testStr = "....Paula";
        final var expected = "Paula";

        var result = FilenameUtils.stripStartingDots(testStr);
        assertEquals(expected, result);
    }

    @Test
    void test_remove_starting_dots_with_leading_whitespace() {
        // this should not modify string as we have whitespace at beginning
        var testStr = " ....Paula";
        var result = FilenameUtils.stripStartingDots(testStr);
        assertEquals(testStr, result);
    }

    @Test
    void test_utf_to_ascii_encoding() {
        var src = "Häuser Bäume Höfe Gärten daß Ü ü ö ä Ä Ö ß Â À Å Á Č Đ É ł Ł \u003F";
        var expected = "Haeuser Baeume Hoefe Gaerten dass UE ue oe ae AE OE ss A A A A C D E l L ?";
        var res = FilenameUtils.convertToASCIIEncoding(src, false);

        assertEquals(expected, res);
    }

    @Test
    void removeWindowsTrailingDots() {
        var testStr = "betrifft: ... ";
        var result = FilenameUtils.removeWindowsTrailingDots(testStr);
        assertEquals("betrifft:", result);
    }

    @Test
    void convertToASCIIEncoding() {
        var testStr = "hellöworld.txt";
        var result = FilenameUtils.convertToASCIIEncoding(testStr, false);
        assertEquals("helloeworld.txt", result);
    }
}
