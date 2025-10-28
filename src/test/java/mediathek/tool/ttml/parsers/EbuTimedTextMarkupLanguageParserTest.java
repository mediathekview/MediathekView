/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.tool.ttml.parsers;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class EbuTimedTextMarkupLanguageParserTest {

    @Test
    @DisplayName("Check alignment conversion")
    void convertAlignment() {
        try (EbuTimedTextMarkupLanguageParser parser = new EbuTimedTextMarkupLanguageParser()) {
            // convert trash to 0
            int alignment = parser.convertAlignment("hello");
            assertEquals(0, alignment);
            //now check the real values
            alignment = parser.convertAlignment("start");
            assertEquals(-1, alignment, "start conversion failed");
            alignment = parser.convertAlignment("left");
            assertEquals(-1, alignment, "left conversion failed");
            alignment = parser.convertAlignment("center");
            assertEquals(0, alignment, "center conversion failed");
            alignment = parser.convertAlignment("right");
            assertEquals(1, alignment, "right conversion failed");
            alignment = parser.convertAlignment("end");
            assertEquals(1, alignment, "end conversion failed");
        }
    }

    @Test
    void convertRegion() {
        try (EbuTimedTextMarkupLanguageParser parser = new EbuTimedTextMarkupLanguageParser()) {
            // convert trash
            int region = parser.convertRegion("hello");
            assertEquals(2, region, "trash conversion failed");
            region = parser.convertRegion("before");
            assertEquals(8, region, "before conversion failed");
            region = parser.convertRegion("center");
            assertEquals(5, region, "center conversion failed");
        }
    }
}