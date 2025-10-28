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

package mediathek.tool.episodes;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

public class MdrTitleParserTest {
    private static TitleParserManager manager;

    @BeforeAll
    static void setup() {
        manager = new TitleParserManager();
        manager.register("MDR",
                // 1) Parenthesized slash notation, e.g. (S04/E03)
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,3})\\s*\\)",
                // 2) Un-parenthesized slash notation, e.g. S04/E03
                "[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,3})"
        );
    }

    @Test
    void testParenthesizedSlashNotation() {
        String title = "Arm dran, selbst schuld? fragt Tan Caglar (S04/E03)";
        Optional<SeasonEpisode> result = manager.parse("MDR", title);
        assertTrue(result.isPresent(), "Should detect parenthesized S04/E03");
        assertEquals(4, result.get().season());
        assertEquals(3, result.get().episode());
    }

    @Test
    void testAudiodescriptionPattern() {
        String title = "Barrierefrei zum Arzt? fragt Gina Rühl (S05/E03) (Audiodeskription)";
        Optional<SeasonEpisode> result = manager.parse("MDR", title);
        assertTrue(result.isPresent(), "Should detect S05/E03 even with audiodescription suffix");
        assertEquals(5, result.get().season());
        assertEquals(3, result.get().episode());
    }

    @Test
    void testStandalonePattern() {
        String title = "(S06/E04)";
        Optional<SeasonEpisode> result = manager.parse("MDR", title);
        assertTrue(result.isPresent(), "Should detect standalone parenthesized pattern");
        assertEquals(6, result.get().season());
        assertEquals(4, result.get().episode());
    }

    @Test
    void testMixedValidInvalidSlash() {
        String title = "233: Kokon (S06/23) (S06/E23) (Audiodeskription)";
        Optional<SeasonEpisode> result = manager.parse("MDR", title);
        assertTrue(result.isPresent(), "Should detect only the valid (Sxx/Eyy) pattern");
        assertEquals(6, result.get().season());
        assertEquals(23, result.get().episode());
    }

    @Test
    void testNoMatchReturnsEmpty() {
        String title = "Absolute Ehre: Leipziger Läufer Farken trainiert künftig in den USA";
        Optional<SeasonEpisode> result = manager.parse("MDR", title);
        assertFalse(result.isPresent(), "Should not detect any season/episode info");
    }
}