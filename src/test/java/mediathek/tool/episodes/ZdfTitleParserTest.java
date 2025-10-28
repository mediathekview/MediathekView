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

public class ZdfTitleParserTest {
    private static TitleParserManager manager;

    @BeforeAll
    static void setup() {
        manager = new TitleParserManager();
        manager.register("ZDF",
                // Parenthesized slash notation, e.g. (S2025/E11)
                "\\(\\s*[sS](?<season>\\d{1,4})/[eE](?<episode>\\d{1,4})\\s*\\)",
                // Parenthesized continuous notation, e.g. (S2024E11)
                "\\(\\s*[sS](?<season>\\d{1,4})[eE](?<episode>\\d{1,4})\\s*\\)",
                // Un-parenthesized slash notation, e.g. S2025/E102
                "[sS](?<season>\\d{1,4})/[eE](?<episode>\\d{1,4})",
                // Un-parenthesized continuous notation, e.g. S01E16
                "[sS](?<season>\\d{1,4})[eE](?<episode>\\d{1,4})"
        );
    }

    @Test
    void testParenthesizedSlashYearPattern() {
        String title = "heute-show vom 14. Februar 2025 - Nachrichtensatire mit Oliver Welke (S2025/E03)";
        Optional<SeasonEpisode> result = manager.parse("ZDF", title);
        assertTrue(result.isPresent(), "Should detect parenthesized slash year pattern");
        assertEquals(2025, result.get().season());
        assertEquals(3, result.get().episode());
    }

    @Test
    void testParenthesizedContinuousYearPattern() {
        String title = "April 2024 (S2024E11) (Gebärdensprache)";
        Optional<SeasonEpisode> result = manager.parse("ZDF", title);
        assertTrue(result.isPresent(), "Should detect parenthesized continuous year pattern");
        assertEquals(2024, result.get().season());
        assertEquals(11, result.get().episode());
    }

    @Test
    void testParenthesizedSlashNonYearPattern() {
        String title = "Eine kleine Sehnsucht (S07/E10)";
        Optional<SeasonEpisode> result = manager.parse("ZDF", title);
        assertTrue(result.isPresent(), "Should detect parenthesized slash non-year pattern");
        assertEquals(7, result.get().season());
        assertEquals(10, result.get().episode());
    }

    @Test
    void testParenthesizedContinuousNonYearPattern() {
        String title = "Ein heißer Tanz (S01E16)";
        Optional<SeasonEpisode> result = manager.parse("ZDF", title);
        assertTrue(result.isPresent(), "Should detect parenthesized continuous non-year pattern");
        assertEquals(1, result.get().season());
        assertEquals(16, result.get().episode());
    }

    @Test
    void testUnparenthesizedSlashPattern() {
        String title = "heute journal update vom 2. Juni 2025 S2025/E102";
        Optional<SeasonEpisode> result = manager.parse("ZDF", title);
        assertTrue(result.isPresent(), "Should detect un-parenthesized slash pattern");
        assertEquals(2025, result.get().season());
        assertEquals(102, result.get().episode());
    }

    @Test
    void testNegativeCaseNoMatch() {
        String title = "heute-show vom 13. September 2024 - Nachrichtensatire mit Oliver Welke";
        Optional<SeasonEpisode> result = manager.parse("ZDF", title);
        assertFalse(result.isPresent(), "Should return empty for titles without S/E info");
    }
}