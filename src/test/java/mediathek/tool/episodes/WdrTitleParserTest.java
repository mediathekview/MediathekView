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

public class WdrTitleParserTest {
    private static TitleParserManager manager;

    @BeforeAll
    static void setup() {
        manager = new TitleParserManager();
        manager.register("WDR",
                // 1) Parenthesized slash notation: (S03/E07)
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 2) Parenthesized continuous notation: (S03E07)
                "\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})\\s*\\)",
                // 3) Inline slash notation: S03/E07
                "[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})",
                // 4) Inline continuous notation: S03E07
                "[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})",
                // 5) German Staffel/Folge: Staffel 2, Folge 5
                "Staffel\\s*(?<season>\\d{1,2})\\s*,\\s*Folge\\s*(?<episode>\\d{1,3})"
        );
    }

    @Test
    void testParenthesizedSlashNotation() {
        String title = "Tatort Dortmund (S03/E07)";
        Optional<SeasonEpisode> result = manager.parse("WDR", title);
        assertTrue(result.isPresent(), "Should detect parenthesized slash notation");
        assertEquals(3, result.get().season());
        assertEquals(7, result.get().episode());
    }

    @Test
    void testParenthesizedContinuousNotation() {
        String title = "WDR Lokalzeit (S04E12) Spezial";
        Optional<SeasonEpisode> result = manager.parse("WDR", title);
        assertTrue(result.isPresent(), "Should detect parenthesized continuous notation");
        assertEquals(4, result.get().season());
        assertEquals(12, result.get().episode());
    }

    @Test
    void testInlineSlashNotation() {
        String title = "Sport inside S05/E03 Bericht";
        Optional<SeasonEpisode> result = manager.parse("WDR", title);
        assertTrue(result.isPresent(), "Should detect inline slash notation");
        assertEquals(5, result.get().season());
        assertEquals(3, result.get().episode());
    }

    @Test
    void testInlineContinuousNotation() {
        String title = "Quiz Taxi S06E08 – beste Fragen";
        Optional<SeasonEpisode> result = manager.parse("WDR", title);
        assertTrue(result.isPresent(), "Should detect inline continuous notation");
        assertEquals(6, result.get().season());
        assertEquals(8, result.get().episode());
    }

    @Test
    void testStaffelFolgeNotation() {
        String title = "Dokumentation: Expedition 1, Staffel 2, Folge 5";
        Optional<SeasonEpisode> result = manager.parse("WDR", title);
        assertTrue(result.isPresent(), "Should detect Staffel/Folge notation");
        assertEquals(2, result.get().season());
        assertEquals(5, result.get().episode());
    }

    @Test
    void testNoMatchReturnsEmpty() {
        String title = "WDR Aktuell am Abend";
        Optional<SeasonEpisode> result = manager.parse("WDR", title);
        assertFalse(result.isPresent(), "Should return empty for titles without season/episode info");
    }
}