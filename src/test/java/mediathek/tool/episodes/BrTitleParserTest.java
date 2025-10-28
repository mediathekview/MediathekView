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

public class BrTitleParserTest {
    private static TitleParserManager manager;

    @BeforeAll
    static void setup() {
        manager = new TitleParserManager();
        manager.register("BR",
                // 1) Parenthesized "(S03/E05)"
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 2) Prefix-colon pattern: "100: ... (S04/E20)"
                "^(?:\\d{1,3}):[^\\(]*\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 3) "Folge 5: ... (S02/E05)"
                "Folge\\s*\\d{1,3}[:\\-][^\\(]*\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)"
        );
    }

    @Test
    void testParenthesizedSlashNotation() {
        String title = "Ein Haus am Berg (S03/E05)";
        Optional<SeasonEpisode> result = manager.parse("BR", title);
        assertTrue(result.isPresent(), "Should detect parenthesized S/E slash notation");
        assertEquals(3, result.get().season());
        assertEquals(5, result.get().episode());
    }

    @Test
    void testPrefixColonPattern() {
        String title = "100: Türkisch für Fortgeschrittene (S04/E20)";
        Optional<SeasonEpisode> result = manager.parse("BR", title);
        assertTrue(result.isPresent(), "Should detect prefix-colon pattern");
        assertEquals(4, result.get().season());
        assertEquals(20, result.get().episode());
    }

    @Test
    void testFolgeColonPattern() {
        String title = "Folge 5: Fluch vom Eckstein (S01/E05)";
        Optional<SeasonEpisode> result = manager.parse("BR", title);
        assertTrue(result.isPresent(), "Should detect Folge-colon pattern");
        assertEquals(1, result.get().season());
        assertEquals(5, result.get().episode());
    }

    @Test
    void testNoMatchOnDateAndFolgeWithoutParentheses() {
        String title = "... Vater sein dagegen sehr · 07.04.08 | Folge 96";
        Optional<SeasonEpisode> result = manager.parse("BR", title);
        assertFalse(result.isPresent(), "Should not detect season/episode in date and Folge-only titles");
    }
}