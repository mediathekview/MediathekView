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

public class HrTitleParserTest {
    private static TitleParserManager manager;

    @BeforeAll
    static void setup() {
        manager = new TitleParserManager();
        manager.register("HR",
                // 1) Parenthesized slash notation, e.g. (S01/E11)
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,3})\\s*\\)",
                // 2) Staffel X, Folge Y pattern, e.g. Staffel 10, Folge 5
                "Staffel\\s*(?<season>\\d{1,2})\\s*,\\s*Folge\\s*(?<episode>\\d{1,3})"
        );
    }

    @Test
    void testParenthesizedSlashSimple() {
        String title = "\"Der Vorfall\" (S01/E11)";
        Optional<SeasonEpisode> result = manager.parse("HR", title);
        assertTrue(result.isPresent(), "Should detect simple parenthesized S/E");
        assertEquals(1, result.get().season());
        assertEquals(11, result.get().episode());
    }

    @Test
    void testParenthesizedSlashWithTeil() {
        String title = "100 Jahre Flughafen – Vom Propellerflugzeug zum Airbus A380 (Teil 2) (S13/E06)";
        Optional<SeasonEpisode> result = manager.parse("HR", title);
        assertTrue(result.isPresent(), "Should detect S/E even with preceding (Teil N)");
        assertEquals(13, result.get().season());
        assertEquals(6, result.get().episode());
    }

    @Test
    void testSeasonEpisodeStaffelFolge() {
        String title = "Serie: Wohnen mal anders, Staffel 10, Folge 3: U-Haus";
        Optional<SeasonEpisode> result = manager.parse("HR", title);
        assertTrue(result.isPresent(), "Should detect Staffel/Folge pattern");
        assertEquals(10, result.get().season());
        assertEquals(3, result.get().episode());
    }

    @Test
    void testNumericParenthesesWithoutSlash() {
        String title = "Die stolze Folge (130)";
        Optional<SeasonEpisode> result = manager.parse("HR", title);
        assertFalse(result.isPresent(), "Should not detect numeric-only parentheses as S/E");
    }

    @Test
    void testNoSeasonEpisodeReturnsEmpty() {
        String title = "\"Aktionstag gegen den Schmerz\" – wie gehen Menschen in Hessen mit Schmerz um?";
        Optional<SeasonEpisode> result = manager.parse("HR", title);
        assertFalse(result.isPresent(), "Titles without S/E info should return empty");
    }
}