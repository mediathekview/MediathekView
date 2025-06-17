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

public class RbbTitleParserTest {
    private static TitleParserManager manager;

    @BeforeAll
    static void setup() {
        manager = new TitleParserManager();
        manager.register("RBB",
                // 1) Parenthesized S/E pattern, e.g. (S05/E01)
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 2) Parenthesized ratio-only pattern, e.g. (1/4)
                "\\(\\s*(?<season>\\d{1,2})/(?<episode>\\d{1,2})\\s*\\)"
        );
    }

    @Test
    void testSeasonEpisodeSlashNotation() {
        String title = "Chez Krömer | Boris Palmer (S03/E04)";
        Optional<SeasonEpisode> result = manager.parse("RBB", title);
        assertTrue(result.isPresent(), "Should detect parenthesized S/E notation");
        assertEquals(3, result.get().season());
        assertEquals(4, result.get().episode());
    }

    @Test
    void testSeasonEpisodeLowercaseNotation() {
        String title = "Chez Krömer | Deniz Yücel (s06/e02)";
        Optional<SeasonEpisode> result = manager.parse("RBB", title);
        assertTrue(result.isPresent(), "Should detect lowercase s/e notation");
        assertEquals(6, result.get().season());
        assertEquals(2, result.get().episode());
    }

    @Test
    void testRatioOnlyPattern() {
        String title = "100xBrandenburg (1/4)";
        Optional<SeasonEpisode> result = manager.parse("RBB", title);
        assertTrue(result.isPresent(), "Should detect ratio-only pattern");
        assertEquals(1, result.get().season());
        assertEquals(4, result.get().episode());
    }

    @Test
    void testRatioOnlyLastEpisode() {
        String title = "100xBrandenburg (4/4)";
        Optional<SeasonEpisode> result = manager.parse("RBB", title);
        assertTrue(result.isPresent(), "Should detect last episode in ratio-only pattern");
        assertEquals(4, result.get().season());
        assertEquals(4, result.get().episode());
    }

    @Test
    void testNoMatchOnYearParentheses() {
        String title = "Brandenburg aktuell 08.07.2016 19:30";
        Optional<SeasonEpisode> result = manager.parse("RBB", title);
        assertFalse(result.isPresent(), "Date patterns should not be interpreted as season/episode");
    }

    @Test
    void testNoMatchWithoutMetadata() {
        String title = "20 Jahre - 20 Gärten: Lieblingsgarten & Familiengarten Mixdorf";
        Optional<SeasonEpisode> result = manager.parse("RBB", title);
        assertFalse(result.isPresent(), "Titles without S/E or ratio info should return empty");
    }
}