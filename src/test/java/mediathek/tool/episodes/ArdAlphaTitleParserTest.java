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

public class ArdAlphaTitleParserTest {
    private static TitleParserManager manager;

    @BeforeAll
    static void setup() {
        manager = new TitleParserManager();
        manager.register("ARD-alpha",
                // 1) Parenthesized "(S01/E04)"
                "\\(\\s*[sS](?<season>\\d{2})/[eE](?<episode>\\d{2})\\s*\\)",
                // 2) Un-parenthesized "S05/E03"
                "[sS](?<season>\\d{2})/[eE](?<episode>\\d{2})",
                // 3) Continuous "S05E03"
                "[sS](?<season>\\d{2})[eE](?<episode>\\d{2})"
        );
    }

    @Test
    void testParenthesizedSlashNotation_CRISPR() {
        String title = "CRISPR - Wie Gentechnik unser Leben verändert (S01/E04)";
        Optional<SeasonEpisode> result = manager.parse("ARD-alpha", title);
        assertTrue(result.isPresent(), "Should detect parenthesized S01/E04");
        assertEquals(1, result.get().season());
        assertEquals(4, result.get().episode());
    }

    @Test
    void testParenthesizedSlashNotation_Holzanbau() {
        String title = "Ein Holzanbau mit Aussicht (S05/E03)";
        Optional<SeasonEpisode> result = manager.parse("ARD-alpha", title);
        assertTrue(result.isPresent(), "Should detect parenthesized S05/E03");
        assertEquals(5, result.get().season());
        assertEquals(3, result.get().episode());
    }

    @Test
    void testUnparenthesizedSlashNotation() {
        String title = "Ein Holzanbau mit Aussicht S05/E03";
        Optional<SeasonEpisode> result = manager.parse("ARD-alpha", title);
        assertTrue(result.isPresent(), "Should detect un-parenthesized S05/E03");
        assertEquals(5, result.get().season());
        assertEquals(3, result.get().episode());
    }

    @Test
    void testContinuousNotation() {
        String title = "Ein Holzanbau mit Aussicht S05E03 Dokumentation";
        Optional<SeasonEpisode> result = manager.parse("ARD-alpha", title);
        assertTrue(result.isPresent(), "Should detect continuous S05E03");
        assertEquals(5, result.get().season());
        assertEquals(3, result.get().episode());
    }

    @Test
    void testNoMatchReturnsEmpty() {
        String title = "Die Sex-Lüge";
        Optional<SeasonEpisode> result = manager.parse("ARD-alpha", title);
        assertFalse(result.isPresent(), "Should not detect any season/episode");
    }
}