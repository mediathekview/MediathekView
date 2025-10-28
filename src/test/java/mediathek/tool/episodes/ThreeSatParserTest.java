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

public class ThreeSatParserTest {
    private static TitleParserManager manager;

    @BeforeAll
    static void setup() {
        manager = new TitleParserManager();
        manager.register("3Sat",
                // 1) Parenthesized "(S2025/E15)"
                "\\(\\s*[sS](?<season>\\d{4})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 2) Un-parenthesized "S2025/E15"
                "[sS](?<season>\\d{4})/[eE](?<episode>\\d{1,2})",
                // 3) Continuous "S2024E81"
                "[sS](?<season>\\d{4})[eE](?<episode>\\d{1,2})",
                // 4) Parenthesized hyphen "(S2025-E15)"
                "\\(\\s*[sS](?<season>\\d{4})-[eE](?<episode>\\d{1,2})\\s*\\)"
        );
    }

    @Test
    void testParenthesizedSlashNotation() {
        String title = "37°: Kreative Köpfe (S2025/E11)";
        Optional<SeasonEpisode> result = manager.parse("3Sat", title);
        assertTrue(result.isPresent(), "Should detect parenthesized S/E slash notation");
        assertEquals(2025, result.get().season());
        assertEquals(11, result.get().episode());
    }

    @Test
    void testUnparenthesizedSlashNotation() {
        String title = "37°: Kreative Köpfe S2025/E11";
        Optional<SeasonEpisode> result = manager.parse("3Sat", title);
        assertTrue(result.isPresent(), "Should detect un-parenthesized S/E slash notation");
        assertEquals(2025, result.get().season());
        assertEquals(11, result.get().episode());
    }

    @Test
    void testContinuousNotation() {
        String title = "Kulturzeit S2024E81";
        Optional<SeasonEpisode> result = manager.parse("3Sat", title);
        assertTrue(result.isPresent(), "Should detect continuous S/E notation");
        assertEquals(2024, result.get().season());
        assertEquals(81, result.get().episode());
    }

    @Test
    void testParenthesizedHyphenNotation() {
        String title = "Sendung (S2023-E05) Spezial";
        Optional<SeasonEpisode> result = manager.parse("3Sat", title);
        assertTrue(result.isPresent(), "Should detect parenthesized S-E hyphen notation");
        assertEquals(2023, result.get().season());
        assertEquals(5, result.get().episode());
    }

    @Test
    void testNoMatchReturnsEmpty() {
        String title = "3Sat Nachrichten";
        Optional<SeasonEpisode> result = manager.parse("3Sat", title);
        assertFalse(result.isPresent(), "Should not detect any season/episode");
    }
}