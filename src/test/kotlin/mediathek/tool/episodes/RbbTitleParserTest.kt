/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.tool.episodes

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test

class RbbTitleParserTest {
    @Test
    fun testSeasonEpisodeSlashNotation() {
        val title = "Chez Krömer | Boris Palmer (S03/E04)"
        val result = manager.parse("RBB", title)
        assertTrue(result.isPresent, "Should detect parenthesized S/E notation")
        assertEquals(3, result.get().season)
        assertEquals(4, result.get().episode)
    }

    @Test
    fun testSeasonEpisodeLowercaseNotation() {
        val title = "Chez Krömer | Deniz Yücel (s06/e02)"
        val result = manager.parse("RBB", title)
        assertTrue(result.isPresent, "Should detect lowercase s/e notation")
        assertEquals(6, result.get().season)
        assertEquals(2, result.get().episode)
    }

    @Test
    fun testRatioOnlyPattern() {
        val title = "100xBrandenburg (1/4)"
        val result = manager.parse("RBB", title)
        assertTrue(result.isPresent, "Should detect ratio-only pattern")
        assertEquals(1, result.get().season)
        assertEquals(4, result.get().episode)
    }

    @Test
    fun testRatioOnlyLastEpisode() {
        val title = "100xBrandenburg (4/4)"
        val result = manager.parse("RBB", title)
        assertTrue(result.isPresent, "Should detect last episode in ratio-only pattern")
        assertEquals(4, result.get().season)
        assertEquals(4, result.get().episode)
    }

    @Test
    fun testNoMatchOnYearParentheses() {
        val title = "Brandenburg aktuell 08.07.2016 19:30"
        val result = manager.parse("RBB", title)
        assertFalse(result.isPresent, "Date patterns should not be interpreted as season/episode")
    }

    @Test
    fun testNoMatchWithoutMetadata() {
        val title = "20 Jahre - 20 Gärten: Lieblingsgarten & Familiengarten Mixdorf"
        val result = manager.parse("RBB", title)
        assertFalse(result.isPresent, "Titles without S/E or ratio info should return empty")
    }

    companion object {
        private lateinit var manager: TitleParserManager

        @JvmStatic
        @BeforeAll
        fun setup() {
            manager = TitleParserManager()
            manager.register(
                "RBB",
                // 1) Parenthesized S/E pattern, e.g. (S05/E01)
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 2) Parenthesized ratio-only pattern, e.g. (1/4)
                "\\(\\s*(?<season>\\d{1,2})/(?<episode>\\d{1,2})\\s*\\)",
            )
        }
    }
}
