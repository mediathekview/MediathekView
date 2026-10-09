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

class ArdParserTest {
    @Test
    fun testDotNotationSeasonEpisode() {
        val title = "15. Staffel, 188. Die alte Frau"
        val result = manager.parse("ARD", title)
        assertTrue(result.isPresent, "Should detect season and episode in dot-notation")
        assertEquals(15, result.get().season)
        assertEquals(188, result.get().episode)
    }

    @Test
    fun testStaffelFolgeFormat() {
        val title = "Sturm der Liebe, Staffel 1, Folge 1 (2005)"
        val result = manager.parse("ARD", title)
        assertTrue(result.isPresent, "Should detect Staffel/Folge format")
        assertEquals(1, result.get().season)
        assertEquals(1, result.get().episode)
    }

    @Test
    fun testParenthesizedSxxExxx() {
        val title = "Muster (S03E27352) Spezial"
        val result = manager.parse("ARD", title)
        assertTrue(result.isPresent, "Should detect parenthesized SxxExxx")
        assertEquals(3, result.get().season)
        assertEquals(27352, result.get().episode)
    }

    @Test
    fun testParenthesizedSxxSlashExxx() {
        val title = "Serie Highlight (S01/E02) Finale"
        val result = manager.parse("ARD", title)
        assertTrue(result.isPresent, "Should detect parenthesized Sxx/Exxx")
        assertEquals(1, result.get().season)
        assertEquals(2, result.get().episode)
    }

    @Test
    fun testInlineSxSlashExxx() {
        val title = "Unfall S3/E12 Dokumentation"
        val result = manager.parse("ARD", title)
        assertTrue(result.isPresent, "Should detect inline Sx/Exxx")
        assertEquals(3, result.get().season)
        assertEquals(12, result.get().episode)
    }

    @Test
    fun testNoMatchReturnsEmpty() {
        val title = "ZDF Heute Journal"
        val result = manager.parse("ARD", title)
        assertFalse(result.isPresent, "Should not detect any season/episode")
    }

    companion object {
        private lateinit var manager: TitleParserManager

        @JvmStatic
        @BeforeAll
        fun setup() {
            manager = TitleParserManager()
            manager.register(
                "ARD",
                // 1) "15. Staffel, 188. Die alte Frau"
                "(?<season>\\d{1,2})\\.\\s*Staffel,\\s*(?<episode>\\d{1,3})\\.",
                // 2) "Staffel 1, Folge 1"
                "Staffel\\s*(?<season>\\d{1,2})[,\\s]*Folge\\s*(?<episode>\\d{1,3})",
                // 3) "(S03E27352)"
                "\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,5})\\s*\\)",
                // 4) "(S01/E02)"
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})\\s*\\)",
                // 5) "S3/E12"
                "[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})",
            )
        }
    }
}
