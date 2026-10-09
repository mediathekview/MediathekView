/*
 * Copyright (c) 2026 derreisende77.
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
package ca.odell.glazedlists.impl.filter

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.parallel.ResourceLock
import java.util.*

@ResourceLock("defaultLocale")
internal class BoyerMooreCaseInsensitiveTextSearchStrategyTest {
    private val strategy = BoyerMooreCaseInsensitiveTextSearchStrategy()

    @Test
    fun searchRequiresConfiguredSubtext() {
        assertThrows(IllegalStateException::class.java) { strategy.indexOf("text") }
    }

    @Test
    fun emptySubtextMatchesAtBeginning() {
        strategy.setSubtext("")

        assertEquals(0, strategy.indexOf(""))
        assertEquals(0, strategy.indexOf("text"))
    }

    @Test
    fun subtextLongerThanTextDoesNotMatch() {
        strategy.setSubtext("longer")

        assertEquals(-1, strategy.indexOf("short"))
    }

    @Test
    fun lengthChangingCaseConversionDoesNotCreateFalsePositive() {
        strategy.setSubtext("Sß")

        assertEquals(-1, strategy.indexOf("sS"))
    }

    @Test
    fun unicodeCaseVariantsMatch() {
        strategy.setSubtext("ςA")

        assertEquals(0, strategy.indexOf("σA"))
    }

    @Test
    fun supplementaryUnicodeCaseVariantsMatch() {
        val supplementaryPrefix = String(Character.toChars(0x1F642))
        val deseretUpper = String(Character.toChars(0x10400))
        val deseretLower = String(Character.toChars(0x10428))
        strategy.setSubtext(deseretUpper + "A")

        assertEquals(2, strategy.indexOf(supplementaryPrefix + deseretLower + "A"))
    }

    @Test
    fun shiftTableCollisionsDoNotSkipMatches() {
        val firstCollisionCharacter = '\u0001'
        val secondCollisionCharacter = '\u0101'
        assertEquals(firstCollisionCharacter.code % 256, secondCollisionCharacter.code % 256)
        strategy.setSubtext("$firstCollisionCharacter-x$secondCollisionCharacter")

        assertEquals(
            3,
            strategy.indexOf("ab\u0201$firstCollisionCharacter-x$secondCollisionCharacter"),
        )
    }

    @Test
    fun normalizedSubtextIsNotMappedTwice() {
        val characterMap = CharArray('c'.code + 1) { it.toChar() }
        characterMap['a'.code] = 'b'
        characterMap['b'.code] = 'c'
        strategy.setCharacterMap(characterMap)
        strategy.setSubtext("b")

        assertEquals(0, strategy.indexOf("a"))

        strategy.setCharacterMap(null)
        assertEquals(0, strategy.indexOf("b"))
    }

    @Test
    fun matchingDoesNotDependOnDefaultLocale() {
        val originalLocale = Locale.getDefault()
        try {
            Locale.setDefault(Locale.forLanguageTag("tr-TR"))
            strategy.setSubtext("title")

            assertEquals(0, strategy.indexOf("TITLE"))
        } finally {
            Locale.setDefault(originalLocale)
        }
    }
}
