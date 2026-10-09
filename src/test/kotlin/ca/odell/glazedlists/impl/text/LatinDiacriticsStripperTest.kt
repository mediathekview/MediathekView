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
package ca.odell.glazedlists.impl.text

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.text.Normalizer

internal class LatinDiacriticsStripperTest {
    @Test
    fun mapperMatchesUnicodeCanonicalDecomposition() {
        val mapper = LatinDiacriticsStripper.getMapper()

        assertEquals(592, mapper.size)
        mapper.indices.forEach { index ->
            val decomposed = Normalizer.normalize(index.toChar().toString(), Normalizer.Form.NFD)
            assertEquals(decomposed[0], mapper[index], "mapping for U+${index.toString(16).padStart(4, '0')}")
        }
    }

    @Test
    fun mapperCannotBeChangedThroughReturnedArray() {
        val modified = LatinDiacriticsStripper.getMapper()
        val original = modified['é'.code]
        modified['é'.code] = 'x'

        assertNotEquals(original, modified['é'.code])
        assertEquals(original, LatinDiacriticsStripper.getMapper()['é'.code])
    }

    @Test
    fun matcherSetupReusesTheInternalMapper() {
        assertSame(LatinDiacriticsStripper.sharedMapper(), LatinDiacriticsStripper.sharedMapper())
    }
}
