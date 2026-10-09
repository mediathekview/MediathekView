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

import java.text.Normalizer

/**
 * Maps the Basic Latin, Latin-1 Supplement, Latin Extended-A, and Latin Extended-B
 * code points to the first character of their Unicode canonical decomposition.
 *
 * The resulting lookup table strips diacritics without normalizing text on the
 * matching hot path. For example, the mapped value of `é` is `e`.
 */
internal class LatinDiacriticsStripper {
    companion object {
        private const val MAPPER_SIZE = 592

        private val MAPPER = CharArray(MAPPER_SIZE) { codePoint ->
            Normalizer.normalize(codePoint.toChar().toString(), Normalizer.Form.NFD)[0]
        }

        /** Returns an independent snapshot of the Latin-character mapping table. */
        fun getMapper(): CharArray = MAPPER.copyOf()

        /** Returns the shared read-only-by-convention table for matcher setup. */
        @JvmSynthetic
        internal fun sharedMapper(): CharArray = MAPPER
    }
}
