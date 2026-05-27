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

package mediathek.gui.duplicates

import mediathek.daten.DatenFilm

class BigSenderPenaltyComparator : Comparator<DatenFilm> {
    override fun compare(first: DatenFilm, second: DatenFilm): Int {
        // "ARD" und "ZDF" immer am Ende um die kleineren Mediatheken nicht zu benachteiligen.
        val firstPenalty = penalty(first.sender)
        val secondPenalty = penalty(second.sender)
        if (firstPenalty != secondPenalty) {
            return firstPenalty.compareTo(secondPenalty)
        }

        // Alphabetisch sortieren für alle anderen.
        return first.compareTo(second)
    }

    private fun penalty(sender: String): Int = if (sender in PENALIZED_SENDERS) 1 else 0

    private companion object {
        private val PENALIZED_SENDERS = setOf("ARD", "ZDF")
    }
}
