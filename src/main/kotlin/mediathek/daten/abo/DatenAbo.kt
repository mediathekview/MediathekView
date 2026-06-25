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

package mediathek.daten.abo

import mediathek.tool.GermanStringSorter
import java.time.LocalDate

class DatenAbo : Comparable<DatenAbo> {
    var mindestDauerMinuten: Int = 0
        set(value) {
            field = value.coerceAtLeast(0)
        }

    /**
     * Stores the active state of the abo.
     * On by default.
     */
    var isActive: Boolean = true

    /**
     * The display name.
     */
    var name: String = ""
    var sender: String = ""
    var thema: String = ""
    var title: String = ""
    var themaTitel: String = ""
    var irgendwo: String = ""
    var zielpfad: String = ""
    var downloadDate: LocalDate? = null
    var psetName: String = ""
    var isDoNotStartAutomatically: Boolean = false

    /**
     * Whether or not to use minimum film length or maximum film length.
     */
    var filmLengthState: FilmLengthState = FilmLengthState.MINIMUM

    val isInvalid: Boolean
        get() = isInvalidFilter(sender, thema, title, themaTitel, irgendwo)

    override fun compareTo(other: DatenAbo): Int =
        GermanStringSorter.compare(name, other.name)

    companion object {
        const val ABO_EINGESCHALTET: Int = 0
        const val ABO_NAME: Int = 1
        const val ABO_SENDER: Int = 2
        const val ABO_THEMA: Int = 3
        const val ABO_TITEL: Int = 4
        const val ABO_THEMA_TITEL: Int = 5
        const val ABO_IRGENDWO: Int = 6
        const val ABO_MINDESTDAUER: Int = 7
        const val ABO_MIN: Int = 8
        const val ABO_ZIELPFAD: Int = 9
        const val ABO_DOWN_DATUM: Int = 10
        const val ABO_PSET: Int = 11
        const val ABO_DO_NOT_START_AUTOMATICALLY: Int = 12
        const val ABO_FILM_COUNT: Int = 13
        const val MAX_ELEM: Int = 14

        fun isInvalidFilter(sender: String, thema: String, title: String, themaTitel: String, irgendwo: String): Boolean =
            sender.isEmpty() && thema.isEmpty() && title.isEmpty() && themaTitel.isEmpty() && irgendwo.isEmpty()
    }
}
