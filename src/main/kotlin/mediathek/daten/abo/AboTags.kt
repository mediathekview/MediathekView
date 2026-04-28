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

import java.util.*

enum class AboTags(val index: Int, val xmlName: String) {
    NR(DatenAbo.ABO_NR, "Nr"),
    EINGESCHALTET(DatenAbo.ABO_EINGESCHALTET, "aktiv"),
    NAME(DatenAbo.ABO_NAME, "Name"),
    SENDER(DatenAbo.ABO_SENDER, "Sender"),
    THEMA(DatenAbo.ABO_THEMA, "Thema"),
    TITEL(DatenAbo.ABO_TITEL, "Titel"),
    THEMA_TITEL(DatenAbo.ABO_THEMA_TITEL, "Thema-Titel"),
    IRGENDWO(DatenAbo.ABO_IRGENDWO, "Irgendwo"),
    MINDESTDAUER(DatenAbo.ABO_MINDESTDAUER, "Mindestdauer"),
    MIN(DatenAbo.ABO_MIN, "min_max"),
    ZIELPFAD(DatenAbo.ABO_ZIELPFAD, "Zielpfad"),
    DOWN_DATUM(DatenAbo.ABO_DOWN_DATUM, "letztes_Abo"),
    PSET(DatenAbo.ABO_PSET, "Programmset"),
    DO_NOT_START_AUTOMATICALLY(DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY, "nicht_automatisch_starten");

    companion object {
        @JvmStatic
        fun fromXmlTag(tag: String): Optional<AboTags> {
            return Arrays.stream(entries.toTypedArray()).filter { e: AboTags -> e.xmlName == tag }.findAny()
        }

        fun fromIndex(index: Int): Optional<AboTags> {
            return Arrays.stream(entries.toTypedArray()).filter { e: AboTags -> e.index == index }.findAny()
        }
    }
}
