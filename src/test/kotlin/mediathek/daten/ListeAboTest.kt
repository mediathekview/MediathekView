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

package mediathek.daten

import mediathek.daten.abo.DatenAbo
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test

class ListeAboTest {
    @Test
    fun inactiveAboDoesNotShadowLaterActiveMatch() {
        val inactiveBroadAbo = DatenAbo().apply {
            sender = "ZDF"
            isActive = false
        }
        val activeSpecificAbo = DatenAbo().apply {
            sender = "ZDF"
            title = "Heute Journal"
            isActive = true
        }
        val abos = ListeAbo().apply {
            addAbo(inactiveBroadAbo)
            addAbo(activeSpecificAbo)
        }
        val film = DatenFilm().apply {
            sender = "ZDF"
            thema = "Nachrichten"
            title = "Heute Journal"
        }

        abos.setAboFuerFilm(ListeFilme().apply { add(film) }, true)

        assertSame(activeSpecificAbo, abos.getAboFuerFilm_schnell(film, false))
    }
}
