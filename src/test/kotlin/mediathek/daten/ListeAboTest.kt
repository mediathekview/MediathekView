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

import ca.odell.glazedlists.event.ListEvent
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class ListeAboTest {
    @Test
    fun addAboAssignsFallbackName() {
        val abos = ListeAbo()
        val unnamedAbo = DatenAbo()

        abos.addAboWithoutNotification(unnamedAbo)

        assertEquals("Abo_1", unnamedAbo.name)
        assertSame(unnamedAbo, abos[0])
    }

    @Test
    fun fallbackNameUsesNextFreeAboName() {
        val abos = ListeAbo().apply {
            addAboWithoutNotification(DatenAbo().apply { name = "Abo_1" })
        }
        val unnamedAbo = DatenAbo()

        abos.addAboWithoutNotification(unnamedAbo)

        assertEquals("Abo_2", unnamedAbo.name)
    }

    @Test
    fun removeAbosWithoutNotificationRemovesEntriesThroughEventList() {
        val first = DatenAbo().apply { name = "first" }
        val second = DatenAbo().apply { name = "second" }
        val abos = ListeAbo().apply {
            addAboWithoutNotification(first)
            addAboWithoutNotification(second)
        }

        val removed = abos.removeAbosWithoutNotification(listOf(first))

        assertTrue(removed)
        assertEquals(1, abos.size)
        assertSame(second, abos[0])
    }

    @Test
    fun addAboKeepsListSortedByName() {
        val abos = ListeAbo().apply {
            addAboWithoutNotification(DatenAbo().apply { name = "Zebra" })
            addAboWithoutNotification(DatenAbo().apply { name = "Alpha" })
        }

        assertEquals("Alpha", abos[0].name)
        assertEquals("Zebra", abos[1].name)
    }

    @Test
    fun configLoadingPreservesReadOrderUntilFinished() {
        val abos = ListeAbo().apply {
            addAboFromConfig(DatenAbo().apply { name = "Zebra" })
            addAboFromConfig(DatenAbo().apply { name = "Alpha" })
        }

        assertEquals("Zebra", abos[0].name)
        assertEquals("Alpha", abos[1].name)

        abos.finishLoading()

        assertEquals("Alpha", abos[0].name)
        assertEquals("Zebra", abos[1].name)
    }

    @Test
    fun fireAboChangedPublishesUpdateEvent() {
        val abo = DatenAbo().apply { name = "Alpha" }
        val abos = ListeAbo().apply { addAboWithoutNotification(abo) }
        var updateEvents = 0
        abos.addListEventListener { event ->
            while (event.next()) {
                if (event.type == ListEvent.UPDATE) {
                    updateEvents++
                }
            }
        }

        abo.name = "Beta"
        abos.fireAboChanged(abo)

        assertEquals(1, updateEvents)
    }

    @Test
    fun fireAboChangedIgnoresUnknownAbo() {
        val abos = ListeAbo()

        abos.fireAboChanged(DatenAbo())

        assertEquals(0, abos.size)
    }

    @Test
    fun addAboInvokesChangeCallback() {
        var changes = 0
        val abos = ListeAbo(onChanged = { changes++ })

        abos.addAbo(DatenAbo().apply { name = "Alpha" })

        assertEquals(1, changes)
    }

    @Test
    fun aboLoeschenInvokesChangeCallbackWhenAboWasRemoved() {
        var changes = 0
        val abo = DatenAbo().apply { name = "Alpha" }
        val abos = ListeAbo(onChanged = { changes++ }).apply {
            addAboWithoutNotification(abo)
        }

        abos.aboLoeschen(abo)

        assertEquals(1, changes)
    }

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
            addAboWithoutNotification(inactiveBroadAbo)
            addAboWithoutNotification(activeSpecificAbo)
        }
        val film = DatenFilm().apply {
            sender = "ZDF"
            thema = "Nachrichten"
            title = "Heute Journal"
        }

        abos.setAboFuerFilm(ListeFilme().apply { add(film) }, true)

        assertSame(activeSpecificAbo, abos.getAboForFilmFast(film, false))
    }

    @Test
    fun inactiveOnlyAboListClearsExistingFilmAbo() {
        val inactiveAbo = DatenAbo().apply {
            sender = "ZDF"
            isActive = false
        }
        val abos = ListeAbo().apply { addAboWithoutNotification(inactiveAbo) }
        val film = DatenFilm().apply {
            sender = "ZDF"
            thema = "Nachrichten"
            title = "Heute Journal"
            abo = inactiveAbo
        }

        abos.setAboFuerFilm(ListeFilme().apply { add(film) }, true)

        assertNull(abos.getAboForFilmFast(film, false))
    }

    @Test
    fun lengthValidAboIsPreferredOverEarlierTextOnlyMatch() {
        val tooLongMinimumAbo = DatenAbo().apply {
            sender = "ZDF"
            title = "Heute Journal"
            mindestDauerMinuten = 60
            filmLengthState = FilmLengthState.MINIMUM
        }
        val validMinimumAbo = DatenAbo().apply {
            sender = "ZDF"
            title = "Heute Journal"
            mindestDauerMinuten = 10
            filmLengthState = FilmLengthState.MINIMUM
        }
        val abos = ListeAbo().apply {
            addAboWithoutNotification(tooLongMinimumAbo)
            addAboWithoutNotification(validMinimumAbo)
        }
        val film = DatenFilm().apply {
            sender = "ZDF"
            thema = "Nachrichten"
            title = "Heute Journal"
            setFilmLengthSeconds(30 * 60)
        }

        abos.setAboFuerFilm(ListeFilme().apply { add(film) }, true)

        assertSame(validMinimumAbo, abos.getAboForFilmFast(film, true))
    }

    @Test
    fun regexTitleAboStillMatchesFilmTitle() {
        val regexAbo = DatenAbo().apply {
            sender = "ZDF"
            title = "#:.*heute journal.*"
        }
        val abos = ListeAbo().apply { addAboWithoutNotification(regexAbo) }
        val film = DatenFilm().apply {
            sender = "ZDF"
            thema = "Nachrichten"
            title = "Heute Journal"
        }

        abos.setAboFuerFilm(ListeFilme().apply { add(film) }, true)

        assertSame(regexAbo, abos.getAboForFilmFast(film, false))
    }

    @Test
    fun irgendwoAboStillMatchesFilmDescription() {
        val descriptionAbo = DatenAbo().apply {
            sender = "ZDF"
            irgendwo = "wirtschaft"
        }
        val abos = ListeAbo().apply { addAboWithoutNotification(descriptionAbo) }
        val film = DatenFilm().apply {
            sender = "ZDF"
            thema = "Nachrichten"
            title = "Heute Journal"
            description = "Aktuelle Nachrichten aus Politik und Wirtschaft"
        }

        abos.setAboFuerFilm(ListeFilme().apply { add(film) }, true)

        assertSame(descriptionAbo, abos.getAboForFilmFast(film, false))
    }

    @Test
    fun earlierGlobalAboStillWinsBeforeLaterSenderSpecificAbo() {
        val globalAbo = DatenAbo().apply {
            title = "Heute Journal"
        }
        val zdfAbo = DatenAbo().apply {
            sender = "ZDF"
            title = "Heute Journal"
        }
        val abos = ListeAbo().apply {
            addAboWithoutNotification(globalAbo)
            addAboWithoutNotification(zdfAbo)
        }
        val film = DatenFilm().apply {
            sender = "ZDF"
            thema = "Nachrichten"
            title = "Heute Journal"
        }

        abos.setAboFuerFilm(ListeFilme().apply { add(film) }, true)

        assertSame(globalAbo, abos.getAboForFilmFast(film, false))
    }

    @Test
    fun earlierSenderSpecificAboStillWinsBeforeLaterGlobalAbo() {
        val zdfAbo = DatenAbo().apply {
            sender = "ZDF"
            title = "Heute Journal"
        }
        val globalAbo = DatenAbo().apply {
            title = "Heute Journal"
        }
        val abos = ListeAbo().apply {
            addAboWithoutNotification(zdfAbo)
            addAboWithoutNotification(globalAbo)
        }
        val film = DatenFilm().apply {
            sender = "ZDF"
            thema = "Nachrichten"
            title = "Heute Journal"
        }

        abos.setAboFuerFilm(ListeFilme().apply { add(film) }, true)

        assertSame(zdfAbo, abos.getAboForFilmFast(film, false))
    }
}
