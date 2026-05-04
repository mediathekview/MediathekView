/*
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten

import mediathek.config.Daten
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.gui.messages.AboListChangedEvent
import mediathek.tool.Filter
import mediathek.tool.MessageBus
import java.util.*

class ListeAbo : ArrayList<DatenAbo>() {
    private var nr = 0

    fun addAbo(datenAbo: DatenAbo) {
        // die Änderung an der Liste wird nicht gemeldet!!
        // für das Lesen der Konfig-Datei beim Programmstart
        ++nr
        datenAbo.nr = nr
        if (datenAbo.name.isEmpty()) {
            // Downloads ohne "Aboname" sind manuelle Downloads
            datenAbo.name = "Abo_$nr"
        }

        add(datenAbo)
    }

    fun aboLoeschen(abo: DatenAbo) {
        remove(abo)
        aenderungMelden()
    }

    fun aenderungMelden() {
        // Filmliste anpassen
        setAboFuerFilm(Daten.getInstance().listeFilme, true)
        MessageBus.messageBus.publishAsync(AboListChangedEvent())
    }

    /**
     * Check if abo already exists in the list.
     * @param abo the new abo to be stored.
     * @return true if it already exists.
     */
    fun existsAlready(abo: DatenAbo): Boolean = any { datenAbo -> existingAboCovers(datenAbo, abo) }

    private fun existingAboCovers(existingAbo: DatenAbo, aboToCheck: DatenAbo): Boolean {
        // prüfen ob "existingAbo" das "aboToCheck" mit abdeckt, also die gleichen (oder mehr)
        // Filme findet, dann wäre das neue Abo hinfällig
        // Abos sollen sich nicht nur in der Länge unterscheiden
        return singleFieldCovers(existingAbo.sender, aboToCheck.sender) &&
            singleFieldCovers(existingAbo.thema, aboToCheck.thema) &&
            filterCoversAny(existingAbo.title, aboToCheck.title) &&
            filterCoversAny(existingAbo.themaTitel, aboToCheck.thema, aboToCheck.title) &&
            filterCoversAny(existingAbo.irgendwo, aboToCheck.thema, aboToCheck.title, aboToCheck.irgendwo)
    }

    private fun singleFieldCovers(existingValue: String, valueToCheck: String): Boolean =
        existingValue.isEmpty() || valueToCheck.equals(existingValue, ignoreCase = true)

    private fun filterCoversAny(existingFilter: String, vararg valuesToCheck: String): Boolean {
        val filter = existingFilter.lowercase(Locale.getDefault()).split(",").toTypedArray()
        if (filter.isEmpty()) {
            return true
        }

        return valuesToCheck.any { value -> Filter.pruefen(filter, value) }
    }

    fun getAboFuerFilm_schnell(film: DatenFilm, laengePruefen: Boolean): DatenAbo? {
        // da wird nur in der Filmliste geschaut, ob in "DatenFilm" ein Abo eingetragen ist
        // geht schneller, "getAboFuerFilm" muss aber vorher schon gelaufen sein!!
        val abo = film.abo ?: return null

        if (laengePruefen && !matchesLength(abo, film)) {
            return null
        }

        return abo
    }

    private fun matchesLength(abo: DatenAbo, film: DatenFilm): Boolean =
        Filter.laengePruefen(
            abo.mindestDauerMinuten,
            film.filmLength.toLong(),
            abo.filmLengthState == FilmLengthState.MINIMUM,
        )

    private fun deleteAboInFilm(film: DatenFilm) {
        // für jeden Film Abo löschen
        film.abo = null
    }

    private fun createAbo(abo: DatenAbo) {
        abo.titelFilterPattern = createFilterPattern(abo.title)
        abo.themaFilterPattern = createFilterPattern(abo.themaTitel)
        abo.irgendwoFilterPattern = createFilterPattern(abo.irgendwo)
    }

    private fun createFilterPattern(value: String): Array<String> =
        when {
            value.isEmpty() -> LEER
            Filter.isPattern(value) -> arrayOf(value)
            else -> value.lowercase(Locale.getDefault()).split(",").toTypedArray()
        }

    /**
     * Assign found active abo to the film objects.
     * Time-intensive procedure!
     *
     * @param film assignee
     */
    private fun assignAboToFilm(film: DatenFilm) {
        var textMatch: DatenAbo? = null

        for (abo in this) {
            if (!abo.isActive) {
                continue
            }

            if (!Filter.filterAufFilmPruefen(
                    abo.sender,
                    abo.thema,
                    abo.titelFilterPattern,
                    abo.themaFilterPattern,
                    abo.irgendwoFilterPattern,
                    film,
                )
            ) {
                continue
            }

            if (textMatch == null) {
                textMatch = abo
            }

            if (matchesLength(abo, film)) {
                film.abo = abo
                return
            }
        }

        if (textMatch == null) {
            deleteAboInFilm(film)
        } else {
            film.abo = textMatch
        }
    }

    /**
     * Hier wird tatsächlich für jeden Film die Liste der Abos durchsucht.
     * Braucht länger.
     * @param listeFilme Die Filmliste
     * @param aboLoeschen abo löschen?
     */
    fun setAboFuerFilm(listeFilme: ListeFilme, aboLoeschen: Boolean) {
        if (isEmpty() && aboLoeschen) {
            listeFilme.forEach { film -> deleteAboInFilm(film) }
            return
        }

        // leere Abos löschen, die sind Fehler
        removeIf { datenAbo -> datenAbo.isInvalid }

        // und jetzt erstellen
        forEach { datenAbo -> createAbo(datenAbo) }

        // das kostet die Zeit!!
        listeFilme.parallelStream().forEach { film -> assignAboToFilm(film) }

        // und jetzt wieder löschen
        forEach { datenAbo ->
            datenAbo.titelFilterPattern = LEER
            datenAbo.themaFilterPattern = LEER
            datenAbo.irgendwoFilterPattern = LEER
        }
    }

    private companion object {
        private val LEER = arrayOf("")
    }
}
