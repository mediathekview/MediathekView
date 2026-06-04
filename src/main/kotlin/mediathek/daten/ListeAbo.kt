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

    private fun createAboMatcher(abo: DatenAbo): CompiledAboMatcher =
        CompiledAboMatcher(
            abo = abo,
            titelFilterPattern = createFilterPattern(abo.title),
            themaFilterPattern = createFilterPattern(abo.themaTitel),
            irgendwoFilterPattern = createFilterPattern(abo.irgendwo),
        )

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
    private fun assignAboToFilm(film: DatenFilm, aboMatchers: List<CompiledAboMatcher>) {
        var textMatch: DatenAbo? = null
        val filmText = FilmAboMatchText(film)

        for (matcher in aboMatchers) {
            val abo = matcher.abo
            if (!matcher.matches(filmText)) {
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

        val aboMatchers = asSequence()
            .filter { datenAbo -> datenAbo.isActive }
            .map { datenAbo -> createAboMatcher(datenAbo) }
            .toList()

        if (aboMatchers.isEmpty()) {
            listeFilme.forEach { film -> deleteAboInFilm(film) }
            return
        }

        // das kostet die Zeit!!
        listeFilme.parallelStream().forEach { film -> assignAboToFilm(film, aboMatchers) }
    }

    private companion object {
        private val LEER = arrayOf("")
    }

    private class CompiledAboMatcher(
        val abo: DatenAbo,
        val titelFilterPattern: Array<String>,
        val themaFilterPattern: Array<String>,
        val irgendwoFilterPattern: Array<String>,
    ) {
        fun matches(filmText: FilmAboMatchText): Boolean =
            senderConditionExists(filmText) &&
                themaConditionExists(filmText) &&
                filterMatches(titelFilterPattern, filmText.title) { filmText.titleLowercase } &&
                (filterMatches(themaFilterPattern, filmText.thema) { filmText.themaLowercase } ||
                    filterMatches(themaFilterPattern, filmText.title) { filmText.titleLowercase }) &&
                (filterMatches(irgendwoFilterPattern, filmText.description) { filmText.descriptionLowercase } ||
                    filterMatches(irgendwoFilterPattern, filmText.thema) { filmText.themaLowercase } ||
                    filterMatches(irgendwoFilterPattern, filmText.title) { filmText.titleLowercase })

        private fun senderConditionExists(filmText: FilmAboMatchText): Boolean =
            abo.sender.isEmpty() || filmText.sender.compareTo(abo.sender) == 0

        private fun themaConditionExists(filmText: FilmAboMatchText): Boolean =
            abo.thema.isEmpty() || filmText.thema.equals(abo.thema, ignoreCase = true)

        private fun filterMatches(filter: Array<String>, text: String, lowercaseText: () -> String): Boolean {
            val firstFilter = filter[0]
            if (filter.size == 1) {
                if (firstFilter.isEmpty()) {
                    return true
                }

                Filter.makePattern(firstFilter)?.let { pattern ->
                    return pattern.matcher(text).matches()
                }
            }

            return Filter.checkLowercase(filter, lowercaseText())
        }
    }

    private class FilmAboMatchText(film: DatenFilm) {
        val sender: String = film.sender
        val thema: String = film.thema
        val title: String = film.title
        val description: String = film.description
        private var themaLowercaseValue: String? = null
        private var titleLowercaseValue: String? = null
        private var descriptionLowercaseValue: String? = null

        val themaLowercase: String
            get() = themaLowercaseValue ?: thema.lowercase(Locale.getDefault()).also { themaLowercaseValue = it }

        val titleLowercase: String
            get() = titleLowercaseValue ?: title.lowercase(Locale.getDefault()).also { titleLowercaseValue = it }

        val descriptionLowercase: String
            get() = descriptionLowercaseValue ?: description.lowercase(Locale.getDefault()).also { descriptionLowercaseValue = it }
    }
}
