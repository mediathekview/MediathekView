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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import mediathek.daten.abo.AboFilmAssignmentService
import mediathek.daten.abo.DatenAbo
import mediathek.tool.Filter
import mediathek.tool.withReadLock
import mediathek.tool.withWriteLock
import java.util.*

class ListeAbo(
    private val onChanged: (() -> Unit)? = null,
    private val entries: BasicEventList<DatenAbo> = BasicEventList(),
) : EventList<DatenAbo> by entries {
    private val filmAssignmentService = AboFilmAssignmentService()

    fun addAbo(datenAbo: DatenAbo) {
        if (addAboSortedWithoutNotification(datenAbo)) {
            notifyChanged()
        }
    }

    internal fun addAboWithoutNotification(datenAbo: DatenAbo): Boolean =
        addAboSortedWithoutNotification(datenAbo)

    internal fun addAboFromConfig(datenAbo: DatenAbo) {
        entries.withWriteLock {
            prepareAboForAdd(datenAbo)
            add(datenAbo)
        }
    }

    private fun addAboSortedWithoutNotification(datenAbo: DatenAbo): Boolean =
        entries.withWriteLock {
            prepareAboForAdd(datenAbo)
            add(datenAbo)
            sort()
            true
        }

    fun aboLoeschen(abo: DatenAbo) {
        if (removeAboWithoutNotification(abo)) {
            notifyChanged()
        }
    }

    internal fun removeAboWithoutNotification(abo: DatenAbo): Boolean =
        entries.withWriteLock {
            remove(abo)
        }

    internal fun removeAbosWithoutNotification(abos: Collection<DatenAbo>): Boolean =
        entries.withWriteLock {
            removeAll(abos.toSet())
        }

    internal fun finishLoading() {
        entries.withWriteLock {
            sort()
        }
    }

    private fun prepareAboForAdd(datenAbo: DatenAbo) {
        if (datenAbo.name.isEmpty()) {
            // Downloads ohne "Aboname" sind manuelle Downloads
            datenAbo.name = nextFallbackName()
        }
    }

    private fun nextFallbackName(): String {
        var index = size + 1
        var name = "Abo_$index"
        while (any { abo -> abo.name == name }) {
            index++
            name = "Abo_$index"
        }
        return name
    }

    fun fireAboChanged(abo: DatenAbo) {
        entries.withWriteLock {
            val index = indexOf(abo)
            if (index != -1) {
                this[index] = abo
            }
        }
    }

    internal fun fireAbosChanged(abos: Collection<DatenAbo>) {
        entries.withWriteLock {
            for (abo in abos) {
                val index = indexOf(abo)
                if (index != -1) {
                    this[index] = abo
                }
            }
        }
    }

    private fun notifyChanged() {
        onChanged?.invoke()
    }

    /**
     * Check if abo already exists in the list.
     * @param abo the new abo to be stored.
     * @return true if it already exists.
     */
    fun existsAlready(abo: DatenAbo): Boolean =
        entries.withReadLock {
            any { datenAbo -> existingAboCovers(datenAbo, abo) }
        }

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

    fun getAboForFilmFast(film: DatenFilm, laengePruefen: Boolean): DatenAbo? {
        // da wird nur in der Filmliste geschaut, ob in "DatenFilm" ein Abo eingetragen ist
        return filmAssignmentService.findAboForFilm(film, laengePruefen)
    }

    /**
     * Hier wird tatsächlich für jeden Film die Liste der Abos durchsucht.
     * Braucht länger.
     * @param listeFilme Die Filmliste
     * @param aboLoeschen abo löschen?
     */
    fun setAboFuerFilm(listeFilme: ListeFilme, aboLoeschen: Boolean) {
        filmAssignmentService.assignAbosToFilms(assignmentSnapshot(), listeFilme, aboLoeschen)
    }

    internal fun assignmentSnapshot(): List<DatenAbo> =
        entries.withWriteLock {
            // leere Abos löschen, die sind Fehler
            removeIf { datenAbo -> datenAbo.isInvalid }
            toList()
        }
}
