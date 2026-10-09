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

import mediathek.config.Konstanten
import mediathek.tool.GermanStringSorter
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeSupport
import java.util.*
import java.util.concurrent.ConcurrentHashMap

open class ListeFilme : ArrayList<DatenFilm>() {
    protected val pcs: PropertyChangeSupport = PropertyChangeSupport(this)

    var metaData: FilmListMetaData = FilmListMetaData()
        set(meta) {
            val oldValue = field
            field = meta
            pcs.firePropertyChange(PCS_METADATA, oldValue, field)
        }

    fun addMetaDataChangeListener(listener: PropertyChangeListener) {
        pcs.addPropertyChangeListener(PCS_METADATA, listener)
    }

    fun snapshot(): List<DatenFilm> =
        synchronized(this) {
            Collections.unmodifiableList(ArrayList(this))
        }

    /**
     * Search all themas within list based on sender.
     * If sender is empty, return full list of themas.
     */
    fun getThemen(sender: String): List<String> {
        var stream = snapshot().parallelStream()
        if (sender.isNotEmpty()) {
            stream = stream.filter { film -> film.sender == sender }
        }

        val seenThemen = ConcurrentHashMap.newKeySet<String>()
        return stream
            .map { film -> film.thema }
            .filter { thema -> seenThemen.add(normalizeKey(thema)) }
            .sorted(GermanStringSorter)
            .toList()
    }

    /**
     * Search all distinct themas within list based on senders.
     * If senders is empty, return the full distinct thema list.
     */
    fun getThemen(senders: Collection<String>): List<String> {
        val snapshot = snapshot()
        val normalizedSenders = senders.mapTo(HashSet(), ::normalizeKey)
        val seenThemen = HashSet<String>()
        val result = ArrayList<String>()
        val includeAllSenders = normalizedSenders.isEmpty()

        for (film in snapshot) {
            if (!includeAllSenders && normalizeKey(film.sender) !in normalizedSenders) {
                continue
            }

            val thema = film.thema
            if (seenThemen.add(normalizeKey(thema))) {
                result.add(thema)
            }
        }

        result.sortWith(GermanStringSorter)
        return Collections.unmodifiableList(result)
    }

    @Synchronized
    fun updateFromFilmList(newFilmsList: ListeFilme) {
        val newFilmIdentities = HashSet<DatenFilm.FilmIdentity>(newFilmsList.size + 1, 1f)
        newFilmsList.forEach { newFilm -> newFilmIdentities.add(newFilm.filmIdentity) }

        removeIf { currentFilm -> currentFilm.filmIdentity in newFilmIdentities }
        newFilmIdentities.clear()

        newFilmsList.forEach { film ->
            film.init()
            add(film)
        }
    }

    @Synchronized
    fun getFilmByUrl_klein_hoch_hd(url: String): DatenFilm? =
        firstOrNull { film ->
            film.urlNormalQuality == url ||
                film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY) == url ||
                film.getUrlFuerAufloesung(FilmResolution.Enum.LOW) == url
        }

    /**
     * List needs update when it is either empty or too old.
     */
    fun needsUpdate(): Boolean =
        isEmpty() || metaData.isOlderThan(Konstanten.ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE)

    @Synchronized
    fun countNewFilms(): Long =
        stream().filter(DatenFilm::isNew).count()

    companion object {
        private const val PCS_METADATA = "metaData"

        private fun normalizeKey(value: String?): String =
            value?.lowercase(Locale.ROOT).orEmpty()
    }
}
