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

package mediathek.controller.history

import mediathek.daten.DatenFilm
import mediathek.gui.messages.history.FilmSeenStateChangedEvent
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager

class FilmSeenHistoryController : AutoCloseable {
    private val controller = SeenHistoryController()

    fun markUnseen(film: DatenFilm) {
        if (controller.markUnseen(SeenHistorySource.FILM, film.urlNormalQuality)) {
            sendFilmSeenStateChanged(false, listOf(film))
        }
    }

    fun markUnseen(list: List<DatenFilm>) {
        val urls = list.asSequence()
            .map { it.urlNormalQuality }
            .filter(String::isNotBlank)
            .distinct()
            .toList()

        if (controller.markUnseen(SeenHistorySource.FILM, urls)) {
            sendFilmSeenStateChanged(false, list)
        }
    }

    fun markSeen(film: DatenFilm?) {
        if (film == null) {
            logger.warn("markSeen: no film found")
            return
        }

        val entry = film.toSeenHistoryEntry() ?: return
        if (controller.markSeen(entry)) {
            sendFilmSeenStateChanged(true, listOf(film))
        }
    }

    fun markSeen(list: List<DatenFilm>) {
        val candidates = list
            .asSequence()
            .mapNotNull { film -> film.toSeenHistoryEntry() }
            .distinctBy(SeenHistoryEntry::url)
            .toList()

        if (controller.markSeen(candidates)) {
            sendFilmSeenStateChanged(true, list)
        }
    }

    fun hasBeenSeen(film: DatenFilm): Boolean =
        controller.hasBeenSeen(SeenHistorySource.FILM, film.urlNormalQuality)

    fun prepareMemoryCache() {
        controller.prepareMemoryCache(SeenHistorySource.FILM)
    }

    fun isMemoryCachePrepared(): Boolean =
        controller.isMemoryCachePrepared(SeenHistorySource.FILM)

    override fun close() {
        controller.close()
    }

    private fun sendFilmSeenStateChanged(seen: Boolean, films: List<DatenFilm>) {
        if (films.isNotEmpty()) {
            MessageBus.messageBus.publishAsync(FilmSeenStateChangedEvent(seen, films))
        }
    }

    companion object {
        private val logger = LogManager.getLogger()

        fun prepareSharedMemoryCache() {
            FilmSeenHistoryController().use { it.prepareMemoryCache() }
        }

        fun hasBeenSeenFromSharedCache(film: DatenFilm): Boolean {
            if (!SeenHistoryCache.isPrepared(SeenHistorySource.FILM)) {
                prepareSharedMemoryCache()
            }
            return SeenHistoryCache.contains(SeenHistorySource.FILM, film.urlNormalQuality)
        }
    }
}

private fun DatenFilm.toSeenHistoryEntry(): SeenHistoryEntry? {
    if (isLivestream) {
        return null
    }

    val url = urlNormalQuality.takeIf(String::isNotBlank) ?: return null
    return SeenHistoryEntry(
        source = SeenHistorySource.FILM,
        theme = thema,
        title = title,
        url = url
    )
}
