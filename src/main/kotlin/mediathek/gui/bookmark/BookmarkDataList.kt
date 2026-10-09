/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.bookmark

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import mediathek.config.StandardLocations
import mediathek.controller.history.FilmSeenHistoryController
import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.gui.messages.BookmarkRefreshCompletedEvent
import mediathek.gui.messages.history.FilmSeenStateChangedEvent
import mediathek.tool.MessageBus
import mediathek.tool.withReadLock
import mediathek.tool.withWriteLock
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.time.LocalDate
import java.util.*

/**
 * Stores a full list of bookmarked movies.
 */
class BookmarkDataList(
    private val allFilms: ListeFilme,
) {
    private val bookmarks = BasicEventList<BookmarkData>()

    init {
        MessageBus.messageBus.subscribe(this)
    }

    /**
     * Remove all bookmarks and deassociate film data
     */
    fun clear() {
        bookmarks.withWriteLock {
            forEach { bookmark ->
                bookmark.datenFilmOptional.ifPresent { film -> film.bookmark = null }
            }
            clear()
        }
        saveToFile()
    }

    /**
     * Return data list for Bookmark window
     *
     * @return observable List
     */
    fun getEventList(): EventList<BookmarkData> = bookmarks

    fun removeBookmark(bookmark: BookmarkData) {
        bookmarks.withWriteLock {
            bookmark.datenFilmOptional.ifPresent { film -> film.bookmark = null }
            bookmark.datenFilm = null
            remove(bookmark)
        }
    }

    /**
     * Add given film(s) to List if not yet in list
     * otherwise remove them from list
     * Note: if one of the given films is not bookmarked all are bookmarked
     *
     * @param movies list of movies to be added
     */
    fun checkAndBookmarkMovies(movies: List<DatenFilm>) {
        val addList = ArrayList<DatenFilm>()
        val delList = LinkedHashSet<BookmarkData>()
        var add = false
        for (film in movies) {
            if (!film.isBookmarked) {
                add = true
                addList.add(film)
            } else {
                val bookmark = findBookmarkFromFilm(film)
                if (bookmark != null) {
                    delList.add(bookmark)
                }
            }
        }

        if (add) {
            // Check if history list is known.
            try {
                bookmarks.withWriteLock {
                    FilmSeenHistoryController().use { history ->
                        addList.forEach { movie ->
                            val bookmarkData = BookmarkData(movie)
                            movie.bookmark = bookmarkData // Link backwards
                            bookmarkData.seen = history.hasBeenSeen(movie)
                            bookmarkData.filmHashCode = movie.sha256
                            bookmarkData.bookmarkAdded = LocalDate.now()
                            bookmarks.add(bookmarkData)
                        }
                    }
                }
            } catch (ex: Exception) {
                logger.error("history produced error", ex)
            }
        } else {
            movies.forEach { movie -> movie.bookmark = null }
            bookmarks.withWriteLock {
                bookmarks.removeAll(delList)
            }
        }
    }

    /**
     * Load Bookmarklist from backup medium
     */
    fun loadFromFile() {
        val filePath = StandardLocations.getBookmarkFilePath()

        try {
            val bookmarkList = BookmarkJsonStore.read(filePath)
            bookmarks.withWriteLock {
                addAll(bookmarkList)
            }
            bookmarkList.clear()
        } catch (e: Exception) {
            logger.error("Could not read bookmarks from file {}, error {} => file ignored", filePath.toString(), e.message)
        }
    }

    @Synchronized
    fun saveToFile() {
        val filePath = StandardLocations.getBookmarkFilePath()

        try {
            bookmarks.withReadLock {
                BookmarkJsonStore.write(filePath, bookmarks)
            }
            logger.trace("Bookmarks written")
        } catch (e: Exception) {
            logger.error("Could not save bookmarks to {}", filePath, e)
        }
    }

    @Synchronized
    fun refreshFromCurrentFilmList() {
        try {
            updateBookMarksFromFilmList()
        } finally {
            MessageBus.messageBus.publishAsync(BookmarkRefreshCompletedEvent())
        }
    }

    fun refreshFromCurrentFilmListAsync() {
        Thread.ofVirtual().start(::refreshFromCurrentFilmList)
    }

    /**
     * Updates the seen state
     *
     * @param seen True if movies are seen
     * @param list List of movies
     */
    fun updateSeen(seen: Boolean, list: List<DatenFilm>) {
        list.asSequence()
            .filter(DatenFilm::isBookmarked)
            .forEach { movie ->
                movie.bookmark?.seen = seen
            }
    }

    fun updateSeen(seen: Boolean, film: DatenFilm) {
        if (film.isBookmarked) {
            film.bookmark?.seen = seen
        }
    }

    @Handler
    private fun handleFilmSeenStateChanged(event: FilmSeenStateChangedEvent) {
        updateSeen(event.seen, event.films)
    }

    /**
     * Find Bookmark from film object.
     * @param film the film object
     * @return the associated bookmark or null.
     */
    private fun findBookmarkFromFilm(film: DatenFilm): BookmarkData? {
        bookmarks.withReadLock {
            for (bookmark in bookmarks) {
                val bookmarkFilm = bookmark.datenFilm
                if (bookmarkFilm != null && bookmarkFilm == film) {
                    return bookmark
                }
            }
        }
        return null
    }

    /**
     * Updates the stored bookmark data reference with actual film list
     * and links the entries
     * Executed in background
     */
    private fun updateBookMarksFromFilmList() {
        val bookmarkSnapshot = bookmarks.withReadLock {
            if (isEmpty()) {
                return
            }
            ArrayList(bookmarks)
        }

        val filmSnapshot: List<DatenFilm> =
            synchronized(allFilms) {
                ArrayList(allFilms)
            }
        val requestedHashes = bookmarkSnapshot
            .asSequence()
            .mapNotNull { bookmark -> bookmark.filmHashCode }
            .toSet()
        val requestedUrls = bookmarkSnapshot
            .asSequence()
            .filter { bookmark -> bookmark.filmHashCode == null }
            .mapNotNull { bookmark -> bookmark.url?.lowercase(Locale.ROOT) }
            .toSet()
        val filmsByHash = createFilmHashIndex(filmSnapshot, requestedHashes)
        val filmsByUrl = createFilmUrlIndex(filmSnapshot, requestedUrls)

        bookmarks.withWriteLock {
            for (bookmark in bookmarkSnapshot) {
                if (bookmark !in bookmarks) {
                    continue
                }

                val hashCodeStr = bookmark.filmHashCode
                if (hashCodeStr != null) {
                    val film = filmsByHash[hashCodeStr]
                    assignData(bookmark, film)
                } else {
                    val url = bookmark.url
                    if (url == null) {
                        logger.warn("Stored bookmark is invalid, url is null")
                    } else {
                        val normalizedUrl = url.lowercase(Locale.ROOT)
                        val film = filmsByUrl[normalizedUrl]
                        assignData(bookmark, film)
                        // if we didn't have hashCode, update to new format now if possible...
                        if (film != null) {
                            bookmark.filmHashCode = film.sha256
                        }
                    }
                }
            }
        }
    }

    private fun assignData(bookmark: BookmarkData, film: DatenFilm?) {
        bookmark.datenFilm = film
        if (film != null) {
            film.bookmark = bookmark // Link backwards
        }
    }

    private fun createFilmHashIndex(films: List<DatenFilm>, targetHashes: Set<String>): Map<String, DatenFilm> {
        if (targetHashes.isEmpty()) {
            return emptyMap()
        }

        val unmatchedHashes = HashSet(targetHashes)
        val filmsByHash = HashMap<String, DatenFilm>(targetHashes.size)
        for (film in films) {
            val hash = film.sha256
            if (hash in unmatchedHashes) {
                filmsByHash.putIfAbsent(hash, film)
                unmatchedHashes.remove(hash)
                if (unmatchedHashes.isEmpty()) {
                    break
                }
            }
        }
        return filmsByHash
    }

    private fun createFilmUrlIndex(films: List<DatenFilm>, targetUrls: Set<String>): Map<String, DatenFilm> {
        if (targetUrls.isEmpty()) {
            return emptyMap()
        }

        val unmatchedUrls = HashSet(targetUrls)
        val filmsByUrl = HashMap<String, DatenFilm>(targetUrls.size)
        for (film in films) {
            val normalizedUrl = film.urlNormalQuality.lowercase(Locale.ROOT)
            if (normalizedUrl in unmatchedUrls) {
                filmsByUrl.putIfAbsent(normalizedUrl, film)
                unmatchedUrls.remove(normalizedUrl)
                if (unmatchedUrls.isEmpty()) {
                    break
                }
            }
        }
        return filmsByUrl
    }

    private companion object {
        val logger: Logger = LogManager.getLogger()
    }
}
