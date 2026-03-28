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

package mediathek.gui.bookmark;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import mediathek.config.Daten;
import mediathek.config.StandardLocations;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.time.LocalDate;
import java.util.*;

/**
 * Stores a full list of bookmarked movies.
 *
 * @author K. Wich
 */
public class BookmarkDataList {
    private static final Logger logger = LogManager.getLogger();
    private final BasicEventList<BookmarkData> bookmarks;

    public BookmarkDataList(@NotNull Daten daten) {
        bookmarks = new BasicEventList<>();

        // Wait until film liste is ready and update references
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                Thread.ofVirtual().start(() -> updateBookMarksFromFilmList());
            }
        });
    }

    /**
     * Remove all bookmarks and deassociate film data
     */
    public void clear() {
        bookmarks.forEach(bookmark -> {
            var filmOpt = bookmark.getDatenFilmOptional();
            filmOpt.ifPresent(film -> film.setBookmark(null));
        });
        bookmarks.clear();
        saveToFile();
    }

    /**
     * Return data list for Bookmark window
     *
     * @return observable List
     */
    public EventList<BookmarkData> getEventList() {
        return bookmarks;
    }

    public void removeBookmark(@NotNull BookmarkData bookmark) {
        var filmOpt = bookmark.getDatenFilmOptional();
        filmOpt.ifPresent(film -> film.setBookmark(null));
        bookmark.setDatenFilm(null);
        bookmarks.remove(bookmark);
    }

    /**
     * Add given film(s) to List if not yet in list
     * otherwise remove them from list
     * Note: if one of the given films is not bookmarked all are bookmarked
     *
     * @param movies: list of movies to be added
     */
    public void checkAndBookmarkMovies(@NotNull List<DatenFilm> movies) {
        ArrayList<DatenFilm> addlist = new ArrayList<>();
        ArrayList<BookmarkData> dellist = new ArrayList<>();
        boolean add = false;
        for (var film : movies) {
            if (!film.isBookmarked()) {
                add = true;
                addlist.add(film);
            }
            else {
                BookmarkData bookmark = findBookmarkFromFilm(film);
                if (bookmark != null) {
                    dellist.add(bookmark);
                }
            }
        }

        if (add) {
            // Check if history list is known
            bookmarks.getReadWriteLock().writeLock().lock();
            try (var history = new SeenHistoryController()) {
                addlist.forEach(movie -> {
                    BookmarkData bdata = new BookmarkData(movie);
                    movie.setBookmark(bdata); // Link backwards
                    bdata.setSeen(history.hasBeenSeen(movie));
                    bdata.setFilmHashCode(movie.getSha256());
                    bdata.setBookmarkAdded(LocalDate.now());
                    bookmarks.add(bdata);
                });
            }
            catch (Exception ex) {
                logger.error("history produced error", ex);
            }
            finally {
                bookmarks.getReadWriteLock().writeLock().unlock();
            }
        }
        else { // delete existing bookmarks
            movies.forEach(movie -> movie.setBookmark(null));
            bookmarks.getReadWriteLock().writeLock().lock();
            try {
                bookmarks.removeAll(dellist);
            }
            finally {
                bookmarks.getReadWriteLock().writeLock().unlock();
            }
        }
    }

    /**
     * Load Bookmarklist from backup medium
     */
    public void loadFromFile() {
        var filePath = StandardLocations.getBookmarkFilePath();

        try {
            var bookmarkList = BookmarkJsonStore.read(filePath);
            if (bookmarkList != null) {
                bookmarks.addAll(bookmarkList);
                bookmarkList.clear();
            }
        }
        catch (Exception e) {
            logger.error("Could not read bookmarks from file {}, error {} => file ignored", filePath.toString(), e.getMessage());
        }
    }

    public synchronized void saveToFile() {
        var filePath = StandardLocations.getBookmarkFilePath();

        try {
            BookmarkJsonStore.write(filePath, bookmarks);
            logger.trace("Bookmarks written");
        }
        catch (Exception e) {
            logger.error("Could not save bookmarks to {}", filePath, e);
        }
    }

    /**
     * Updates the seen state
     *
     * @param seen: True if movies are seen
     * @param list: List of movies
     */
    public void updateSeen(boolean seen, @NotNull List<DatenFilm> list) {
        list.stream()
                .filter(DatenFilm::isBookmarked)
                .forEachOrdered((movie) -> {
                    var bookmark = movie.getBookmark();
                    if (bookmark != null)
                        bookmark.setSeen(seen);
                });
    }

    /// called from [SeenHistoryController].
    public void updateSeen(boolean seen, @NotNull DatenFilm film) {
        if (film.isBookmarked()) {
            var bookmark = film.getBookmark();
            if (bookmark != null) {
                bookmark.setSeen(seen);
            }
        }
    }

    /**
     * Find Bookmark from film object.
     * @param film the film object
     * @return the associated bookmark or null.
     */
    private BookmarkData findBookmarkFromFilm(@NotNull DatenFilm film) {
        for (var bookmark : bookmarks) {
            var bookmarkFilm = bookmark.getDatenFilm();
            if (bookmarkFilm != null && bookmarkFilm.equals(film)) {
                return bookmark;
            }
        }
        return null;
    }

    /**
     * Updates the stored bookmark data reference with actual film list
     * and links the entries
     * Executed in background
     */
    private void updateBookMarksFromFilmList() {
        if (bookmarks.isEmpty())
            return;

        ListeFilme listefilme = Daten.getInstance().getListeFilme();
        List<DatenFilm> filmSnapshot;
        synchronized (listefilme) {
            filmSnapshot = new ArrayList<>(listefilme);
        }
        var filmsByHash = createFilmHashIndex(filmSnapshot);
        var filmsByUrl = createFilmUrlIndex(filmSnapshot);
        List<BookmarkData> bookmarkSnapshot;
        bookmarks.getReadWriteLock().readLock().lock();
        try {
            bookmarkSnapshot = new ArrayList<>(bookmarks);
        }
        finally {
            bookmarks.getReadWriteLock().readLock().unlock();
        }

        for (var bookmark : bookmarkSnapshot) {
            var hashCodeStr = bookmark.getFilmHashCode();
            if (hashCodeStr != null) {
                var film = filmsByHash.get(hashCodeStr);
                assignData(bookmark, film);
            }
            else {
                var url = bookmark.getUrl();
                if (url == null) {
                    logger.warn("Stored bookmark is invalid, url is null");
                }
                else {
                    var film = filmsByUrl.get(url.toLowerCase(Locale.ROOT));
                    assignData(bookmark, film);
                    // if we didn't have hashCode, update to new format now if possible...
                    if (film != null) {
                        bookmark.setFilmHashCode(film.getSha256());
                    }
                }
            }
        }
    }

    private void assignData(BookmarkData bookmark, DatenFilm film) {
        bookmark.setDatenFilm(film);
        if (film != null) {
            film.setBookmark(bookmark);   // Link backwards
        }
    }

    private Map<String, DatenFilm> createFilmHashIndex(List<DatenFilm> films) {
        Map<String, DatenFilm> filmsByHash = new HashMap<>(films.size());
        for (var film : films) {
            filmsByHash.putIfAbsent(film.getSha256(), film);
        }
        return filmsByHash;
    }

    private Map<String, DatenFilm> createFilmUrlIndex(List<DatenFilm> films) {
        Map<String, DatenFilm> filmsByUrl = new HashMap<>(films.size());
        for (var film : films) {
            filmsByUrl.putIfAbsent(film.getUrlNormalQuality().toLowerCase(Locale.ROOT), film);
        }
        return filmsByUrl;
    }
}
