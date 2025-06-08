package mediathek.javafx.bookmark;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.google.common.hash.HashCode;
import javafx.beans.Observable;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
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

import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Stores a full list of bookmarked movies.
 *
 * @author K. Wich
 */
public class BookmarkDataList {
    private static final Logger logger = LogManager.getLogger();
    private final ObservableList<BookmarkData> bookmarks;
    private final ObjectMapper objectMapper;

    public BookmarkDataList(@NotNull Daten daten) {
        bookmarks = FXCollections.observableArrayList((BookmarkData data) -> new Observable[]{
                data.getSeenProperty()
        });

        objectMapper = JsonMapper.builder().findAndAddModules().build();

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
    }

    /**
     * Return data list for Bookmark window
     *
     * @return observable List
     */
    @JsonGetter("bookmarks")
    public ObservableList<BookmarkData> getObervableList() {
        return bookmarks;
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
            try (var history = new SeenHistoryController()) {
                addlist.forEach(movie -> {
                    BookmarkData bdata = new BookmarkData(movie);
                    movie.setBookmark(bdata); // Link backwards
                    bdata.setSeen(history.hasBeenSeen(movie));
                    bdata.setFilmHashCode(movie.getSha256().toString());
                    bdata.setBookmarkAdded(LocalDate.now());
                    bookmarks.add(bdata);
                });
            }
            catch (Exception ex) {
                logger.error("history produced error", ex);
            }
        }
        else { // delete existing bookmarks
            movies.forEach(movie -> movie.setBookmark(null));
            bookmarks.removeAll(dellist);
        }
    }

    /**
     * Delete given bookmarkList from list and remove reference in film list)
     *
     * @param bookmarkList The list of bookmarkList.
     */
    public void deleteEntries(@NotNull List<BookmarkData> bookmarkList) {
        for (var bookmark : bookmarkList) {  // delete references
            var movie = bookmark.getDatenFilm();
            if (movie != null) {
                movie.setBookmark(null);
            }
        }
        bookmarks.removeAll(bookmarkList);
    }

    /**
     * Load Bookmarklist from backup medium
     */
    public void loadFromFile() {
        var filePath = StandardLocations.getBookmarkFilePath();

        try {
            var wrapper = objectMapper.readValue(filePath.toFile(), BookmarksWrapper.class);
            var bookmarkList = wrapper.getBookmarks();
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
            var objectWriter = objectMapper.writerWithDefaultPrettyPrinter();
            objectWriter.writeValue(filePath.toFile(), this);
            logger.trace("Bookmarks written");
        }
        catch (IOException e) {
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

        for (var bookmark : bookmarks) {
            var hashCodeStr = bookmark.getFilmHashCode();
            if (hashCodeStr != null) {
                var hashCode = HashCode.fromString(hashCodeStr);
                var film = listefilme.parallelStream()
                        .filter(df -> df.getSha256().equals(hashCode)).findFirst().orElse(null);
                assignData(bookmark, film);
            }
            else {
                var film = listefilme.getFilmByUrl(bookmark.getUrl());
                assignData(bookmark, film);
                // if we didn't have hashCode, update to new format now if possible...
                if (film != null) {
                    bookmark.setFilmHashCode(film.getSha256().toString());
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


    /**
     * Internal wrapper for reading bookmarks with object mapper
     */
    private static class BookmarksWrapper {
        private List<BookmarkData> bookmarks;

        public List<BookmarkData> getBookmarks() {
            return bookmarks;
        }

        @SuppressWarnings("unused")
        public void setBookmarks(List<BookmarkData> bookmarks) {
            this.bookmarks = bookmarks;
        }
    }
}