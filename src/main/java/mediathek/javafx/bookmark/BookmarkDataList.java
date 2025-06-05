package mediathek.javafx.bookmark;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.ObjectMapper;
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

    public BookmarkDataList(@NotNull Daten daten) {
        bookmarks = FXCollections.observableArrayList((BookmarkData data) -> new Observable[]{
                data.getSeenProperty()
        });

        // Wait until film liste is ready and update references
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                Thread.ofVirtual().start(() -> updateBookMarksFromFilmList());
            }
        });
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
     * Get size of bookmark list
     *
     * @return number of stored movies
     */
    @JsonIgnore
    public int getNbOfEntries() {
        return bookmarks.size();
    }

    /**
     * Delete Bookmarklist
     */
    public void clear() {
        bookmarks.clear();
    }

    /**
     * Add given film(s) to List if not yet in list
     * otherwise remove them from list
     * Note: if one of the given films is not bookmarked all are bookmarked
     *
     * @param movies: list of movies to be added
     */
    public void checkAndBookmarkMovies(List<DatenFilm> movies) {
        ArrayList<DatenFilm> addlist = new ArrayList<>();
        ArrayList<BookmarkData> dellist = new ArrayList<>();
        boolean add = false;
        for (DatenFilm data : movies) {
            if (!data.isBookmarked()) {
                add = true;
                addlist.add(data);
            }
            else {
                BookmarkData movie = findMovieInList(data);
                if (movie != null) {
                    dellist.add(movie);
                }
            }
        }

        if (add) {
            // Check if history list is known
            try (var history = new SeenHistoryController()) {
                for (DatenFilm movie : addlist) {
                    BookmarkData bdata = new BookmarkData(movie);
                    movie.setBookmark(bdata); // Link backwards
                    // Set seen marker if in history and not livestream
                    bdata.setSeen(!bdata.isLiveStream() && history.hasBeenSeen(movie));
                    bookmarks.add(bdata);
                }
            }
            catch (Exception ex) {
                logger.error("history produced error", ex);
            }
        }
        else { // delete existing bookmarks
            for (DatenFilm movie : movies) {  // delete references
                movie.setBookmark(null);
            }
            bookmarks.removeAll(dellist);
        }
    }

    /**
     * Delete given bookmarks from list and remove reference in film list)
     *
     * @param bookmarks The list of bookmarks.
     */
    public void deleteEntries(ObservableList<BookmarkData> bookmarks) {
        for (BookmarkData bookmark : bookmarks) {  // delete references
            DatenFilm movie = bookmark.getDatenFilm();
            if (movie != null) {
                movie.setBookmark(null);
            }
        }
        this.bookmarks.removeAll(bookmarks);
    }

    /**
     * Load Bookmarklist from backup medium
     */
    public void loadFromFile() {
        var filePath = StandardLocations.getBookmarkFilePath();

        try {
            var objectMapper = new ObjectMapper();
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
            var objectMapper = new ObjectMapper().writerWithDefaultPrettyPrinter();
            objectMapper.writeValue(filePath.toFile(), this);
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
    public void updateSeen(boolean seen, List<DatenFilm> list) {
        list.stream()
                .filter(DatenFilm::isBookmarked)
                .forEachOrdered((movie) -> {
                    var bookmark = movie.getBookmark();
                    if (bookmark != null)
                        bookmark.setSeen(seen);
                });
    }

    public void updateSeen(boolean seen, DatenFilm film) {
        if (film.isBookmarked()) {
            var bookmark = film.getBookmark();
            if (bookmark != null) {
                bookmark.setSeen(seen);
            }
        }
    }

    /**
     * Find Movie in list
     */
    private BookmarkData findMovieInList(DatenFilm movie) {
        BookmarkData result = null;
        for (var data : bookmarks) {
            if (data.getDatenFilm() != null && data.getDatenFilm().equals(movie)) {
                result = data;
                break;
            }
        }
        return result;
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
            var film = listefilme.getFilmByUrl(bookmark.getUrl());
            bookmark.setDatenFilm(film);
            if (film != null) {
                film.setBookmark(bookmark);   // Link backwards
            }
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

        public void setBookmarks(List<BookmarkData> bookmarks) {
            this.bookmarks = bookmarks;
        }
    }
}