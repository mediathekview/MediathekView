package mediathek.gui.bookmark;

import com.fasterxml.jackson.core.JsonEncoding;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.javafx.bookmark.BookmarkData;
import mediathek.javafx.bookmark.BookmarkDataList;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

/**
 * Stores a full list of bookmarked movies.
 * @author K. Wich
 */
public class BookmarkDataListSwing {
    private final List<BookmarkDataSwing> list;
    private final EventListenerList listenerList = new EventListenerList();
    private static BookmarkDataListSwing instance;

    public BookmarkDataListSwing(Daten daten) {
        list = new ArrayList<>();

        if (daten != null) {
            // Wait until film list is ready and update references
            daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
                @Override
                public void fertig(ListenerFilmeLadenEvent event) {
                    Runnable r = () -> updateBookMarksFromFilmList();
                    new Thread(r).start();
                }
            });
        }
    }

    /**
     * Return singleton
     * @param daten Reference to Daten object used by list
     * @return existing or new instance
     */
    public static BookmarkDataListSwing getInstance(Daten daten) {
        return instance == null ? instance = new BookmarkDataListSwing(daten) : instance;
    }

    /**
     * Return data list for Bookmark window
     * @return List of bookmarks
     */
    public List<BookmarkDataSwing> getList() {
        return list;
    }

    /**
     * Get size of bookmark list
     * @return number of stored movies
     */
    public int getNbOfEntries() {
        return list.size();
    }

    /**
     * Delete Bookmarklist
     */
    public void clear() {
        list.clear();
        fireChangeEvent();
    }

    /**
     * Add a change listener
     * @param listener the listener to add
     */
    public void addChangeListener(ChangeListener listener) {
        listenerList.add(ChangeListener.class, listener);
    }

    /**
     * Remove a change listener
     * @param listener the listener to remove
     */
    public void removeChangeListener(ChangeListener listener) {
        listenerList.remove(ChangeListener.class, listener);
    }

    /**
     * Notify all listeners that the list has changed
     */
    protected void fireChangeEvent() {
        ChangeEvent event = new ChangeEvent(this);
        for (ChangeListener listener : listenerList.getListeners(ChangeListener.class)) {
            listener.stateChanged(event);
        }
    }

    /**
     * Get number of Films marked as seen
     * @return number
     */
    public int getSeenNbOfEntries() {
        int count = 0;
        for (BookmarkDataSwing d: list) {
            if (d.getSeen()) {
                count++;
            }
        }
        return count;
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
        ArrayList<BookmarkDataSwing> dellist = new ArrayList<>();
        boolean add = false;
        for (DatenFilm data: movies) {
            if (!data.isBookmarked()) {
                add = true;
                addlist.add(data);
            }
            else {
                BookmarkDataSwing movie = findMovieInList(data);
                if (movie != null) {
                    dellist.add(movie);
                }
            }
        }

        if (add) {
            // Check if history list is known
            try (var history = new SeenHistoryController()){
                for (DatenFilm movie: addlist) {
                    BookmarkDataSwing bdata = new BookmarkDataSwing(movie);
                    movie.setBookmark(bdata); // Link backwards
                    // Set seen marker if in history and not livestream
                    bdata.setSeen(!bdata.isLiveStream() && history.hasBeenSeen(movie));
                    list.add(bdata);
                }
            }
            catch (Exception ex) {
                logger.error("history produced error", ex);
            }
        }
        else { // delete existing bookmarks
            for (DatenFilm movie: movies) {  // delete references
                movie.setBookmark((BookmarkDataSwing) null);
            }
            list.removeAll(dellist);
        }
        fireChangeEvent();
    }

    /**
     * Delete given bookmarks from list and remove reference in film list)
     * @param bookmarks The list of bookmarks.
     */
    public void deleteEntries(List<BookmarkDataSwing> bookmarks) {
        for (BookmarkDataSwing bookmark: bookmarks) {  // delete references
            DatenFilm movie = bookmark.getDatenFilm();
            if (movie != null) {
                movie.setBookmark((BookmarkDataSwing) null);
            }
        }
        list.removeAll(bookmarks);
        fireChangeEvent();
    }


    /**
     * Load Bookmarklist from backup medium
     * @param filePath: File to read from
     *
     * TODO: Add File checks
     */
    public void loadFromFile(Path filePath) {
        try (JsonParser parser = new MappingJsonFactory().createParser(filePath.toFile())) {
            JsonToken jToken;
            while((jToken = parser.nextToken()) != null){
                if (jToken == JsonToken.START_ARRAY) {
                    while (parser.nextToken() != JsonToken.END_ARRAY) {
                        BookmarkDataSwing obj = parser.readValueAs(BookmarkDataSwing.class);
                        list.add(obj);
                    }
                }
            }
        }
        catch (Exception e) {
            logger.warn("Could not read bookmarks from file {}, error {} => file ignored", filePath.toString(), e.getMessage());
        }

        //sanity check if someone added way too many bookmarks
        if (list.size() > 1000)
            logger.warn("Bookmark entries exceed threshold: {}", list.size());

        fireChangeEvent();
    }

    private static final Logger logger = LogManager.getLogger();

    /**
     * Save Bookmarklist to backup medium
     * @param filePath: File to save to
     */
    public void saveToFile(Path filePath) {
        try (JsonGenerator jGenerator = new MappingJsonFactory().createGenerator(filePath.toFile(), JsonEncoding.UTF8).useDefaultPrettyPrinter()) {
            jGenerator.writeStartObject();
            jGenerator.writeFieldName("bookmarks");
            jGenerator.writeStartArray();
            for (BookmarkDataSwing bookmarkData : list) {
                jGenerator.writeObject(bookmarkData);
            }
            jGenerator.writeEndArray();
            jGenerator.writeEndObject();
        }
        catch (IOException e) {
            logger.warn("Could not save bookmarks to file {}, error {}", filePath.toString(), e.toString());
        }
    }

    /**
     * Updates the seen state
     * @param seen: True if movies are seen
     * @param list: List of movies
     */
    public void updateSeen(boolean seen, List<DatenFilm> list) {
        list.stream().filter(DatenFilm::isBookmarked).forEachOrdered((movie) -> movie.getBookmark().setSeen(seen));
        fireChangeEvent();
    }

    public void updateSeen(boolean seen, DatenFilm film) {
        if (film.isBookmarked()) {
            film.getBookmark().setSeen(seen);
            fireChangeEvent();
        }
    }

    /**
     * Find Movie in list
     */
    private BookmarkDataSwing findMovieInList(DatenFilm movie) {
        BookmarkDataSwing result = null;
        for (BookmarkDataSwing data: list) {
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
    public void updateBookMarksFromFilmList() {
        Iterator<BookmarkDataSwing> iterator = list.iterator();
        ListeFilme listefilme = Daten.getInstance().getListeFilme();
        DatenFilm filmdata;
        while (iterator.hasNext()) {
            BookmarkDataSwing data = iterator.next();
            filmdata = listefilme.getFilmByUrlAndSender(data.getUrl(), data.getSender());
            if (filmdata != null) {
                data.setDatenFilm(filmdata);
                filmdata.setBookmark(data);   // Link backwards
            }
            else {
                data.setDatenFilm(null);
            }
        }
        fireChangeEvent();
    }
}
