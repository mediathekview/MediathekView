package mediathek.javafx.bookmark;

import com.fasterxml.jackson.core.JsonEncoding;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import javafx.beans.Observable;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;

/**
 * Stores a full list of bookmarked movies.
 * @author K. Wich
 */
public class BookmarkDataList
{
  private final ObservableList<BookmarkData> olist;
  private static SeenHistoryController history;
  private static BookmarkDataList instance;
  private static boolean linked;  // Indicates if list is linked with movie list
  private List <DatenFilm> pendingAddList;

  private BookmarkDataList(Daten daten) {
    olist = FXCollections.observableArrayList((BookmarkData data) -> new Observable[]{
      data.getSeenProperty()
    });
    history = null;
    linked = false;
    if (daten != null) {
      // Wait until film liste is ready and update references
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
   * @return exisitng or new instance
   */
  public static BookmarkDataList getInstance(Daten daten) {
    return instance == null ? instance = new BookmarkDataList(daten) : instance;
  }

  /**
   * Return data list for Bookmark window
   * @return observable List
   */
  public ObservableList<BookmarkData> getObervableList() {
    return olist;
  }

  /**
   * Get size of bookmark list
   * @return number of stored movies
   */
  public int getNbOfEntries() {
    return olist.size();
  }

  /**
   * Delete Bookmarklist
   */
  public void clear() {
    olist.clear();
  }

  /**
   * Get number of Films marked as seen
   * @return number
   */
  public int getSeenNbOfEntries() {
    int count = 0;
    for (BookmarkData d: olist) {
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
  public void checkAndBookmarkMovies(List <DatenFilm> movies) {
    ArrayList<DatenFilm> addlist = new ArrayList<>();
    ArrayList<BookmarkData> dellist = new ArrayList<>();
    boolean add = false;
    for (DatenFilm data: movies) {
      if (!data.isBookmarked()) {
        add = true;
        addlist.add(data);
      }
      else {
        if (!add) { // not necessary to store movie for add operation
          BookmarkData movie = findMovieInList(data);
          if (movie != null) {
            dellist.add(movie);
          }
        }
      }
    }

    if (add) {
      // Check if history list is known
      if (history == null) {
        history = Daten.getInstance().getSeenHistoryController();
      }
      for (DatenFilm movie: addlist) {
        BookmarkData bdata = new BookmarkData(movie);
        movie.setBookmark(bdata); // Link backwards
        // Set seen marker if in history and not livestream
        bdata.setSeen(!bdata.isLiveStream() && history.urlPruefen(movie.getUrl()));
        olist.add(bdata);
      }
    }
    else { // delete existing bookmarks
      for (DatenFilm movie: movies) {  // delete references
        movie.setBookmark(null);
      }
      olist.removeAll(dellist);
    }
  }


  /**
   * Add movies into bookmarks as backgrund task
   * @param movies: list of movies to be added
   */
  public void bookmarkMoviesInBackground(List <DatenFilm> movies) {
    if (linked) {
      new Thread(() -> {
        if (history == null) {
          history = Daten.getInstance().getSeenHistoryController();
        }
        for (DatenFilm movie: movies) {
          if (!movie.isBookmarked()) {
            BookmarkData bdata = new BookmarkData(movie);
            movie.setBookmark(bdata); // Link backwards
            // Set seen marker if in history and not livestream
            bdata.setSeen(!bdata.isLiveStream() && history.urlPruefen(movie.getUrl()));
            olist.add(bdata);
          }
        }
      }).start();
    }
    else {
      pendingAddList = movies;
    }
  }

  /**
   * Delete given bookmarks from list and remove reference in film list)
   * @param bookmarks
   */
  public void deleteEntries(ObservableList<BookmarkData> bookmarks) {
    for (BookmarkData bookmark: bookmarks) {  // delete references
      DatenFilm movie = bookmark.getDatenFilm();
      if (movie != null) {
         movie.setBookmark(null);
      }
    }
    olist.removeAll(bookmarks);
  }


  /**
   * Load Bookmarklist from backup medium
   * @param filePath: File to read from
   *
   * TODO: Add File checks
   */
  public void loadFromFile(Path filePath) {
    try (JsonParser parser = new MappingJsonFactory().createParser(filePath.toFile()))
    {
      JsonToken jToken;
      while((jToken = parser.nextToken()) != null){
        if (jToken == JsonToken.START_ARRAY) {
          while (parser.nextToken() != JsonToken.END_ARRAY) {
            BookmarkData obj = parser.readValueAs(BookmarkData.class);
            olist.add(obj);
          }
        }
      }
    }
    catch (Exception e)
    {
      LogManager.getLogger(Daten.class).warn("Could not read bookmarks from file {}, error {} => file ignored", filePath.toString(), e.getMessage());
    }

    //sanity check if someone added way too many bookmarks
    if (olist.size() > 1000)
      logger.warn("Bookmark entries exceed threshold: {}", olist.size());
  }

  private static final Logger logger = LogManager.getLogger();

  /**
   * Save Bookmarklist to backup medium
   * @param filePath: File to save to
   */
  public void saveToFile(Path filePath) {
    try (JsonGenerator jGenerator = new MappingJsonFactory().createGenerator(filePath.toFile(), JsonEncoding.UTF8).useDefaultPrettyPrinter())
    {
      jGenerator.writeStartObject();
      jGenerator.writeFieldName("bookmarks");
      jGenerator.writeStartArray();
      for (BookmarkData bookmarkData : olist) {
        jGenerator.writeObject(bookmarkData);
      }
      jGenerator.writeEndArray();
      jGenerator.writeEndObject();
    }
    catch (IOException e)
    {
      LogManager.getLogger(Daten.class).warn("Could not save bookmarks to file {}, error {}", filePath.toString(), e.toString());
    }
  }

  /**
   * Updates the seen state
   * @param seen: True if movies are seen
   * @param list: List of movies
   */
  public void updateSeen(boolean seen, List<DatenFilm> list) {
    list.stream().filter(DatenFilm::isBookmarked).forEachOrdered((movie) -> movie.getBookmark().setSeen(seen));
  }

  /**
   * Find Movie in list
   */
  private BookmarkData findMovieInList(DatenFilm movie) {
    BookmarkData result = null;
    for (BookmarkData data: olist ) {
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
    Iterator<BookmarkData> iterator = olist.iterator();
    ListeFilme listefilme = Daten.getInstance().getListeFilme();
    DatenFilm filmdata;
    while (iterator.hasNext()) {
      BookmarkData data = iterator.next();
      filmdata = listefilme.getFilmByUrlAndSender(data.getUrl(), data.getSender());
      if (filmdata != null) {
        data.setDatenFilm(filmdata);
        filmdata.setBookmark(data);   // Link backwards
      }
      else {
        data.setDatenFilm(null);
      }
    }
    linked = true;

    if (pendingAddList != null) {
      bookmarkMoviesInBackground(pendingAddList);
      pendingAddList = null;
    }
  }


 /**
   * Try to retrieve the expiry date from the associated webpage
   */
  private static final Pattern[] DATE_PATTERNS = {null, null};
  private static final String[] DATE_PATTERN_STRINGS = {"verfügbar.+?bis.+?([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})", "verfügbar.+?bis.+?([0-9]{2}/[0-9]{2}/[0-9]{4})"};
  public static  String searchExpiryDate(BookmarkData data) {
    String result = "";
    if (data.hasWebURL()) {
      final Request request = new Request.Builder().url(data.getWebUrl())
                .header("User-Agent", ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_USER_AGENT))
                .build();
      try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute()) {
        if (response.isSuccessful()) {
          String responsedata = response.body().string();
          if (responsedata.length() > 0) {
            // 2.) use regex to extract date
            for (int k = 0; k < DATE_PATTERNS.length; k++) {
              if (DATE_PATTERNS[k] == null) {   // compile pattern only once!
                DATE_PATTERNS[k] = Pattern.compile(DATE_PATTERN_STRINGS[k], Pattern.CASE_INSENSITIVE );
              }
              Matcher matcher = DATE_PATTERNS[k].matcher(responsedata);
              if (matcher.find()) {
                result = matcher.group(1).replaceAll("/", "\\.");
                break;
              }
            }
          }
        }
      }
      catch (Exception UNUSED) {}
    }
    return result;
  }

}
