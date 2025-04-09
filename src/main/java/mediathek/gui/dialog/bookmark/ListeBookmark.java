package mediathek.gui.dialog.bookmark;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import mediathek.config.Daten;
import mediathek.config.StandardLocations;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;

import mediathek.tool.MessageBus;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ListeBookmark {
  private static final Logger logger = LogManager.getLogger();
  private static final int BOOKMARK_THRESHOLD = 2500;
  private final List<DatenBookmark> bookmarks;

  public ListeBookmark(Daten daten) {
    bookmarks = Collections.synchronizedList(new ArrayList<>());

    // Filme-Laden Listener hinzufügen
    daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
      @Override
      public void fertig(ListenerFilmeLadenEvent event) {
        new Thread(() -> updateBookMarksFromFilmList()).start();
      }
    });
  }

  @JsonGetter("bookmarks")
  public List<DatenBookmark> getBookmarks() {
    return bookmarks;
  }

  public void clear() {
    bookmarks.clear();
  }

  @JsonIgnore
  public long getSeenNbOfEntries() {
    return bookmarks.stream().filter(DatenBookmark::getSeen).count();
  }

  public void checkAndBookmarkMovies(List<DatenFilm> movies) {
    List<DatenFilm> addList = new ArrayList<>();
    List<DatenBookmark> delList = new ArrayList<>();
    boolean add = false;

    for (DatenFilm data : movies) {
      if (!data.isBookmarked()) {
        add = true;
        addList.add(data);
      } else {
        DatenBookmark movie = findMovieInList(data);
        if (movie != null) {
          delList.add(movie);
        }
      }
    }

    if (add) {
      try (SeenHistoryController history = new SeenHistoryController()) {
        for (DatenFilm movie : addList) {
          DatenBookmark bdata = new DatenBookmark(movie);
          movie.setBookmark(bdata);
          bdata.setSeen(!bdata.isLiveStream() && history.hasBeenSeen(movie));
          bookmarks.add(bdata);
        }
      } catch (Exception ex) {
        logger.error("Fehler beim Laden der Verlaufsliste", ex);
      }
      MessageBus.getMessageBus().publishAsync(new BookmarkAddEvent());
    } else {
      for (DatenFilm movie : movies) {
        DatenBookmark bdata = new DatenBookmark(movie);
        movie.setBookmark(null);
        bookmarks.remove(bdata);
      }
      bookmarks.removeAll(delList);
      MessageBus.getMessageBus().publishAsync(new BookmarkRemoveEvent());
    }
  }

  public void deleteEntries(List<DatenBookmark> entries) {
    for (DatenBookmark bookmark : entries) {
      DatenFilm movie = bookmark.getDatenFilm();
      if (movie != null) {
        movie.setBookmark(null);
      }
    }
    bookmarks.removeAll(entries);
  }

  public void loadFromFile() {
    var filePath = StandardLocations.getBookmarkFilePath();
    try (JsonParser parser = new MappingJsonFactory().createParser(filePath.toFile())) {
      JsonToken token;
      while ((token = parser.nextToken()) != null) {
        if (token == JsonToken.START_ARRAY) {
          while (parser.nextToken() != JsonToken.END_ARRAY) {
            DatenBookmark obj = parser.readValueAs(DatenBookmark.class);
            bookmarks.add(obj);
          }
        }
      }
    } catch (Exception e) {
      logger.error("Fehler beim Einlesen der Bookmarks aus {}: {}", filePath, e.getMessage());
    }

    if (bookmarks.size() > BOOKMARK_THRESHOLD) {
      logger.warn("Merkliste überschreitet Grenzwert: {}", bookmarks.size());
    }
  }

  public synchronized void saveToFile() {
    var filePath = StandardLocations.getBookmarkFilePath();
    try {
      var objectMapper = new ObjectMapper().writerWithDefaultPrettyPrinter();
      objectMapper.writeValue(filePath.toFile(), this);
      logger.trace("Bookmarks gespeichert");
    } catch (IOException e) {
      logger.error("Fehler beim Speichern nach {}", filePath, e);
    }
  }

  public void updateSeen(boolean seen, List<DatenFilm> list) {
    for (DatenFilm film : list) {
      if (film.isBookmarked()) {
        DatenBookmark bookmark = film.getBookmark();
        if (bookmark != null) {
          bookmark.setSeen(seen);
        }
      }
    }
  }

  public void updateSeen(boolean seen, DatenFilm film) {
    if (film.isBookmarked()) {
      DatenBookmark bookmark = film.getBookmark();
      if (bookmark != null) {
        bookmark.setSeen(seen);
      }
    }
  }

  private DatenBookmark findMovieInList(DatenFilm movie) {
    for (DatenBookmark data : bookmarks) {
      if (data.getDatenFilm() != null && data.getDatenFilm().equals(movie)) {
        return data;
      }
    }
    return null;
  }

  private void updateBookMarksFromFilmList() {
    ListeFilme listeFilme = Daten.getInstance().getListeFilme();
    for (DatenBookmark data : bookmarks) {
      DatenFilm filmData = listeFilme.getFilmByUrlAndSender(data.getUrl(), data.getSender());
      if (filmData != null) {
        data.setDatenFilm(filmData);
        filmData.setBookmark(data);
      } else {
        data.setDatenFilm(null);
      }
    }
  }
}


