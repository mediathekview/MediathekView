package mediathek.controller.history;

import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.stream.Collectors;

public class SeenHistoryController extends MVUsedUrls<DownloadHistoryChangedEvent> {
    private static final Logger logger = LogManager.getLogger(SeenHistoryController.class);

    public SeenHistoryController() {
        super("history.txt", Daten.getSettingsDirectory_String(), DownloadHistoryChangedEvent.class);
    }

    @Override
    public synchronized void zeileSchreiben(String thema, String titel, String url) {
        logger.trace("zeileSchreiben(thema: {}, titel: {}, url: {})", thema, titel, url);
        super.zeileSchreiben(thema, titel, url);
    }

    @Override
    public synchronized void zeileSchreiben(List<DatenFilm> arrayFilms) {
        logger.trace("zeileSchreiben(thema: {})", arrayFilms);
        super.zeileSchreiben(arrayFilms);
    }

    public void markAsSeen(List<DatenFilm> filmList) {
        setGesehen(true, filmList);
    }

    public void markAsUnseen(List<DatenFilm> filmList) {
        setGesehen(false, filmList);
    }

    private synchronized void setGesehen(boolean gesehen, List<DatenFilm> arrayFilms) {
        logger.trace("setGesehen({})", gesehen);
        if (arrayFilms.isEmpty()) {
            return;
        }
        if (!gesehen) {
            urlAusLogfileLoeschen(arrayFilms);
        } else {
            List<DatenFilm> neueFilme = arrayFilms.stream()
                    .filter(film -> !urlPruefen(film.getUrl()))
                    .filter(film -> !film.isLivestream())
                    .collect(Collectors.toList());

            if (!neueFilme.isEmpty())
                zeileSchreiben(neueFilme);

            neueFilme.clear();
        }
        // Update bookmarks with seen information 
        Daten.getInstance().getListeBookmarkList().updateSeen(gesehen, arrayFilms);
    }
}
