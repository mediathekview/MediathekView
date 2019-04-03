package mediathek.controller.history;

import mSearch.daten.DatenFilm;
import mediathek.config.Daten;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;

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
    public synchronized void zeileSchreiben(ArrayList<DatenFilm> arrayFilms) {
        logger.trace("zeileSchreiben(thema: {})", arrayFilms);
        super.zeileSchreiben(arrayFilms);
    }

    public synchronized void setGesehen(boolean gesehen, ArrayList<DatenFilm> arrayFilms) {
        logger.trace("setGesehen({})", gesehen);
        if (arrayFilms.isEmpty()) {
            return;
        }
        if (!gesehen) {
            urlAusLogfileLoeschen(arrayFilms);
        } else {
            ArrayList<DatenFilm> neueFilme = new ArrayList<>();
            arrayFilms.stream().filter(film -> !urlPruefen(film.getUrlHistory()))
                    .forEach(neueFilme::add);
            zeileSchreiben(neueFilme);
        }
    }
}
