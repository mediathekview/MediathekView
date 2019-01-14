package mediathek.controller.history;

import mSearch.daten.DatenFilm;
import mediathek.config.Daten;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;

import java.util.ArrayList;

public class SeenHistoryController extends MVUsedUrls<DownloadHistoryChangedEvent> {
    public SeenHistoryController() {
        super("history.txt", Daten.getSettingsDirectory_String(), DownloadHistoryChangedEvent.class);
    }

    public synchronized void setGesehen(boolean gesehen, ArrayList<DatenFilm> arrayFilms) {
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
