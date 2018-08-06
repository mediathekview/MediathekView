/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.filmlisten;

import mSearch.Config;
import mSearch.daten.ListeFilme;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.FilmlistenSuchen;
import mSearch.filmlisten.reader.FilmListReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.event.EventListenerList;

public class ImportFilmliste {

    private static final Logger logger = LogManager.getLogger(ImportFilmliste.class);
    private final EventListenerList listeners;
    private final FilmListReader msFilmListReader;
    public FilmlistenSuchen msFilmlistenSuchen;

    public ImportFilmliste() {
        listeners = new EventListenerList();
        msFilmListReader = new FilmListReader();
        msFilmlistenSuchen = new FilmlistenSuchen();
        msFilmListReader.addAdListener(new ListenerFilmeLaden() {
            @Override
            public synchronized void start(ListenerFilmeLadenEvent event) {
                for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
                    l.start(event);
                }
            }

            @Override
            public synchronized void progress(ListenerFilmeLadenEvent event) {
                for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
                    l.progress(event);

                }
            }

            @Override
            public synchronized void fertig(ListenerFilmeLadenEvent event) {
            }
        });
    }

    /**
     * Filmeliste importieren, URL automatisch wählen
     */
    public void importFromUrl(ListeFilme listeFilme, ListeFilme listeFilmeDiff, int days) {
        Config.setStop(false);
        Thread importThread = new FilmeImportierenAutoThread(msFilmlistenSuchen, listeFilme, listeFilmeDiff, days,
                this::urlLaden, this::fertigMelden);
        importThread.start();
    }

    /**
     * Filmeliste importieren, mit fester URL/Pfad
     */
    public void importFromFile(String pfad, ListeFilme listeFilme, int days) {
        Config.setStop(false);
        Thread importThread = new FilmeImportierenDateiThread(pfad, listeFilme, days,
                this::urlLaden, this::fertigMelden);
        importThread.start();
    }

    public void addAdListener(ListenerFilmeLaden listener) {
        listeners.add(ListenerFilmeLaden.class, listener);
    }

    private boolean urlLaden(String dateiUrl, ListeFilme listeFilme, int days) {
        boolean ret = false;
        try {
            if (!dateiUrl.isEmpty()) {
                logger.info("Filmliste laden von: {}", dateiUrl);
                msFilmListReader.readFilmListe(dateiUrl, listeFilme, days);
                if (!listeFilme.isEmpty()) {
                    ret = true;
                }
            }
        } catch (Exception ex) {
            logger.error("urlLaden", ex);
        }
        return ret;
    }

    private synchronized void fertigMelden(boolean ok) {
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.fertig(new ListenerFilmeLadenEvent("", "", 0, 0, 0, !ok));
        }
    }
}
