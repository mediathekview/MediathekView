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
package mediathek.controller.filmeLaden;

import javax.swing.event.EventListenerList;
import mediathek.controller.filmeLaden.importieren.ImportFilmliste;
import mediathek.controller.filmeLaden.importieren.ListeFilmUpdateServer;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.daten.Daten;
import mediathek.daten.ListeFilme;
import mediathek.tool.GuiFunktionen;

public class FilmeLaden {

    // public static
    public static final int UPDATE_FILME_AUS = 0; // nix
    public static final int UPDATE_FILME_URL = 1; // manuell laden, Url automatisch wählen
    public static final int UPDATE_FILME_AUTO = 2; // beim Start, immer mal wieder, + Url auto
    public static final int ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE = 3 * 60 * 60; // beim Start des Programms wir die Liste geladen wenn sie älter ist als ..
    public static String updateUrl = "";
    // private
    private boolean stop = false;
    private ListeFilme listeFilmeAlt = null; // ist nur eine Referenz auf die bestehende Liste und die bleibt unverändert!!!
    private ListeFilme listeFilmeNeu = null; //ist eine NEUE ungefilterte Liste, wird beim Laden NEU erstellt
    private FilmeSuchenSender filmeSuchen;
    private ImportFilmliste filmeImportieren;
    private EventListenerList listeners = new EventListenerList();
    private boolean istAmLaufen = false;

    public FilmeLaden() {
        filmeSuchen = new FilmeSuchenSender();
        filmeImportieren = new ImportFilmliste();
        filmeSuchen.addAdListener(new BeobLadenSuchen());
        filmeImportieren.addAdListener(new BeobLadenImportieren());
    }

    // ###########################
    // Listener
    // ###########################
    public void addAdListener(ListenerFilmeLaden listener) {
        listeners.add(ListenerFilmeLaden.class, listener);
    }

    private void notifyStart(ListenerFilmeLadenEvent event) {
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.start(event);
        }
    }

    private void notifyProgress(ListenerFilmeLadenEvent event) {
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.progress(event);
        }
    }

    private void notifyFertig(ListenerFilmeLadenEvent event) {
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.fertig(event);
        }
    }
    // ###########################

    public void setAllesLaden(boolean alles) {
        // beim Sender laden: alles nicht nur ein Update
        filmeSuchen.allesLaden = alles;
    }

    public synchronized void setStop() {
        stop = true;
    }

    public synchronized boolean getStop() {
        return stop;
    }

    public ListeFilmUpdateServer getListeFilmUpdateServer(boolean update) {
        if (update) {
            filmeImportieren.filmUpdateServer.suchen();
        }
        return filmeImportieren.filmUpdateServer.listeUpdateServer;
    }

    public String[] getSenderNamen() {
        return filmeSuchen.getNamenSenderFilmliste();
    }

    // #########################################################
    // Filme als Liste importieren
    // #########################################################
    public void importFilmliste(String dateiUrl) {
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            stop = false;
            if (dateiUrl.equals("")) {
                // Filme als Liste importieren, Url automatisch ermitteln
                filmeImportieren.filmeImportierenAuto();
            } else {
                // Filme als Liste importieren, feste URL/Datei
                filmeImportieren.filmeImportierenDatei(dateiUrl, GuiFunktionen.istUrl(dateiUrl));
            }
        }
    }

    // #######################################
    // Filme bei den Sendern laden
    // #######################################
    public void filmeBeimSenderSuchen(ListeFilme llisteFilme, boolean allesLaden) {
        // Filme bei allen Sender suchen
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            stop = false;
            listeFilmeAlt = llisteFilme;
            filmeSuchen.filmeBeimSenderLaden(allesLaden, listeFilmeAlt);
        }
    }

    public void updateSender(String sender, ListeFilme llisteFilme) {
        // Filme nur bei EINEM Sender suchen (nur update)
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            stop = false;
            listeFilmeAlt = llisteFilme;
            filmeSuchen.updateSender(sender, llisteFilme);
        }
    }

    private class BeobLadenSuchen extends ListenerFilmeLaden {

        @Override
        public synchronized void start(ListenerFilmeLadenEvent event) {
            notifyStart(event);
        }

        @Override
        public synchronized void progress(ListenerFilmeLadenEvent event) {
            notifyProgress(event);
        }

        @Override
        public synchronized void fertig(ListenerFilmeLadenEvent event) {
            // Ergebnisliste listeFilme eintragen -> Feierabend!
            listeFilmeNeu = filmeSuchen.listeFilmeNeu;
            filmeSuchen.listeFilmeNeu = null;
            fertig_(event);
        }
    }

    private class BeobLadenImportieren extends ListenerFilmeLaden {

        @Override
        public synchronized void start(ListenerFilmeLadenEvent event) {
            notifyStart(event);
        }

        @Override
        public synchronized void progress(ListenerFilmeLadenEvent event) {
            notifyProgress(event);
        }

        @Override
        public synchronized void fertig(ListenerFilmeLadenEvent event) {
            // Ergebnisliste listeFilme eintragen -> Feierabend!
            listeFilmeNeu = filmeImportieren.listeFilme;
            filmeImportieren.listeFilme = null;
            fertig_(event);
        }
    }

    private void fertig_(ListenerFilmeLadenEvent event) {
        istAmLaufen = false;
        if (listeFilmeNeu != null) {
            Daten.listeFilme = listeFilmeNeu;
        } else {
            Daten.listeFilme = new ListeFilme();
        }
        notifyFertig(event);
    }
}
