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
package mediathek.controller;

import javax.swing.SwingUtilities;
import javax.swing.event.EventListenerList;
import mediathek.daten.Daten;
import mediathek.tool.Duration;
import mediathek.tool.Log;
import msearch.daten.ListeFilme;
import msearch.daten.MSearchConfig;
import msearch.filmeLaden.ListeDownloadUrlsFilmlisten;
import msearch.filmeLaden.ListeFilmlistenServer;
import msearch.filmeLaden.MSearchImportFilmliste;
import msearch.filmeSuchen.MSearchFilmeSuchen;
import msearch.filmeSuchen.MSearchListenerFilmeLaden;
import msearch.filmeSuchen.MSearchListenerFilmeLadenEvent;

public class FilmeLaden {

    // public static
    public static final int UPDATE_FILME_AUS = 0; // nix
    public static final int UPDATE_FILME_URL = 1; // manuell laden, Url automatisch wählen
    public static final int UPDATE_FILME_AUTO = 2; // beim Start, immer mal wieder, + Url auto
    public static final int ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE = 3 * 60 * 60; // beim Start des Programms wir die Liste geladen wenn sie älter ist als ..
    private Duration duration = new Duration(FilmeLaden.class.getSimpleName());

    private static enum ListenerMelden {

        START, PROGRESS, FINISHED
    };
    // private
    private MSearchFilmeSuchen mSearchFilmeSuchen;
    private MSearchImportFilmliste mSearchImportFilmliste;
    private EventListenerList listeners = new EventListenerList();
    private boolean istAmLaufen = false;

    public FilmeLaden() {
        mSearchFilmeSuchen = new MSearchFilmeSuchen();
        mSearchFilmeSuchen.addAdListener(new MSearchListenerFilmeLaden() {
            @Override
            public synchronized void start(MSearchListenerFilmeLadenEvent event) {
                notifyStart(event);
            }

            @Override
            public synchronized void progress(MSearchListenerFilmeLadenEvent event) {
                notifyProgress(event);
            }

            @Override
            public synchronized void fertig(MSearchListenerFilmeLadenEvent event) {
                // Ergebnisliste listeFilme eintragen -> Feierabend!
                Daten.listeFilme = mSearchFilmeSuchen.listeFilmeNeu;
                undEnde(event);
            }
        });
        mSearchImportFilmliste = new MSearchImportFilmliste();
        mSearchImportFilmliste.addAdListener(new MSearchListenerFilmeLaden() {
            @Override
            public synchronized void start(MSearchListenerFilmeLadenEvent event) {
                notifyStart(event);
            }

            @Override
            public synchronized void progress(MSearchListenerFilmeLadenEvent event) {
                notifyProgress(event);
            }

            @Override
            public synchronized void fertig(MSearchListenerFilmeLadenEvent event) {
                // Ergebnisliste listeFilme eintragen -> Feierabend!
                duration.stop("Filme laden, ende");
                //Daten.filmlisteSpeichern();
                undEnde(event);
            }
        });
    }

    // ###########################
    public synchronized void setStop(boolean set) {
        MSearchConfig.setStop(set);
    }

    public String[] getSenderNamen() {
        return mSearchFilmeSuchen.getNamenSender();
    }

    // #########################################################
    // Filme als Liste importieren
    // #########################################################
    public void importFilmliste(String dateiUrl) {
        // damit wird die filmliste geladen UND auch gleich im Konfig-Ordner gespeichert
        duration.start("Filme laden, start");
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            Daten.listeFilme.clear();
            Daten.listeFilmeNachBlackList.clear();
            System.gc();
//            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, FilmeLaden.class.getSimpleName());
            if (dateiUrl.equals("")) {
                // Filme als Liste importieren, Url automatisch ermitteln
                mSearchImportFilmliste.filmeImportierenAuto(Daten.getDateiFilmliste(), Daten.listeFilme);
            } else {
                // Filme als Liste importieren, feste URL/Datei
                mSearchImportFilmliste.filmeImportierenDatei(dateiUrl, Daten.getDateiFilmliste(), Daten.listeFilme);
            }
        }
    }

    // #######################################
    // Filme bei den Sendern laden
    // #######################################
    public void filmeBeimSenderSuchen(ListeFilme llisteFilme, boolean senderAllesLaden, boolean filmlisteUpdate) {
        // Filme bei allen Sender suchen
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            MSearchConfig.senderAllesLaden = senderAllesLaden;
            MSearchConfig.updateFilmliste = filmlisteUpdate;
            mSearchFilmeSuchen.filmeBeimSenderLaden(Daten.listeFilme);
        }
    }

    public void updateSender(String[] sender, ListeFilme llisteFilme, boolean senderAllesLaden) {
        // Filme nur bei EINEM Sender suchen (nur update)
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            MSearchConfig.senderAllesLaden = senderAllesLaden;
            mSearchFilmeSuchen.updateSender(sender, Daten.listeFilme);
        }
    }

    public ListeFilmlistenServer getListeFilmlistnServer() {
        return mSearchImportFilmliste.getListe_FilmlistenServer();
    }

    public ListeDownloadUrlsFilmlisten getDownloadUrlsFilmlisten(boolean update) {
        return mSearchImportFilmliste.getDownloadUrls_Filmlisten(update);
    }

    private void undEnde(MSearchListenerFilmeLadenEvent event) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird
        Daten.listeFilme.themenLaden();
        Daten.listeAbo.setAboFuerFilm(Daten.listeFilme);
        istAmLaufen = false;
        notifyFertig(event);
    }

    // ###########################
    // Listener
    // ###########################
    public void addAdListener(MSearchListenerFilmeLaden listener) {
        listeners.add(MSearchListenerFilmeLaden.class, listener);
    }

    private void notifyStart(MSearchListenerFilmeLadenEvent event) {
        for (MSearchListenerFilmeLaden l : listeners.getListeners(MSearchListenerFilmeLaden.class)) {
            run_(new Start(l, event, ListenerMelden.START));
        }
    }

    private void notifyProgress(MSearchListenerFilmeLadenEvent event) {
        for (MSearchListenerFilmeLaden l : listeners.getListeners(MSearchListenerFilmeLaden.class)) {
            run_(new Start(l, event, ListenerMelden.PROGRESS));
        }
    }

    private void notifyFertig(MSearchListenerFilmeLadenEvent event) {
        for (MSearchListenerFilmeLaden l : listeners.getListeners(MSearchListenerFilmeLaden.class)) {
            run_(new Start(l, event, ListenerMelden.FINISHED));
        }
    }

    private class Start implements Runnable {

        private MSearchListenerFilmeLaden listenerFilmeLaden;
        private MSearchListenerFilmeLadenEvent event;
        private ListenerMelden listenerMelden;

        public Start(MSearchListenerFilmeLaden llistenerFilmeLaden, MSearchListenerFilmeLadenEvent eevent, ListenerMelden lliListenerMelden) {
            listenerFilmeLaden = llistenerFilmeLaden;
            event = eevent;
            listenerMelden = lliListenerMelden;
        }

        @Override
        public synchronized void run() {
            switch (listenerMelden) {
                case START:
                    listenerFilmeLaden.start(event);
                    break;
                case PROGRESS:
                    listenerFilmeLaden.progress(event);
                    break;
                case FINISHED:
                    listenerFilmeLaden.fertig(event);
                    break;
            }
        }
    }

    private void run_(Runnable r) {
        try {
            if (SwingUtilities.isEventDispatchThread()) {
                // entweder hier
                r.run();
            } else {
                SwingUtilities.invokeLater(r);
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(926369741, Log.FEHLER_ART_PROG, "ListenerFilmeLaden.run_", ex);
        }
    }
}
