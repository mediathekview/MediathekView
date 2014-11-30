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

import java.util.HashSet;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.event.EventListenerList;
import mediathek.daten.Daten;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden;
import mediathek.tool.Duration;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.MVConfig;
import mediathek.tool.MVListeFilme;
import mediathek.tool.MVMessageDialog;
import msearch.daten.DatenFilm;
import msearch.daten.ListeFilme;
import msearch.filmeSuchen.MSFilmeSuchen;
import msearch.filmeSuchen.MSListenerFilmeLaden;
import msearch.filmeSuchen.MSListenerFilmeLadenEvent;
import msearch.filmlisten.ListeFilmlistenUrls;
import msearch.filmlisten.MSFilmlisteLesen;
import msearch.filmlisten.MSImportFilmliste;
import msearch.tool.MSConfig;

public class FilmeLaden {

    private Duration duration = new Duration(FilmeLaden.class.getSimpleName());
    private final HashSet<String> hashSet = new HashSet<>();
    private final ListeFilme diffListe = new ListeFilme();

    private enum ListenerMelden {

        START, PROGRESS, FINISHED
    }
    // private
    private final MSImportFilmliste msImportFilmliste;
    private final EventListenerList listeners = new EventListenerList();
    private boolean istAmLaufen = false;

    public FilmeLaden() {
        msImportFilmliste = new MSImportFilmliste();
        msImportFilmliste.addAdListener(new MSListenerFilmeLaden() {
            @Override
            public synchronized void start(MSListenerFilmeLadenEvent event) {
                notifyStart(event);
            }

            @Override
            public synchronized void progress(MSListenerFilmeLadenEvent event) {
                notifyProgress(event);
            }

            @Override
            public synchronized void fertig(MSListenerFilmeLadenEvent event) {
                // Ergebnisliste listeFilme eintragen -> Feierabend!
                duration.stop("Filme laden, ende");
                undEnde(event);
            }
        });
    }

    // #########################################################
    // Filmliste importieren
    // #########################################################
    public void filmeLaden(Daten daten, boolean manuell) {
        if (manuell || GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUS) {
            // Dialog zum Laden der Filme anzeigen
            DialogLeer dialog = new DialogLeer(daten.mediathekGui, true);
            dialog.init("Einstellungen zum Laden der Filme", new PanelFilmlisteLaden(daten, daten.mediathekGui, dialog));
            dialog.setVisible(true);
        } else {
            // Filme werden automatisch geladen
            importFilmliste("");
        }
    }

    public void importFilmliste(String dateiUrl) {
        importFilmliste(dateiUrl, false);
    }

    public void importFilmliste(String dateiUrl, boolean immerNeuLaden) {
        // damit wird die Filmliste geladen UND auch gleich im Konfig-Ordner gespeichert
        duration.start("Filme laden, start");
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            // Hash mit URLs füllen
            hashSet.clear();
            fillHash(Daten.listeFilme);
            if (immerNeuLaden) {
                // dann die alte löschen, damit immer komplett geladen wird, aber erst nach dem Hash!!
                Daten.listeFilme.clear(); // sonst wird eine "zu kurze" Liste wieder nur mit einer Diff-Liste aufgefüllt, wenn das Alter noch passt
            }
            Daten.listeFilmeNachBlackList.clear();
            System.gc();
            if (dateiUrl.equals("")) {
                // Filme als Liste importieren, Url automatisch ermitteln
                Log.systemMeldung("Aktuelle Filmliste laden");
                msImportFilmliste.filmeImportierenAuto(Daten.listeFilme, diffListe, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
            } else {
                // Filme als Liste importieren, feste URL/Datei
                Log.systemMeldung("Filmliste laden von: " + dateiUrl);
                Daten.listeFilme.clear();
                msImportFilmliste.filmeImportierenDatei(dateiUrl, Daten.listeFilme, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
            }
        }
    }

    public void updateFilmliste(String dateiUrl) {
        // damit wird die Filmliste mit einer weiteren aktualisiert (die bestehende bleibt
        // erhalten) UND auch gleich im Konfig-Ordner gespeichert
        duration.start("Filme laden (Update), start");
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            // Hash mit URLs füllen
            hashSet.clear();
            fillHash(Daten.listeFilme);
            //Daten.listeFilme.clear();
            Daten.listeFilmeNachBlackList.clear();
            System.gc();
            // Filme als Liste importieren, feste URL/Datei
            Log.systemMeldung("Filmliste laden von: " + dateiUrl);
            msImportFilmliste.filmeImportierenDatei(dateiUrl, diffListe, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
        }
    }

    // #######################################
    // #######################################
    public synchronized void setStop(boolean set) {
        MSConfig.setStop(set);
    }

    public String[] getSenderNamen() {
        return MSFilmeSuchen.getNamenSender();
    }

    public void updateDownloadUrlsFilmlisten(boolean akt, boolean diff) {
        msImportFilmliste.updateDownloadUrlsFilmlisten(akt, diff);
    }

    public ListeFilmlistenUrls getDownloadUrlsFilmlisten_akt() {
        return msImportFilmliste.msFilmlistenSuchen.listeFilmlistenUrls_akt;
    }

    public ListeFilmlistenUrls getDownloadUrlsFilmlisten_diff() {
        return msImportFilmliste.msFilmlistenSuchen.listeFilmlistenUrls_diff;
    }

    private void undEnde(MSListenerFilmeLadenEvent event) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird

        // wenn nur ein Update
        if (!diffListe.isEmpty()) {
            Daten.listeFilme.updateListe(diffListe, true/* Vergleich über Index, sonst nur URL */, true /*ersetzen*/);
            Daten.listeFilme.metaDaten = diffListe.metaDaten;
            Daten.listeFilme.sort(); // jetzt sollte alles passen
            diffListe.clear();
        }

        searchHash(Daten.listeFilme);
        Daten.listeFilme.themenLaden();
        Daten.listeAbo.setAboFuerFilm(Daten.listeFilme, false/*aboLoeschen*/);
        istAmLaufen = false;
        if (event.fehler) {
            Log.systemMeldung("Filmliste laden war fehlerhaft");
            MVMessageDialog.showMessageDialog(null, "Das Laden der Filmliste hat nicht geklappt!", "Fehler", JOptionPane.ERROR_MESSAGE);
            // dann die alte Liste wieder laden
            Daten.listeFilme.clear();
            new MSFilmlisteLesen().readFilmListe(Daten.getDateiFilmliste(), Daten.listeFilme, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
        } else {
            Log.systemMeldung("Filmliste geladen: " + Daten.listeFilme.size() + " Filme");
            Daten.filmlisteSpeichern();
        }
        MVListeFilme.checkBlacklist();
        notifyFertig(event);
        System.gc();
    }

    private void fillHash(ListeFilme listeFilme) {
        for (DatenFilm film : listeFilme) {
            hashSet.add(film.getUrlHistory());
        }
    }

    private void searchHash(ListeFilme listeFilme) {
        listeFilme.neueFilme = false;
        for (DatenFilm film : listeFilme) {
            if (!hashSet.contains(film.getUrlHistory())) {
                film.neuerFilm = true;
                listeFilme.neueFilme = true;
            } else {
                film.neuerFilm = false;
            }
        }
        hashSet.clear();
    }

    // ###########################
    // Listener
    // ###########################
    public void addAdListener(MSListenerFilmeLaden listener) {
        listeners.add(MSListenerFilmeLaden.class, listener);
    }

    private void notifyStart(MSListenerFilmeLadenEvent event) {
        for (MSListenerFilmeLaden l : listeners.getListeners(MSListenerFilmeLaden.class)) {
            run_(new Start(l, event, ListenerMelden.START));
        }
    }

    private void notifyProgress(MSListenerFilmeLadenEvent event) {
        for (MSListenerFilmeLaden l : listeners.getListeners(MSListenerFilmeLaden.class)) {
            run_(new Start(l, event, ListenerMelden.PROGRESS));
        }
    }

    private void notifyFertig(MSListenerFilmeLadenEvent event) {
        for (MSListenerFilmeLaden l : listeners.getListeners(MSListenerFilmeLaden.class)) {
            run_(new Start(l, event, ListenerMelden.FINISHED));
        }
    }

    private class Start implements Runnable {

        private final MSListenerFilmeLaden listenerFilmeLaden;
        private final MSListenerFilmeLadenEvent event;
        private final ListenerMelden listenerMelden;

        public Start(MSListenerFilmeLaden llistenerFilmeLaden, MSListenerFilmeLadenEvent eevent, ListenerMelden lliListenerMelden) {
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
            Log.fehlerMeldung(926369741, "ListenerFilmeLaden.run_", ex);
        }
    }
}
