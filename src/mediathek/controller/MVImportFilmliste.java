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

import java.util.ArrayList;
import javax.swing.JOptionPane;
import javax.swing.event.EventListenerList;
import mediathek.tool.MVMessageDialog;
import msearch.daten.ListeFilme;
import msearch.daten.MSearchConfig;
import msearch.filmeLaden.ListeDownloadUrlsFilmlisten;
import msearch.filmeLaden.ListeFilmlistenServer;
import msearch.filmeLaden.MSearchFilmlistenSuchen;
import msearch.filmeSuchen.MSearchListenerFilmeLaden;
import msearch.filmeSuchen.MSearchListenerFilmeLadenEvent;
import msearch.io.MSearchFilmlisteLesen;
import msearch.tool.MSearchLog;

public class MVImportFilmliste {

    private EventListenerList listeners = new EventListenerList();
    private MSearchFilmlisteLesen ioXmlFilmlisteLesen = null;
    public MSearchFilmlistenSuchen filmlistenSuchen = new MSearchFilmlistenSuchen();

    public MVImportFilmliste() {
        ioXmlFilmlisteLesen = new MSearchFilmlisteLesen();
        ioXmlFilmlisteLesen.addAdListener(new MSearchListenerFilmeLaden() {
            @Override
            public synchronized void start(MSearchListenerFilmeLadenEvent event) {
                for (MSearchListenerFilmeLaden l : listeners.getListeners(MSearchListenerFilmeLaden.class)) {
                    l.start(event);
                }
            }

            @Override
            public synchronized void progress(MSearchListenerFilmeLadenEvent event) {
                for (MSearchListenerFilmeLaden l : listeners.getListeners(MSearchListenerFilmeLaden.class)) {
                    l.progress(event);

                }
            }

            @Override
            public synchronized void fertig(MSearchListenerFilmeLadenEvent event) {
            }
        });
    }

    public void addAdListener(MSearchListenerFilmeLaden listener) {
        listeners.add(MSearchListenerFilmeLaden.class, listener);
    }

    // #######################################
    // Filme von Server/Datei importieren
    // #######################################
    public ListeDownloadUrlsFilmlisten getDownloadUrls_Filmlisten(boolean update) {
        if (update) {
            filmlistenSuchen.suchen(null);
        }
        return filmlistenSuchen.listeDownloadUrlsFilmlisten;
    }

    public ListeFilmlistenServer getListe_FilmlistenServer() {
        return filmlistenSuchen.listeFilmlistenServer;
    }

    // #########################################################
    // Filmeliste importieren, URL automatisch wählen
    // #########################################################
    public void filmeImportierenAuto(String dateiZiel, ListeFilme listeFilme) {
        MSearchConfig.setStop(false);
        new Thread(new FilmeImportierenAutoThread(dateiZiel, listeFilme)).start();
    }

    private class FilmeImportierenAutoThread implements Runnable {

        private ListeFilme listeFilme;
        private String ziel;

        public FilmeImportierenAutoThread(String dateiZiel, ListeFilme llisteFilme) {
            ziel = dateiZiel;
            listeFilme = llisteFilme;
        }

        @Override
        public void run() {
            //wenn auto-update-url dann erst mal die Updateserver aktualiseren
            boolean ret = false;
            ArrayList<String> versuchteUrls = new ArrayList<>();
            String updateUrl = filmlistenSuchen.suchen(versuchteUrls);
            if (!updateUrl.equals("")) {
                for (int i = 0; i < 5; ++i) {
                    //5 mal mit einem anderen Server probieren
                    if (urlLaden(updateUrl, ziel, listeFilme)) {
                        // hat geklappt, nix wie weiter
                        ret = true; // keine Fehlermeldung
                        if (i < 3 && listeFilme.filmlisteIstAelter(5 * 60 * 60 /*sekunden*/)) {
                            MSearchLog.systemMeldung("Filmliste zu alt, neuer Versuch");
                        } else {
                            // 3 Versuche mit einer alten Liste sind genug
                            break;
                        }
                    } else {
                        // nur wenn nicht abgebrochen, weitermachen
                        if (MSearchConfig.getStop()) {
                            break;
                        }
                    }
                    updateUrl = filmlistenSuchen.listeDownloadUrlsFilmlisten.getRand(versuchteUrls, i); //nächste Adresse in der Liste wählen
                    versuchteUrls.add(updateUrl);
                }
            }
            if (!ret /* listeFilme ist schon wieder null -> "FilmeLaden" */) {
                MVMessageDialog.showMessageDialog(null, "Das Laden der Filmliste hat nicht geklappt!", "Fehler", JOptionPane.ERROR_MESSAGE);
                MSearchLog.fehlerMeldung(951235497, MSearchLog.FEHLER_ART_PROG, "Filme laden", "Es konnten keine Filme geladen werden!");
            }
            fertigMelden();
        }
    }

    // #######################################
    // Filmeliste importieren, mit fester URL/Pfad
    // #######################################
    public void filmeImportierenDatei(String pfad, String dateiZiel, ListeFilme listeFilme) {
        MSearchConfig.setStop(false);
        new Thread(new FilmeImportierenDateiThread(pfad, dateiZiel, listeFilme)).start();
    }

    private class FilmeImportierenDateiThread implements Runnable {

        private String pfad;
        private String ziel;
        private ListeFilme listeFilme;

        public FilmeImportierenDateiThread(String ppfad, String dateiZiel, ListeFilme llisteFilme) {
            pfad = ppfad;
            ziel = dateiZiel;
            listeFilme = llisteFilme;
        }

        @Override
        public void run() {
            if (!urlLaden(pfad, ziel, listeFilme)) {
                MVMessageDialog.showMessageDialog(null, "Das Laden der Filmliste hat nicht geklappt!", "Fehler", JOptionPane.ERROR_MESSAGE);
            }
            fertigMelden();
        }
    }

    //===================================
    // private
    //===================================
    private boolean urlLaden(String dateiUrl, String dateiZiel, ListeFilme listeFilme) {
        boolean ret = false;
        try {
            if (!dateiUrl.equals("")) {
                MSearchLog.systemMeldung("Filmliste laden von: " + dateiUrl);
                ret = ioXmlFilmlisteLesen.filmlisteLesenJson(dateiUrl, dateiZiel, listeFilme);
            }
        } catch (Exception ex) {
            MSearchLog.fehlerMeldung(965412378, MSearchLog.FEHLER_ART_PROG, "ImportListe.urlLaden: ", ex);
        }
        return ret;
    }

    private synchronized void fertigMelden() {
        for (MSearchListenerFilmeLaden l : listeners.getListeners(MSearchListenerFilmeLaden.class)) {
            l.fertig(new MSearchListenerFilmeLadenEvent("", "", 0, 0));
        }
    }
}
