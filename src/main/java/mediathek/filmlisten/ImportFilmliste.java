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
import mSearch.filmlisten.FilmlisteLesen;
import mSearch.filmlisten.FilmlistenSuchen;
import mSearch.tool.Log;

import javax.swing.event.EventListenerList;
import java.util.ArrayList;

public class ImportFilmliste {

    private final EventListenerList listeners;
    private final FilmlisteLesen msFilmlisteLesen;
    public FilmlistenSuchen msFilmlistenSuchen;

    public ImportFilmliste() {
        listeners = new EventListenerList();
        msFilmlisteLesen = new FilmlisteLesen();
        msFilmlistenSuchen = new FilmlistenSuchen();
        msFilmlisteLesen.addAdListener(new ListenerFilmeLaden() {
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

    // #########################################################
    // Filmeliste importieren, URL automatisch wählen
    // #########################################################
    public void filmeImportierenAuto(ListeFilme listeFilme, ListeFilme listeFilmeDiff, int days) {
        Config.setStop(false);
        new Thread(new FilmeImportierenAutoThread(listeFilme, listeFilmeDiff, days)).start();
    }

    public enum STATE {AKT, DIFF}

    private class FilmeImportierenAutoThread implements Runnable {
        private final ListeFilme listeFilme;
        private final ListeFilme listeFilmeDiff;
        private STATE state;
        private final int days;

        public FilmeImportierenAutoThread(ListeFilme listeFilme, ListeFilme listeFilmeDiff, int days) {
            this.listeFilme = listeFilme;
            this.listeFilmeDiff = listeFilmeDiff;
            this.days = days;
        }

        @Override
        public void run() {
            boolean ret;
            if (listeFilme.isTooOldForDiff()) {
                // dann eine komplette Liste laden
                state = STATE.AKT;
                listeFilme.clear();
                ret = suchenAktListe(listeFilme);
            } else {
                // nur ein Update laden
                state = STATE.DIFF;
                ret = suchenAktListe(listeFilmeDiff);
                if (!ret || listeFilmeDiff.isEmpty()) {
                    // wenn diff, dann nochmal mit einer kompletten Liste versuchen
                    state = STATE.AKT;
                    listeFilme.clear();
                    listeFilmeDiff.clear();
                    ret = suchenAktListe(listeFilme);
                }
            }
            if (!ret /* listeFilme ist schon wieder null -> "FilmeLaden" */) {
                Log.errorLog(951235497, "Es konnten keine Filme geladen werden!");
            }
            fertigMelden(ret);
        }

        private boolean suchenAktListe(ListeFilme liste) {
            boolean ret = false;
            ArrayList<String> versuchteUrls = new ArrayList<>();
            String updateUrl = "";

            switch (state) {
                case AKT:
                    updateUrl = msFilmlistenSuchen.suchenAkt(versuchteUrls);
                    break;
                case DIFF:
                    updateUrl = msFilmlistenSuchen.suchenDiff(versuchteUrls);
                    break;
            }

            if (updateUrl.isEmpty()) {
                return false;
            }

            // 5 mal mit einem anderen Server probieren, wenns nicht klappt
            final int maxRetries = state == STATE.DIFF ? 2 : 5; //bei diff nur 2x probieren, dann eine akt-liste laden
            for (int i = 0; i < maxRetries; ++i) {
                ret = urlLaden(updateUrl, liste, days);
                if (ret && i < 1 && liste.isOlderThan(5 * 60 * 60 /*sekunden*/)) {
                    // Laden hat geklappt ABER: Liste zu alt, dann gibts einen 2. Versuch
                    Log.sysLog("Filmliste zu alt, neuer Versuch");
                    ret = false;
                }

                if (ret) {
                    // hat geklappt, nix wie weiter
                    return true;
                }

                switch (state) {
                    case AKT:
                        updateUrl = msFilmlistenSuchen.listeFilmlistenUrls_akt.getRand(versuchteUrls); //nächste Adresse in der Liste wählen
                        break;
                    case DIFF:
                        updateUrl = msFilmlistenSuchen.listeFilmlistenUrls_diff.getRand(versuchteUrls); //nächste Adresse in der Liste wählen
                        break;
                }
                versuchteUrls.add(updateUrl);
                // nur wenn nicht abgebrochen, weitermachen
                if (Config.getStop()) {
                    break;
                }

            }
            return ret;
        }
    }

    // #######################################
    // Filmeliste importieren, mit fester URL/Pfad
    // #######################################
    public void filmeImportierenDatei(String pfad, ListeFilme listeFilme, int days) {
        Config.setStop(false);
        new Thread(new FilmeImportierenDateiThread(pfad, listeFilme, days)).start();

    }

    private class FilmeImportierenDateiThread implements Runnable {

        private final String pfad;
        private final ListeFilme listeFilme;
        private final int days;

        public FilmeImportierenDateiThread(String pfad, ListeFilme listeFilme, int days) {
            this.pfad = pfad;
            this.listeFilme = listeFilme;
            this.days = days;
        }

        @Override
        public void run() {
            fertigMelden(urlLaden(pfad, listeFilme, days));
        }
    }

    // #######################################
    // #######################################
    public void addAdListener(ListenerFilmeLaden listener) {
        listeners.add(ListenerFilmeLaden.class, listener);
    }

    public void updateDownloadUrlsFilmlisten(boolean akt) {
        msFilmlistenSuchen.updateURLsFilmlisten(akt);
    }

    private boolean urlLaden(String dateiUrl, ListeFilme listeFilme, int days) {
        boolean ret = false;
        try {
            if (!dateiUrl.equals("")) {
                Log.sysLog("Filmliste laden von: " + dateiUrl);
                msFilmlisteLesen.readFilmListe(dateiUrl, listeFilme, days);
                if (!listeFilme.isEmpty()) {
                    ret = true;
                }
            }
        } catch (Exception ex) {
            Log.errorLog(965412378, ex);
        }
        return ret;

    }

    private synchronized void fertigMelden(boolean ok) {
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.fertig(new ListenerFilmeLadenEvent("", "", 0, 0, 0, !ok));
        }
    }
}
