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
package mediathek.controller.filme.filmeImportieren;

import javax.swing.JOptionPane;
import javax.swing.event.EventListenerList;
import mediathek.Daten;
import mediathek.Log;
import mediathek.controller.filme.FilmListener;
import mediathek.controller.filme.FilmListenerElement;
import mediathek.controller.filme.FilmeLaden;
import mediathek.controller.filme.filmeImportieren.filmUpdateServer.FilmUpdateServer;
import mediathek.controller.io.IoXmlFilmlisteLesen;
import mediathek.daten.ListeFilme;

public class FilmeImportieren {

    public ListeFilme listeFilme;
    public String[] filmlisteMetaDaten;
    private EventListenerList listeners = new EventListenerList();
    private IoXmlFilmlisteLesen ioXmlFilmlisteLesen = null;
    public FilmUpdateServer filmUpdateServer = new FilmUpdateServer();

    /**
     *
     * @param ddaten
     */
    public FilmeImportieren() {
        ioXmlFilmlisteLesen = new IoXmlFilmlisteLesen();
        ioXmlFilmlisteLesen.addAdListener(new BeobLaden());
    }

    // #######################################
    // Filme von Server/Datei importieren
    // #######################################
    public void filmeImportierenAuto() {
        new Thread(new filmeImportierenAutoThread()).start();
    }

    private class filmeImportierenAutoThread implements Runnable {

        @Override
        public synchronized void run() {
            //wenn auto-update-url dann erst mal die Updateserver aktualiseren laden
            boolean ret = false;
            FilmeLaden.updateUrl = filmUpdateServer.suchen();
            for (int i = 0; i < 10; ++i) {
                //10 mal mit einem anderen Server probieren
                if (urlLaden(FilmeLaden.updateUrl, true)) {
                    // hat geklappt, nix wie weiter
                    ret=true;
                    break;
                }
                FilmeLaden.updateUrl = filmUpdateServer.listeUpdateServer.getRand(i); //nächste Adresse in der Liste wählen
            }
             if (!ret /*listeFilme ist schon wieder null -> "FilmeLaden"*/) {
                Log.fehlerMeldung("Filme laden", "Es konnten keine Filme geladen werden!");
            }
        }
    }

    // #######################################
    // Filme aus Datei laden
    // #######################################
    public void filmeImportierenDatei(String pfad, boolean istUrl) {
        new Thread(new filmeImportierenDateiThread(pfad, istUrl)).start();
    }

    private class filmeImportierenDateiThread implements Runnable {

        String pfad;
        boolean istUrl;

        public filmeImportierenDateiThread(String ppfad, boolean iistUrl) {
            pfad = ppfad;
            istUrl = iistUrl;
        }

        @Override
        public synchronized void run() {
            if (!urlLaden(pfad, istUrl)) {
                JOptionPane.showMessageDialog(null, "Das Laden der Filmliste hat nicht geklappt!", "Fehler", JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    //===================================
    // private
    //===================================
    private boolean urlLaden(String dateiUrl, boolean istUrl) {
        boolean ret = false;
        ////////////////////////////
//////////        Log.systemMeldung("laden von: http://178.77.79.81/mediathek2/Mediathek_18.zip");
//////////        Log.systemMeldung("statt von: " + dateiUrl);
//////////        dateiUrl = "http://178.77.79.81/mediathek2/Mediathek_18.zip";
//////////        istUrl = true;
        ////////////////////////////
        try {
            if (dateiUrl.equals("")) {
                JOptionPane.showMessageDialog(null, "Keine Datei/Url angegeben", "Pfad", JOptionPane.INFORMATION_MESSAGE);
            } else {
                Log.systemMeldung("Filmliste laden von: " + dateiUrl);
                listeFilme = new ListeFilme();
                ret = ioXmlFilmlisteLesen.filmlisteLesen(dateiUrl, istUrl, listeFilme, Daten.getUserAgent());
            }
        } catch (Exception ex) {
            Log.fehlerMeldung("ImportListe.urlLaden: ", ex);
        }
        return ret;
    }

    // #######################################
    // Listener
    // #######################################
    public void addAdListener(FilmListener listener) {
        listeners.add(FilmListener.class, listener);
    }

    private class BeobLaden implements FilmListener {

        @Override
        public synchronized void start(FilmListenerElement filmListenerElement) {
            for (FilmListener l : listeners.getListeners(FilmListener.class)) {
                l.start(filmListenerElement);
            }
        }

        @Override
        public synchronized void progress(FilmListenerElement filmListenerElement) {
            for (FilmListener l : listeners.getListeners(FilmListener.class)) {
                l.progress(filmListenerElement);
            }
        }

        @Override
        public synchronized void fertig(FilmListenerElement filmListenerElement) {
            for (FilmListener l : listeners.getListeners(FilmListener.class)) {
                l.fertig(filmListenerElement);
            }
        }
    }
}
