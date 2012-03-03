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
package mediathek.controller.filme;

import java.util.Iterator;
import javax.swing.event.EventListenerList;
import mediathek.Konstanten;
import mediathek.controller.filme.filmeImportieren.FilmeImportieren;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.controller.filme.filmeImportieren.filmUpdateServer.ListeFilmUpdateServer;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.controller.filme.filmeSuchen.sender.MediathekReader;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DDaten;
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
    private FilmeSuchen filmeSuchen;
    private FilmeImportieren filmeImportieren;
    private EventListenerList listeners = new EventListenerList();
    private boolean istAmLaufen = false;

    public FilmeLaden() {
        filmeSuchen = new FilmeSuchen();
        filmeImportieren = new FilmeImportieren();
        filmeSuchen.addAdListener(new BeobLadenSuchen());
        filmeImportieren.addAdListener(new BeobLadenImportieren());
    }

    public void filmeLaden(DDaten daten) {
        if (GuiFunktionen.getImportArtFilme() == FilmeLaden.UPDATE_FILME_AUS) {
            // ImportDialog starten zum Auswählen der URL
            filmlisteImportieren(DDaten.system[Konstanten.SYSTEM_IMPORT_URL_MANUELL_NR]);
        } else {
            filmlisteImportierenAuto();
        }
    }

    public synchronized void setStop() {
        stop = true;
    }

    public synchronized boolean getStop() {
        return stop;
    }

    public synchronized void resetStop() {
        stop = false;
    }

    public ListeFilmUpdateServer getListeFilmUpdateServer(boolean update) {
        if (update) {
            filmeImportieren.filmUpdateServer.suchen();
        }
        return filmeImportieren.filmUpdateServer.listeUpdateServer;
    }

    public int getSeitenGeladen() {
        return GetUrl.getSeitenZaehler();
    }

    public ListeFilme getListeFilme() {
        if (listeFilmeNeu != null) {
            return listeFilmeNeu;
        } else {
            return new ListeFilme();
        }
    }

    public String[] getSenderNamen() {
        String[] ret = new String[filmeSuchen.mediathekListe.size()];
        Iterator<MediathekReader> it = filmeSuchen.mediathekListe.iterator();
        int i = 0;
        while (it.hasNext()) {
            ret[i] = it.next().getSenderName();
            ++i;
        }
        return ret;
    }

    // #########################################################
    // Filme als Liste importieren, Url automatisch ermitteln
    // #########################################################
    public synchronized void filmlisteImportierenAuto() {
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            stop = false;
            filmeImportieren.filmeImportierenAuto();
        }
    }

    // #################################################
    // Filme als Liste importieren, feste URL/Datei
    // #################################################
    public synchronized void filmlisteImportieren(String dateiUrl) {
        boolean istUrl;
        istUrl = dateiUrl.startsWith("http") ? true : false || dateiUrl.startsWith("www") ? true : false;
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            stop = false;
            filmeImportieren.filmeImportierenDatei(dateiUrl, istUrl);
        }
    }

    public synchronized void filmlisteDirektImportieren(String dateiUrl) {
        boolean istUrl;
        istUrl = dateiUrl.startsWith("http") ? true : false || dateiUrl.startsWith("www") ? true : false;
        filmeImportieren.filmeDirektImportierenDatei(dateiUrl, istUrl);
    }

    // #######################################
    // Filme beim allen Sender laden
    // #######################################
    public void filmeBeimSenderSuchen(ListeFilme llisteFilme, boolean allesLaden) {
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            stop = false;
            listeFilmeAlt = llisteFilme;
            filmeSuchen.filmeBeimSenderLaden(allesLaden, listeFilmeAlt);
        }
    }

    // ###########################################
    // Filme bei EINEM Sender laden (nur update)
    // ###########################################
    public void updateSender(String sender, ListeFilme llisteFilme) {
        if (!istAmLaufen) {
            // nicht doppelt starten
            istAmLaufen = true;
            stop = false;
            listeFilmeAlt = llisteFilme;
            filmeSuchen.updateSender(sender, llisteFilme);
        }
    }

    // ###########################
    // Listener
    // ###########################
    public void addListener(MediathekListener listener) {
        listeners.add(MediathekListener.class, listener);
    }

    public void addAdListener(FilmListener listener) {
        listeners.add(FilmListener.class, listener);
    }

    public void notifyStart(FilmListenerElement filmListenerElement) {
        for (FilmListener l : listeners.getListeners(FilmListener.class)) {
            l.start(filmListenerElement);
        }
    }

    public void notifyProgress(FilmListenerElement filmListenerElement) {
        for (FilmListener l : listeners.getListeners(FilmListener.class)) {
            l.progress(filmListenerElement);
        }
    }

    public void notifyFertig(FilmListenerElement filmListenerElement) {
        for (FilmListener l : listeners.getListeners(FilmListener.class)) {
            l.fertig(filmListenerElement);
        }
    }

    private class BeobLadenSuchen implements FilmListener {

        @Override
        public synchronized void start(FilmListenerElement filmListenerElement) {
            notifyStart(filmListenerElement);
        }

        @Override
        public synchronized void progress(FilmListenerElement filmListenerElement) {
            notifyProgress(filmListenerElement);
        }

        @Override
        public synchronized void fertig(FilmListenerElement filmListenerElement) {
            // Ergebnisliste listeFilme eintragen -> Feierabend!
            listeFilmeNeu = filmeSuchen.listeFilmeNeu;
            filmeSuchen.listeFilmeNeu = null;
            istAmLaufen = false;
            notifyFertig(filmListenerElement);
        }
    }

    private class BeobLadenImportieren implements FilmListener {

        @Override
        public synchronized void start(FilmListenerElement filmListenerElement) {
            notifyStart(filmListenerElement);
        }

        @Override
        public synchronized void progress(FilmListenerElement filmListenerElement) {
            notifyProgress(filmListenerElement);
        }

        @Override
        public synchronized void fertig(FilmListenerElement filmListenerElement) {
            // Ergebnisliste listeFilme eintragen -> Feierabend!
            listeFilmeNeu = filmeImportieren.listeFilme;
            filmeImportieren.listeFilme = null;
            istAmLaufen = false;
            notifyFertig(filmListenerElement);
        }
    }
}
