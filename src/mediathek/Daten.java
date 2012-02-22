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
package mediathek;

import java.io.File;
import javax.swing.event.EventListenerList;
import mediathek.controller.filme.FilmeLaden;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.controller.io.IoXmlFilmlisteLesen;
import mediathek.controller.io.IoXmlFilmlisteSchreiben;
import mediathek.daten.ListeFilme;

public class Daten {

    // Konstanten
    public final static int GESTARTET_ALS_GUI = 1; // Programm läuft als GUI-Version
    public final static int GESTARTET_ALS_AUTO = 2; // als auto
    public final static int GESTARTET_ALS_NOGUI = 3; // als noGui nur zum Laden der Filmliste
    // Systemeinstellungen
    public static String[] system = new String[Konstanten.SYSTEM_MAX_ELEM];
    // flags
    public static boolean debug = false;
    public static boolean fehlerFensterAnzeigen = true;
    private static boolean geaendert;
    private static String basisverzeichnis;
    // Klassen
    public static FilmeLaden filmeLaden;
    public static IoXmlFilmlisteLesen ioXmlFilmlisteLesen = null;
    public static IoXmlFilmlisteSchreiben ioXmlFilmlisteSchreiben = null;
    public static ListeFilme listeFilme = null;
    private static EventListenerList listeners = new EventListenerList();

    public Daten(String basis) {
        basisverzeichnis = basis;
        // einrichten der statischen
        system = new String[Konstanten.SYSTEM_MAX_ELEM];
        system = new String[Konstanten.SYSTEM_MAX_ELEM];
        for (int i = 0; i < system.length; ++i) {
            system[i] = "";
        }
        // der Rest
        geaendert = false;
        //zur Info, Basisverzeichnis für die Einstellungen
        Log.systemMeldung("Programmeinstellungen: " + getBasisVerzeichnis());
        //initialisieren
        system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR] = "1";
        system[Konstanten.SYSTEM_WARTEN_NR] = "1";
        system[Konstanten.SYSTEM_USER_AGENT_NR] = Konstanten.USER_AGENT_DEFAULT;
        if (system[Konstanten.SYSTEM_WARTEN_NR].equals("")) {
            system[Konstanten.SYSTEM_WARTEN_NR] = "1";
        }
        if (system[Konstanten.SYSTEM_LOOK_NR].equals("")) {
            system[Konstanten.SYSTEM_LOOK_NR] = "1";
        }
        //Version prüfen+++++++++++++++++++++++
        if (!system[Konstanten.SYSTEM_VERSION_NR].equals(Konstanten.VERSION)) {
            //Version setzten
            system[Konstanten.SYSTEM_VERSION_NR] = Konstanten.VERSION;
        }
        //Version prüfen+++++++++++++++++++++++

        listeFilme = new ListeFilme();
        ioXmlFilmlisteLesen = new IoXmlFilmlisteLesen();
        ioXmlFilmlisteSchreiben = new IoXmlFilmlisteSchreiben();
        filmeLaden = new FilmeLaden();
        //updateListe aufbauen
//        filmeLaden.getListeFilmUpdateServer(false).addWithCheck(new DatenFilmUpdateServer("http://178.77.79.81/mediathek4/Mediathek_14.bz2", "1"));
//        filmeLaden.getListeFilmUpdateServer(false).addWithCheck(new DatenFilmUpdateServer("http://178.77.79.81/mediathek3/Mediathek_10.bz2", "1"));
//        filmeLaden.getListeFilmUpdateServer(false).addWithCheck(new DatenFilmUpdateServer("http://178.77.79.81/mediathek2/Mediathek_08.zip", "1"));
////////            filmeLaden.getListeFilmUpdateServer(false).addWithCheck(new DatenFilmUpdateServer("http://mediathekview.xml.in/mediathek-1-13.bz2", "1"));
////////            filmeLaden.getListeFilmUpdateServer(false).addWithCheck(new DatenFilmUpdateServer("http://mediathek.000a.de/filme/Mediathek_15.bz2", "1"));
////////            filmeLaden.getListeFilmUpdateServer(false).addWithCheck(new DatenFilmUpdateServer("http://178.77.79.81/mediathek1/Mediathek_15.bz2", "1"));
////////            filmeLaden.getListeFilmUpdateServer(false).addWithCheck(new DatenFilmUpdateServer("http://mitglied.multimania.de/mediathekview/mediathek-1-13.bz2", "1"));


    }
    // userAgent

    public synchronized static void addAdListener(MediathekListener listener) {
        listeners.add(MediathekListener.class, listener);
    }

    public synchronized static void notifyMediathekListener(int ereignis, String klasse) {
        for (MediathekListener l : listeners.getListeners(MediathekListener.class)) {
            if (l.ereignis == ereignis) {
                if (!l.klasse.equals(klasse)) {
                    // um einen Kreislauf zu verhindern
                    l.ping();
                }
            }
        }
    }

    public static boolean isUserAgentAuto() {
        if (system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR].equals("")) {
            system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR] = Boolean.TRUE.toString();
            return true;
        } else {
            return Boolean.parseBoolean(system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR]);
        }
    }

    public static String getUserAgent() {
        if (isUserAgentAuto()) {
            return Konstanten.USER_AGENT_DEFAULT;
        } else {
            return system[Konstanten.SYSTEM_USER_AGENT_NR];
        }
    }

    public static void setUserAgentAuto() {
        system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR] = Boolean.TRUE.toString();
    }

    public static void setUserAgentManuel(String ua) {
        system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR] = Boolean.FALSE.toString();
        system[Konstanten.SYSTEM_USER_AGENT_NR] = ua;
    }

    // geändert
    public void setGeaendertPanel() {
        geaendert = true;
    }

    public void setGeaendertPanelSofort() {
        geaendert = true;
    }

    public static void setGeaendert() {
        geaendert = true;
    }

    public static boolean isGeaendert() {
        return geaendert;
    }

    public static String getBasisVerzeichnis() {
        return getBasisVerzeichnis(false);
    }

    public static String getBasisVerzeichnis(boolean anlegen) {
        return getBasisVerzeichnis(basisverzeichnis, anlegen);
    }

    public static String getBasisVerzeichnis(String basis, boolean anlegen) {
        String ret;
        if (basis.equals("")) {
            ret = System.getProperty("user.home") + File.separator + Konstanten.MEDIATHEK_VIEW_VERZEICHNISS + File.separator;
        } else {
            ret = basis;
        }
        if (anlegen) {
            File basisF = new File(ret);
            if (!basisF.exists()) {
                if (!basisF.mkdir()) {
                    Log.fehlerMeldung("Daten.getBasisVerzeichnis", new String[]{"Kann den Ordner zum Speichern der Daten nicht anlegen!",
                                "Daten.getBasisVerzeichnis"});
                }

            }
        }
        return ret;
    }

    public void allesLaden() {
        ioXmlFilmlisteLesen.filmlisteLesen(getBasisVerzeichnis() + Konstanten.XML_DATEI_FILME, false /* istUrl */, listeFilme);
    }

    public void allesSpeichern() {
        ioXmlFilmlisteSchreiben.filmeSchreiben(getBasisVerzeichnis(true) + Konstanten.XML_DATEI_FILME, listeFilme);
    }

    public void allesAbbrechen() {
    }
}
