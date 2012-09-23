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
package mediathek.daten;

import java.io.File;
import javax.swing.event.EventListenerList;
import mediathek.controller.filmeLaden.FilmeLaden;
import mediathek.controller.io.IoXmlFilmlisteLesen;
import mediathek.controller.io.IoXmlFilmlisteSchreiben;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class Daten {

    // Konstanten
    // Systemeinstellungen
    public static String[] system = new String[Konstanten.SYSTEM_MAX_ELEM];
    // flags
    public static boolean debug = false;
    public static boolean nogui = false;
    public static boolean auto = false;
    private static boolean geaendert = false;
    private static String basisverzeichnis = "";
    // Klassen
    public static FilmeLaden filmeLaden;
    public static IoXmlFilmlisteLesen ioXmlFilmlisteLesen = null;
    public static ListeFilme listeFilme = null;
    private static EventListenerList listeners = new EventListenerList();
    public static final String LINE_SEPARATOR = System.getProperty("line.separator");

    public Daten(String pfad) {
        basisverzeichnis = pfad;
        init();
    }

    private void init() {
        for (int i = 0; i < system.length; ++i) {
            system[i] = "";
        }
        //initialisieren
        system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR] = "1";
        system[Konstanten.SYSTEM_WARTEN_NR] = "1";
        system[Konstanten.SYSTEM_USER_AGENT_NR] = Konstanten.USER_AGENT_DEFAULT;
        system[Konstanten.SYSTEM_WARTEN_NR] = "1";
        system[Konstanten.SYSTEM_LOOK_NR] = "0";
        system[Konstanten.SYSTEM_VERSION_NR] = Konstanten.VERSION;
        system[Konstanten.SYSTEM_UPDATE_SUCHEN_NR] = Boolean.TRUE.toString();
        system[Konstanten.SYSTEM_ABOS_SOFORT_SUCHEN_NR] = Boolean.TRUE.toString();
        listeFilme = new ListeFilme();
        ioXmlFilmlisteLesen = new IoXmlFilmlisteLesen();
        filmeLaden = new FilmeLaden();
    }

    public synchronized static void addAdListener(ListenerMediathekView listener) {
        listeners.add(ListenerMediathekView.class, listener);
    }

    public synchronized static void notifyMediathekListener(int ereignis, String klasse) {
        for (ListenerMediathekView l : listeners.getListeners(ListenerMediathekView.class)) {
            if (l.ereignis == ereignis) {
                if (!l.klasse.equals(klasse)) {
                    // um einen Kreislauf zu verhindern
                    try {
                        l.pingGui();
                    } catch (Exception ex) {
                        Log.fehlerMeldung(562314008, "Daten.notifyMediathekListener", ex);
                    }
                }
            }
        }
    }

    public static void setUserAgentAuto() {
        system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR] = Boolean.TRUE.toString();
    }

    public static void setUserAgentManuel(String ua) {
        system[Konstanten.SYSTEM_USER_AGENT_AUTO_NR] = Boolean.FALSE.toString();
        system[Konstanten.SYSTEM_USER_AGENT_NR] = ua;
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

    // geändert
    public void setGeaendertPanel() {
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

    private static String getBasisVerzeichnis(String basis, boolean anlegen) {
        String ret;
        if (basis.equals("")) {
            ret = System.getProperty("user.home") + File.separator + Konstanten.VERZEICHNISS_EINSTELLUNGEN + File.separator;
        } else {
            ret = basis;
        }
        if (anlegen) {
            File basisF = new File(ret);
            if (!basisF.exists()) {
                if (!basisF.mkdir()) {
                    Log.fehlerMeldung(898736548, "Daten.getBasisVerzeichnis", new String[]{"Kann den Ordner zum Speichern der Daten nicht anlegen!",
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
        new IoXmlFilmlisteSchreiben().filmeSchreiben(getBasisVerzeichnis(true) + Konstanten.XML_DATEI_FILME, listeFilme);
    }

    public void allesAbbrechen() {
    }
}
