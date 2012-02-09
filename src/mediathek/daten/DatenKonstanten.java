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

import java.awt.Color;

public class DatenKonstanten {

    public static final String VERSION = "2.6.0";
    public static final String PROGRAMMNAME = "MediathekView";
    ////////////////////////////////
    public static final String ADRESSE_UPDATE = "http://zdfmediathk.sourceforge.net/update.xml";
    //public static final String ADRESSE_UPDATE = "http://zdfmediathk.sourceforge.net/update-test.xml";
    public static final String ADRESSE_DOWNLAD = "http://sourceforge.net/projects/zdfmediathk/";
    public static final String ADRESSE_ANLEITUNG_UPDATE = "http://zdfmediathk.sourceforge.net/update.html";
    public static final String TextUpdateSuchen = "Eimal am Tag nach einer neuen Programmversion suchen";
    //Sender
    public static final String SENDER_PREFX = "xml"; //für den XML-Standard
    public static final String[][] THEME = {{"", ""},//das muss da sein
        {"System", ""}, // und das auch!!
        {"Metal", "javax.swing.plaf.metal.MetalLookAndFeel"},
        {"Nimbus", "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel"},
        {"GTK-Linux", "com.sun.java.swing.plaf.gtk.GTKLookAndFeel"}
    };
    // Dateien/Verzeichnisse
    public static final String MEDIATHEK_VIEW_VERZEICHNISS = ".mediathek3";
    public static final String LOG_DATEI_DOWNLOADS = "downloads.txt";
    public static final String LOG_DATEI_HISTORY = "history.txt";
    public static final String XML_DATEI = "mediathek.xml";
    public static final String XML_DATEI_FILME = "filme.xml";
    // 
    public static final int MIN_DATEI_GROESSE_KB = 2; //minimale Größe (kb !!!) eines Films um nicht als Fehler zu gelten
    public static final String KODIERUNG_UTF = "UTF-8";
    public static final String KODIERUNG_ISO15 = "ISO-8859-15";
    public static final String XML_START = "Mediathek";
    public static final String LEITUNG_DSL1000 = "dsl1000";
    public static final String LEITUNG_DSL2000 = "dsl2000";
    public static final String IMPORT_WEBSITE = "import-website";
    public static final String IMPORT_URL = "import-url";
    public static final String IMPORT_DATEI = "import-datei";
    public static final String IMPORT_WEBSITE_TEXT = "Filme laden (Website)";
    public static final String IMPORT_URL_TEXT = "Filme laden (Url)";
    public static final String IMPORT_DATEI_TEXT = "Filme laden (Datei)";
    public static final String DIREKTE_DOWNLOAD_SUFFIX = "mp4,mp3,m4v,flv";
    public static final String DIREKTE_DOWNLOAD_PRAEFIX = "http";
    //Formate zu Zippen
    public static final String FORMAT_ZIP = ".zip";
    public static final String FORMAT_BZ2 = ".bz2";
    // für das Anpassen der URL für den flvstreamer
    public static final String RTMP_FLVSTREAMER = "-r ";
    public static final String RTMP_PRTOKOLL = "rtmp";
    // Auto-Neu-Laden
    public static final int NEU_LADEN_IN = 75 * 60; // Sekunden
    public static final int MAX_SENDER_FILME_LADEN = 2;//es können maximal soviele Filme eines Senders gleichzeitig geladen werden
    //
    public static final String USER_AGENT_DEFAULT = PROGRAMMNAME + " " + VERSION;
    //
    public static final int UPDATE_FILME_AUS = 0; // nix
    public static final int UPDATE_FILME_URL = 1; // manuell laden, Url automatisch wählen
    public static final int UPDATE_FILME_AUTO = 2; // beim Start, immer mal wieder, + Url auto
    //
    public static Color DOWNLOAD_FARBE_WAIT = new Color(239, 244, 255);
    public static Color DOWNLOAD_FARBE_WAIT_SEL = new Color(199, 206, 222);
    public static Color DOWNLOAD_FARBE_RUN = new Color(241, 228, 188);
    public static Color DOWNLOAD_FARBE_RUN_SEL = new Color(206, 178, 92);
    public static Color DOWNLOAD_FARBE_FERTIG = new Color(188, 241, 195);
    public static Color DOWNLOAD_FARBE_FERTIG_SEL = new Color(115, 206, 92);
    public static Color DOWNLOAD_FARBE_ERR = new Color(241, 188, 221);
    public static Color DOWNLOAD_FARBE_ERR_SEL = new Color(206, 92, 128);    //
    public static Color DOWNLOAD_FARBE_ABO_EINMAL = new Color(80, 80, 80);    //
    public static Color DOWNLOAD_FARBE_LIVE = new Color(130, 0, 0);    //
    public static Color DOWNLOAD_FARBE_GESEHEN = new Color(225, 225, 225);    //
    public static Color DOWNLOAD_FARBE_GESEHEN_SEL = new Color(190, 190, 190);    //
    //Tags System
    public static final String SYSTEM = "System";
    public static final int SYSTEM_MAX_ELEM = 33;
    public static final String SYSTEM_FEHLER_ANZEIGEN = "Fehler_anzeigen";
    public static final int SYSTEM_FEHLER_ANZEIGEN_NR = 0;
    public static final String SYSTEM_LOOK = "System-look";
    public static final int SYSTEM_LOOK_NR = 1;
    public static final String SYSTEM_START_MAX = "Start-maximiert";
    public static final int SYSTEM_START_MAX_NR = 2;
    public static final String SYSTEM_USER_AGENT = "User-Agent";
    public static final int SYSTEM_USER_AGENT_NR = 3;
    public static final String SYSTEM_VERSION = "version";
    public static final int SYSTEM_VERSION_NR = 4;
    public static final String SYSTEM_GROESSE_X = "GroesseX";
    public static final int SYSTEM_GROESSE_X_NR = 5;
    public static final String SYSTEM_GROESSE_Y = "GroesseY";
    public static final int SYSTEM_GROESSE_Y_NR = 6;
    //abo
    public static final String SYSTEM_ABO = "Abo";
    public static final int SYSTEM_ABO_NR = 7;
    public static final String SYSTEM_NUR_ABO = "nur_abo";
    public static final int SYSTEM_NUR_ABO_NR = 8;
    public static final String SYSTEM_USER_AGENT_MANUEL = "user-agent-manuel";
    public static final int SYSTEM_USER_AGENT_MANUEL_NR = 9;
    //proxy
    public static final String SYSTEM_PROXY_SERVER = "proxy-server";
    public static final int SYSTEM_PROXY_SERVER_NR = 10;
    public static final String SYSTEM_PROXY_PORT = "proxy-port";
    public static final int SYSTEM_PROXY_PORT_NR = 11;
    public static final String SYSTEM_PROXY_USER = "proxy-benutzer";
    public static final int SYSTEM_PROXY_USER_NR = 12;
    public static final String SYSTEM_PROXY_PWD = "proxy-passwort";
    public static final int SYSTEM_PROXY_PWD_NR = 13;
    public static final String SYSTEM_PROXY_ON = "proxy-an";
    public static final int SYSTEM_PROXY_ON_NR = 14;
    //Einstellungen Sender
    public static final String SYSTEM_MAX_DOWNLOAD = "maxDownload";
    public static final int SYSTEM_MAX_DOWNLOAD_NR = 15;
    public static final String SYSTEM_LEITUNG_LOW = "Leitung";
//    public static final int SYSTEM_LEITUNG_LOW_NR = 16;
    public static final String SYSTEM_RTMP_FLVSTREAMER = "Rtmp-bauen";
//    public static final int SYSTEM_RTMP_FLVSTREAMER_NR = 17;
    public static final String SYSTEM_SWR_LISTE = "nr-in-liste";
    public static final int SYSTEM_SWR_LISTE_NR = 18;
    //Einstellungen Im-Export
    public static final String SYSTEM_PFAD_EXPORT_ABOS = "Pfad_Export_Abos";
    public static final int SYSTEM_PFAD_EXPORT_ABOS_NR = 19;
    public static final String SYSTEM_IMPORT_DATEI = "system-import-datei";
    public static final int SYSTEM_IMPORT_DATEI_NR = 20;
    public static final String SYSTEM_IMPORT_URL_DIALOG = "system-import-url-dialog";
    public static final int SYSTEM_IMPORT_URL_DIALOG_NR = 21;
    public static final String SYSTEM_IMPORT_URL_MANUELL = "system-import-url-manuell";
    public static final int SYSTEM_IMPORT_URL_MANUELL_NR = 22;
    public static final String SYSTEM_EXPORT_DATEI = "system-export-datei";
    public static final int SYSTEM_EXPORT_DATEI_NR = 23;
    //##############
    public static final String SYSTEM_HINWEIS_ANZEIGEN = "Hinweis_anzeigen";
    public static final int SYSTEM_HINWEIS_ANZEIGEN_NR = 24;
    public static final String SYSTEM_WARTEN = "warten";
    public static final int SYSTEM_WARTEN_NR = 25;
    public static final String SYSTEM_ALTE_FILME = "alte-filme";
    public static final int SYSTEM_ALTE_FILME_NR = 26;
    public static final String SYSTEM_UDATE_SUCHEN = "update-suchen";
    public static final int SYSTEM_UPDATE_SUCHEN_NR = 27;
    public static final String SYSTEM_UPDATE_DATUM = "update-datum";
    public static final int SYSTEM_UPDATE_DATUM_NR = 28;
    public static final String SYSTEM_UPDATE_20 = "update-nach-20";
    public static final int SYSTEM_UPDATE_20_NR = 29;
    public static final String SYSTEM_IMPORT_ART_FILME = "update-filme"; // url automatisch suchen - oder nur manuell
    public static final int SYSTEM_IMPORT_ART_FILME_NR = 30;
    //
    public static final String[] SYSTEM_COLUMN_NAMES = {SYSTEM_FEHLER_ANZEIGEN, SYSTEM_LOOK, SYSTEM_START_MAX, SYSTEM_USER_AGENT, SYSTEM_VERSION, SYSTEM_GROESSE_X, SYSTEM_GROESSE_Y,
        SYSTEM_ABO, SYSTEM_NUR_ABO, SYSTEM_USER_AGENT_MANUEL,
        SYSTEM_PROXY_SERVER, SYSTEM_PROXY_PORT, SYSTEM_PROXY_USER, SYSTEM_PROXY_PWD, SYSTEM_PROXY_ON,
        SYSTEM_MAX_DOWNLOAD, SYSTEM_LEITUNG_LOW, SYSTEM_RTMP_FLVSTREAMER,
        SYSTEM_SWR_LISTE, SYSTEM_PFAD_EXPORT_ABOS, SYSTEM_IMPORT_DATEI, SYSTEM_IMPORT_URL_DIALOG, SYSTEM_IMPORT_URL_MANUELL,
        SYSTEM_EXPORT_DATEI, SYSTEM_HINWEIS_ANZEIGEN, SYSTEM_WARTEN, SYSTEM_ALTE_FILME,
        SYSTEM_UDATE_SUCHEN, SYSTEM_UPDATE_DATUM, SYSTEM_UPDATE_20, SYSTEM_IMPORT_ART_FILME};
    //
    //
    //
    //Tags Programm
    public static final String PROGRAMM_BUTTON = "Programm-Button";
    public static final String PROGRAMM_ABO = "Programm-Abo";
    public static final String PROGRAMM = "Programm";
    public static final int PROGRAMM_MAX_ELEM = 7;
    public static final String PROGRAMM_NAME = "Programmname";
    public static final int PROGRAMM_NAME_NR = 0;
    public static final String PROGRAMM_ZIEL_DATEINAME = "Programm-Zieldateiname";
    public static final int PROGRAMM_ZIEL_DATEINAME_NR = 1;
    public static final String PROGRAMM_PROGRAMMPFAD = "Programmpfad";
    public static final int PROGRAMM_PROGRAMMPFAD_NR = 2;
    public static final String PROGRAMM_SCHALTER = "Programmschalter";
    public static final int PROGRAMM_SCHALTER_NR = 3;
    public static final String PROGRAMM_PRAEFIX = "Praefix";
    public static final int PROGRAMM_PRAEFIX_NR = 4;
    public static final String PROGRAMM_SUFFIX = "Suffix";
    public static final int PROGRAMM_SUFFIX_NR = 5;
    public static final String PROGRAMM_RESTART = "Prog-Restart";
    public static final int PROGRAMM_RESTART_NR = 6;
    public static final String[] PROGRAMM_COLUMN_NAMES = {PROGRAMM_NAME, PROGRAMM_ZIEL_DATEINAME, PROGRAMM_PROGRAMMPFAD,
        PROGRAMM_SCHALTER, PROGRAMM_PRAEFIX, PROGRAMM_SUFFIX, PROGRAMM_RESTART};
    //
    //Tags Programmgruppen
    public static final String PROGRAMMGRUPPE_BUTTON = "Programmgruppe-Button";
    public static final String PROGRAMMGRUPPE_ABO = "Programmgruppe-Abo";
    public static final int PROGRAMMGRUPPE_MAX_ELEM = 7;
    public static final String PROGRAMMGRUPPE_NAME = "Programmgruppen-Name";
    public static final int PROGRAMMGRUPPE_NAME_NR = 0;
    public static final String PROGRAMMGRUPPE_PRAEFIX_DIREKT = "Programmgruppen-Praefix";
    public static final int PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR = 1;
    public static final String PROGRAMMGRUPPE_SUFFIX_DIREKT = "Programmgruppen-Suffix";
    public static final int PROGRAMMGRUPPE_SUFFIX_DIREKT_NR = 2;
    public static final String PROGRAMMGRUPPE_FARBE = "Programmgruppen-Farbe";
    public static final int PROGRAMMGRUPPE_FARBE_NR = 3;
    public static final String PROGRAMMGRUPPE_DOPPELKLICK = "Programmgruppen-Doppelklick";
    public static final int PROGRAMMGRUPPE_DOPPELKLICK_NR = 4;
    public static final String PROGRAMMGRUPPE_ZIEL_PFAD = "Programmgruppen-Zielpfad";
    public static final int PROGRAMMGRUPPE_ZIEL_PFAD_NR = 5;
    public static final String PROGRAMMGRUPPE_ZIEL_DATEINAME = "Programmgruppen-Zieldateiname";
    public static final int PROGRAMMGRUPPE_ZIEL_DATEINAME_NR = 6;
    public static final String[] PROGRAMMGRUPPE_COLUMN_NAMES = {PROGRAMMGRUPPE_NAME, PROGRAMMGRUPPE_PRAEFIX_DIREKT, PROGRAMMGRUPPE_SUFFIX_DIREKT,
        PROGRAMMGRUPPE_FARBE, PROGRAMMGRUPPE_DOPPELKLICK,
        PROGRAMMGRUPPE_ZIEL_PFAD, PROGRAMMGRUPPE_ZIEL_DATEINAME};
    //
    //Tags Abo
    public static final String ABO = "Abonnement";
    public static final int ABO_MAX_ELEM = 11;
    public static final String ABO_NR = "Nr";
    public static final int ABO_NR_NR = 0;
    public static final String ABO_NAME = "Name";
    public static final int ABO_NAME_NR = 1;
    public static final String ABO_SENDER = DatenFilm.FILM_SENDER;
    public static final int ABO_SENDER_NR = 2;
    public static final String ABO_THEMA = DatenFilm.FILM_THEMA;
    public static final int ABO_THEMA_NR = 3;
    public static final String ABO_THEMA_EXAKT = DatenFilm.FILM_THEMA + "-exakt";
    public static final int ABO_THEMA_EXAKT_NR = 4;
    public static final String ABO_TITEL = DatenFilm.FILM_TITEL;
    public static final int ABO_TITEL_NR = 5;
    public static final String ABO_ZIELPFAD = "Zielpfad";
    public static final int ABO_ZIELPFAD_NR = 6;
    public static final String ABO_DOWN_DATUM = "letztes_Abo";
    public static final int ABO_DOWN_DATUM_NR = 7;
    public static final String ABO_PGRUPPE = "Programmgruppe";
    public static final int ABO_PGRUPPE_NR = 8;
    public static final String ABO_EINMAL_URL = "Einmalabo";
    public static final int ABO_EINMAL_URL_NR = 9;
    public static final String ABO_EINMAL_ERLEDIGT = "fertig";
    public static final int ABO_EINMAL_ERLEDIGT_NR = 10;
    public static final String[] ABO_COLUMN_NAMES = {ABO_NR, ABO_NAME, ABO_SENDER, ABO_THEMA, ABO_THEMA_EXAKT, ABO_TITEL,
        ABO_ZIELPFAD, ABO_DOWN_DATUM, ABO_PGRUPPE, ABO_EINMAL_URL, ABO_EINMAL_ERLEDIGT};
    //
    //Tags Podder
    public static final String POD = "Pod";
    public static final int POD_MAX_ELEM = 5;
    public static final String POD_NR = "Nr";
    public static final int POD_NR_NR = 0;
    public static final String POD_NAME = "Name";
    public static final int POD_NAME_NR = 1;
    public static final String POD_URL = "Url";
    public static final int POD_URL_NR = 2;
    public static final String POD_ZIELPFAD = "Zielpfad";
    public static final int POD_ZIELPFAD_NR = 3;
    public static final String POD_DOWN_DATUM = "letzter_Pod";
    public static final int POD_DOWN_DATUM_NR = 4;
    public static final String[] POD_COLUMN_NAMES = {POD_NR, POD_NAME, POD_URL, POD_ZIELPFAD, POD_DOWN_DATUM};
    //Tags Poddester
    public static final String PODSTER = "Podster";
    public static final int PODSTER_MAX_ELEM = 4;
    public static final String PODSTER_NR = "Nr";
    public static final int PODSTER_NR_NR = 0;
    public static final String PODSTER_THEMA = "Thema";
    public static final int PODSTER_THEMA_NR = 1;
    public static final String PODSTER_TITEL = "Titel";
    public static final int PODSTER_TITEL_NR = 2;
    public static final String PODSTER_URL = "Url";
    public static final int PODSTER_URL_NR = 3;
    public static final String[] PODSTER_COLUMN_NAMES = {PODSTER_NR, PODSTER_THEMA, PODSTER_TITEL, PODSTER_URL};
}
