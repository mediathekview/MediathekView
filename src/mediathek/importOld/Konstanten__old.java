/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.importOld;

public class Konstanten__old {

    public static final String KODIERUNG_UTF = "UTF-8";
    public static final String KODIERUNG_ISO15 = "ISO-8859-15";
    public static final String LOG_DATEI_ZDF = ".mediathek_zdf_log";
    public static final String LOG_DATEI_POD = ".mediathek_podder_log";
    public static final String LOG_DATEI_HISTORY = "history.txt";
    public static final String XML_DATEI = ".mediathek";
    public static final String XML_DATEI_FILME = ".filme";
    public static final String XML_START = "Mediathek";
    //Formate zu Zippen
    public static final String FORMAT_ZIP = ".zip";
    public static final String FORMAT_BZ2 = ".bz2";
    // für das Anpassen der URL für den flvstreamer
    public static final String RTMP_FLVSTREAMER = "-r ";
    public static final String RTMP_PRTOKOLL = "rtmp";
    public static final String USER_AGENT_DEFAULT = "MediathekView";
    //
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
    //podcast
    public static final String SYSTEM_PODCAST = "Podcast";
    public static final int SYSTEM_PODCAST_NR = 10;
    public static final String SYSTEM_ZIELPFAD_POD = "Basisziel-Podcast";
    public static final int SYSTEM_ZIELPFAD_POD_NR = 11;
    //proxy
    public static final String SYSTEM_PROXY_SERVER = "proxy-server";
    public static final int SYSTEM_PROXY_SERVER_NR = 12;
    public static final String SYSTEM_PROXY_PORT = "proxy-port";
    public static final int SYSTEM_PROXY_PORT_NR = 13;
    public static final String SYSTEM_PROXY_USER = "proxy-benutzer";
    public static final int SYSTEM_PROXY_USER_NR = 14;
    public static final String SYSTEM_PROXY_PWD = "proxy-passwort";
    public static final int SYSTEM_PROXY_PWD_NR = 15;
    public static final String SYSTEM_PROXY_ON = "proxy-an";
    public static final int SYSTEM_PROXY_ON_NR = 16;
    //Einstellungen Sender
    public static final String SYSTEM_MAX_DOWNLOAD = "maxDownload";
    public static final int SYSTEM_MAX_DOWNLOAD_NR = 17;
    public static final String SYSTEM_LEITUNG_LOW = "Leitung";
    public static final int SYSTEM_LEITUNG_LOW_NR = 18;
    public static final String SYSTEM_RTMP_FLVSTREAMER = "Rtmp-bauen";
    public static final int SYSTEM_RTMP_FLVSTREAMER_NR = 19;
    public static final String SYSTEM_SWR_LISTE = "nr-in-liste";
    public static final int SYSTEM_SWR_LISTE_NR = 20;
    //Einstellungen Im-Export
    public static final String SYSTEM_PFAD_EXPORT_ABOS = "Pfad_Export_Abos";
    public static final int SYSTEM_PFAD_EXPORT_ABOS_NR = 21;
    public static final String SYSTEM_IMPORT_DATEI = "system-import-datei";
    public static final int SYSTEM_IMPORT_DATEI_NR = 22;
    public static final String SYSTEM_IMPORT_URL = "system-import-url";
    public static final int SYSTEM_IMPORT_URL_NR = 23;
    public static final String SYSTEM_IMPORT_URL_AUTO = "system-import-url-auto";
    public static final int SYSTEM_IMPORT_URL_AUTO_NR = 24;
    public static final String SYSTEM_EXPORT_DATEI = "system-export-datei";
    public static final int SYSTEM_EXPORT_DATEI_NR = 25;
    //##############
    public static final String SYSTEM_HINWEIS_ANZEIGEN = "Hinweis_anzeigen";
    public static final int SYSTEM_HINWEIS_ANZEIGEN_NR = 26;
    public static final String SYSTEM_WARTEN = "warten";
    public static final int SYSTEM_WARTEN_NR = 27;
    public static final String SYSTEM_ALTE_FILME = "alte-filme";
    public static final int SYSTEM_ALTE_FILME_NR = 28;
    public static final String SYSTEM_UDATE_SUCHEN = "update-suchen";
    public static final int SYSTEM_UPDATE_SUCHEN_NR = 29;
    public static final String SYSTEM_UPDATE_DATUM = "update-datum";
    public static final int SYSTEM_UPDATE_DATUM_NR = 30;
    public static final String SYSTEM_UPDATE_20 = "update-nach-20";
    public static final int SYSTEM_UPDATE_20_NR = 31;
    public static final String SYSTEM_UPDATE_FILME = "update-filme";
    public static final int SYSTEM_UPDATE_FILME_NR = 32;
    //
    public static final String[] SYSTEM_COLUMN_NAMES = {SYSTEM_FEHLER_ANZEIGEN, SYSTEM_LOOK, SYSTEM_START_MAX, SYSTEM_USER_AGENT, SYSTEM_VERSION, SYSTEM_GROESSE_X, SYSTEM_GROESSE_Y,
        SYSTEM_ABO, SYSTEM_NUR_ABO, SYSTEM_USER_AGENT_MANUEL, SYSTEM_PODCAST, SYSTEM_ZIELPFAD_POD,
        SYSTEM_PROXY_SERVER, SYSTEM_PROXY_PORT, SYSTEM_PROXY_USER, SYSTEM_PROXY_PWD, SYSTEM_PROXY_ON,
        SYSTEM_MAX_DOWNLOAD, SYSTEM_LEITUNG_LOW, SYSTEM_RTMP_FLVSTREAMER,
        SYSTEM_SWR_LISTE, SYSTEM_PFAD_EXPORT_ABOS, SYSTEM_IMPORT_DATEI, SYSTEM_IMPORT_URL, SYSTEM_IMPORT_URL_AUTO,
        SYSTEM_EXPORT_DATEI, SYSTEM_HINWEIS_ANZEIGEN, SYSTEM_WARTEN, SYSTEM_ALTE_FILME,
        SYSTEM_UDATE_SUCHEN, SYSTEM_UPDATE_DATUM, SYSTEM_UPDATE_20, SYSTEM_UPDATE_FILME};
    //
    //
    //Tags Programm
    public static final String PROGRAMM_BUTTON = "Programm-Button";
    public static final String PROGRAMM_ABO = "Programm-Abo";
    public static final String PROGRAMM = "Programm";
    public static final int PROGRAMM_MAX_ELEM = 6;
    public static final String PROGRAMM_NAME = "Programmname";
    public static final int PROGRAMM_NAME_NR = 0;
    public static final String PROGRAMM_PROGRAMMPFAD = "Programmpfad";
    public static final int PROGRAMM_PROGRAMMPFAD_NR = 1;
    public static final String PROGRAMM_SCHALTER = "Programmschalter";
    public static final int PROGRAMM_SCHALTER_NR = 2;
    public static final String PROGRAMM_PRAEFIX = "Praefix";
    public static final int PROGRAMM_PRAEFIX_NR = 3;
    public static final String PROGRAMM_SUFFIX = "Suffix";
    public static final int PROGRAMM_SUFFIX_NR = 4;
    public static final String PROGRAMM_RESTART = "Prog-Restart";
    public static final int PROGRAMM_RESTART_NR = 5;
    public static final String[] PROGRAMM_COLUMN_NAMES = {PROGRAMM_NAME, PROGRAMM_PROGRAMMPFAD,
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
    public static final String FILM_SENDER = "Sender";
    public static final String FILM_THEMA = "Thema";
    public static final String FILM_TITEL = "Titel";
    //Tags Abo
    public static final String ABO = "Abonnement";
    public static final int ABO_MAX_ELEM = 11;
    public static final String ABO_NR = "Nr";
    public static final int ABO_NR_NR = 0;
    public static final String ABO_NAME = "Name";
    public static final int ABO_NAME_NR = 1;
    public static final String ABO_SENDER = FILM_SENDER;
    public static final int ABO_SENDER_NR = 2;
    public static final String ABO_THEMA = FILM_THEMA;
    public static final int ABO_THEMA_NR = 3;
    public static final String ABO_THEMA_EXAKT = FILM_THEMA + "-exakt";
    public static final int ABO_THEMA_EXAKT_NR = 4;
    public static final String ABO_TITEL = FILM_TITEL;
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
    //Tags Blacklist
    public static final String BLACKLIST = "Blacklist";
    public static final int BLACKLIST_MAX_ELEM = 2;
    public static final String BLACKLIST_SENDER = "black-sender";
    public static final int BLACKLIST_SENDER_NR = 0;
    public static final String BLACKLIST_THEMA = "black-thema";
    public static final int BLACKLIST_THEMA_NR = 1;
    public static final String[] BLACKLIST_COLUMN_NAMES = {BLACKLIST_SENDER, BLACKLIST_THEMA};
    //
}
