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

public class Konstanten {

    public static final String VERSION = "3.0.0";
    public static final String PROGRAMMNAME = "MediathekView";
    public static final String USER_AGENT_DEFAULT = PROGRAMMNAME + " " + VERSION;
    // MediathekView URLs
    public static final String ADRESSE_UPDATE_SERVER = "http://zdfmediathk.sourceforge.net/update.xml";
    public static final String ADRESSE_PROGRAMM_VERSION = "http://zdfmediathk.sourceforge.net/version.xml";
    public static final String ADRESSE_DOWNLAD = "http://sourceforge.net/projects/zdfmediathk/";
    public static final String ADRESSE_ANLEITUNG = "http://zdfmediathk.sourceforge.net";
    public static final String ADRESSE_VORLAGE_PROGRAMMGRUPPEN = "http://zdfmediathk.sourceforge.net/programmgruppen.xml";
    // ProgrammUrls
    public static final String ADRESSE_VLC = "http://www.videolan.org/";
    public static final String ADRESSE_FLVSTREAMER = "https://savannah.nongnu.org/projects/flvstreamer/";
    // Dateien/Verzeichnisse
    public static final String MEDIATHEK_VIEW_VERZEICHNISS = ".mediathek3";
    public static final String LOG_DATEI_DOWNLOAD_ABOS = "downloadAbos.txt";
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
    public static final int MAX_SENDER_FILME_LADEN = 2;//es können maximal soviele Filme eines Senders/Servers gleichzeitig geladen werden
    // ################################
    // Tags System
    // ################################
    public static final String SYSTEM_ECHTZEITSUCHE = "Echtzeitsuche";
    public static final int SYSTEM_ECHTZEITSUCHE_NR = 0;
    public static final String SYSTEM_LOOK = "System-look";
    public static final int SYSTEM_LOOK_NR = 1;
    public static final String SYSTEM_USER_AGENT_AUTO = "User-Agent-Auto";
    public static final int SYSTEM_USER_AGENT_AUTO_NR = 2;
    public static final String SYSTEM_USER_AGENT = "User-Agent";
    public static final int SYSTEM_USER_AGENT_NR = 3;
    public static final String SYSTEM_VERSION = "version";
    public static final int SYSTEM_VERSION_NR = 4;
    public static final String SYSTEM_GROESSE_X = "GroesseX";
    public static final int SYSTEM_GROESSE_X_NR = 5;
    public static final String SYSTEM_GROESSE_Y = "GroesseY";
    public static final int SYSTEM_GROESSE_Y_NR = 6;
    //abo
    public static final String SYSTEM_ICON_KLEIN = "system-icon-groesse";
    public static final int SYSTEM_ICON_KLEIN_NR = 7;
    public static final String SYSTEM_FENSTER_MAX = "programmfenster-maximieren";
    public static final int SYSTEM_FENSTER_MAX_NR = 8;
    public static final String SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN = "system-panel-videoplayer-anzeigen";
    public static final int SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN_NR = 9;
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
    public static final int SYSTEM_LEITUNG_LOW_NR = 16;
    public static final String SYSTEM_RTMP_FLVSTREAMER = "Rtmp-bauen";
//    public static final int SYSTEM_RTMP_FLVSTREAMER_NR = 17;
    public static final String SYSTEM_SWR_LISTE = "nr-in-liste";
    public static final int SYSTEM_SWR_LISTE_NR = 18;
    //Einstellungen Im-Export
    public static final String SYSTEM_PFAD_EXPORT_ABOS = "Pfad_Export_Abos";
    public static final int SYSTEM_PFAD_EXPORT_ABOS_NR = 19;
    public static final String SYSTEM_IMPORT_DATEI = "system-import-datei";
    public static final int SYSTEM_IMPORT_DATEI_NR = 20;
    public static final String SYSTEM_IMPORT_URL_AUTO = "system-import-url-auto";
    public static final int SYSTEM_IMPORT_URL_AUTO_NR = 21;
    public static final String SYSTEM_IMPORT_URL_MANUELL = "system-import-url-manuell";
    public static final int SYSTEM_IMPORT_URL_MANUELL_NR = 22;
    public static final String SYSTEM_EXPORT_DATEI = "system-export-datei";
    public static final int SYSTEM_EXPORT_DATEI_NR = 23;
    //##############
    public static final String SYSTEM_HINWEIS_ANZEIGEN = "Hinweis_anzeigen";
//    public static final int SYSTEM_HINWEIS_ANZEIGEN_NR = 24;
    public static final String SYSTEM_WARTEN = "warten";
    public static final int SYSTEM_WARTEN_NR = 25;
    public static final String SYSTEM_ALTE_FILME = "alte-filme";
//    public static final int SYSTEM_ALTE_FILME_NR = 26;
    public static final String SYSTEM_UDATE_SUCHEN = "update-suchen";
    public static final int SYSTEM_UPDATE_SUCHEN_NR = 27;
    public static final String SYSTEM_UPDATE_DATUM = "update-datum";
    public static final int SYSTEM_UPDATE_DATUM_NR = 28;
    public static final String SYSTEM_UPDATE_20 = "update-nach-20";
//    public static final int SYSTEM_UPDATE_20_NR = 29;
    public static final String SYSTEM_IMPORT_ART_FILME = "update-filme"; // url automatisch suchen - oder nur manuell
    public static final int SYSTEM_IMPORT_ART_FILME_NR = 30;
    // Filter
    public static final String SYSTEM_FILTER_TAGE = "system-filter-tage"; // index im Array GuiFilme.COMBO_ZEIT_INT
    public static final int SYSTEM_FILTER_TAGE_NR = 31;
    public static final String SYSTEM_FILTER_KEINE_ABO = "system-filter-abo";
    public static final int SYSTEM_FILTER_KEINE_ABO_NR = 32;
    public static final String SYSTEM_FILTER_KEINE_GESEHENE = "system-filter-gesehen";
    public static final int SYSTEM_FILTER_KEINE_GESEHENE_NR = 33;
    // Programmpfade
    public static final String SYSTEM_PFAD_VLC = "pfad-vlc";
    public static final int SYSTEM_PFAD_VLC_NR = 34;
    public static final String SYSTEM_PFAD_FLVSTREAMER = "pfad-flvstreamer";
    public static final int SYSTEM_PFAD_FLVSTREAMER_NR = 35;
    //
    public static final String SYSTEM = "system";
    public static final int SYSTEM_MAX_ELEM = 36;
    public static final String[] SYSTEM_COLUMN_NAMES = {SYSTEM_ECHTZEITSUCHE, SYSTEM_LOOK, SYSTEM_USER_AGENT_AUTO, SYSTEM_USER_AGENT, SYSTEM_VERSION, SYSTEM_GROESSE_X, SYSTEM_GROESSE_Y,
        SYSTEM_ICON_KLEIN, SYSTEM_FENSTER_MAX, SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN,
        SYSTEM_PROXY_SERVER, SYSTEM_PROXY_PORT, SYSTEM_PROXY_USER, SYSTEM_PROXY_PWD, SYSTEM_PROXY_ON,
        SYSTEM_MAX_DOWNLOAD, SYSTEM_LEITUNG_LOW, SYSTEM_RTMP_FLVSTREAMER,
        SYSTEM_SWR_LISTE, SYSTEM_PFAD_EXPORT_ABOS, SYSTEM_IMPORT_DATEI, SYSTEM_IMPORT_URL_AUTO, SYSTEM_IMPORT_URL_MANUELL,
        SYSTEM_EXPORT_DATEI, SYSTEM_HINWEIS_ANZEIGEN, SYSTEM_WARTEN, SYSTEM_ALTE_FILME,
        SYSTEM_UDATE_SUCHEN, SYSTEM_UPDATE_DATUM, SYSTEM_UPDATE_20, SYSTEM_IMPORT_ART_FILME,
        SYSTEM_FILTER_TAGE, SYSTEM_FILTER_KEINE_ABO, SYSTEM_FILTER_KEINE_GESEHENE,
        SYSTEM_PFAD_VLC, SYSTEM_PFAD_FLVSTREAMER};
    //
}
