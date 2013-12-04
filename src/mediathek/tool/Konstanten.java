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
package mediathek.tool;

public class Konstanten {

    public static final String VERSION = "3.3.0";
    public static final String PROGRAMMNAME = "MediathekView";
    public static final String USER_AGENT_DEFAULT = PROGRAMMNAME + " " + VERSION;
    // MediathekView URLs
    public static final String ADRESSE_FILMLISTEN_SERVER = "http://zdfmediathk.sourceforge.net/update.xml";
    public static final String DATEINAME_LISTE_FILMLISTEN = "filmlisten.xml";
    public static final String ADRESSE_PROGRAMM_VERSION = "http://zdfmediathk.sourceforge.net/version.xml";
    public static final String ADRESSE_DOWNLAD = "http://sourceforge.net/projects/zdfmediathk/";
    public static final String ADRESSE_ANLEITUNG = "http://sourceforge.net/p/zdfmediathk/wiki/Home/";
    public static final String ADRESSE_VORLAGE_PROGRAMMGRUPPEN = "http://zdfmediathk.sourceforge.net/programmgruppen/programmgruppen.xml";
    public static final String ADRESSE_WEBSITE = "http://zdfmediathk.sourceforge.net/";
    public static final String ADRESSE_FORUM = "http://sourceforge.net/apps/phpbb/zdfmediathk/";
    // ProgrammUrls
    public static final String ADRESSE_WEBSITE_MPLAYER = "http://sourceforge.net/projects/smplayer/";
    public static final String ADRESSE_WEBSITE_VLC = "http://www.videolan.org/";
    public static final String ADRESSE_WEBSITE_FLVSTREAMER = "https://savannah.nongnu.org/projects/flvstreamer/";
    // Dateien/Verzeichnisse
    public static final String VERZEICNHISS_ICONS = "Icons"; // Unterverzeichnis im Programmverzeichnis in dem die Iconsets liegen
    public static final String VERZEICNHISS_DOWNLOADS = PROGRAMMNAME; // Standard wenn nichts angeben, Verzeichnis wird im Homeverzeichnis angelegt
    public static final String VERZEICHNISS_EINSTELLUNGEN = ".mediathek3"; // im Homeverzeichnis
    public static final String JSON_DATEI_FILME = "filme.json";
    // 
    public static final int MIN_DATEI_GROESSE_FILM = 256 * 1024; //minimale Größe (256 KiB) eines Films um nicht als Fehler zu gelten
    public static final String KODIERUNG_UTF = "UTF-8";
    public static final String KODIERUNG_ISO15 = "ISO-8859-15";
    public static final String XML_START = "Mediathek";
    public static final String LEITUNG_DSL1000 = "dsl1000";
    public static final String LEITUNG_DSL2000 = "dsl2000";
    public static final int MAX_SENDER_FILME_LADEN = 2;//es können maximal soviele Filme eines Senders/Servers gleichzeitig geladen werden
    public static final int STRING_BUFFER_START_BUFFER = 10 * 1024 * 8; // 10 KiB
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
    // Fenstereinstellungen
    public static final String SYSTEM_GROESSE = "Groesse";
    public static final int SYSTEM_GROESSE_NR = 5;
    public static final String SYSTEM_GROESSE_EINSTELLUNGEN = "Groesse-Einstellungen";
    public static final int SYSTEM_GROESSE_EINSTELLUNGEN_NR = 6;
    public static final String SYSTEM_ICON_KLEIN = "system-icon-groesse";
    public static final int SYSTEM_ICON_KLEIN_NR = 7;
    public static final String SYSTEM_FENSTER_MAX = "programmfenster-maximieren";
    public static final int SYSTEM_FENSTER_MAX_NR = 8;
    public static final String SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN = "system-panel-videoplayer-anzeigen";
    public static final int SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN_NR = 9;
    public static final String SYSTEM_PANEL_MELDUNGEN_ANZEIGEN = "system-panel-meldungen-anzeigen";
    public static final int SYSTEM_PANEL_MELDUNGEN_ANZEIGEN_NR = 10;
    public static final String SYSTEM_PANEL_FILTER_ANZEIGEN = "system-panel-filter-anzeigen";
    public static final int SYSTEM_PANEL_FILTER_ANZEIGEN_NR = 11;
    public static final String SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN = "system-panel-beschreibung-anzeigen";
    public static final int SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN_NR = 12;
    //proxy
    public static final String SYSTEM_HTTP_PROXY_SERVER = "proxy-server";
    public static final int SYSTEM_HTTP_PROXY_SERVER_NR = 13;
    public static final String SYSTEM_HTTP_PROXY_PORT = "proxy-port";
    public static final int SYSTEM_HTTP_PROXY_PORT_NR = 14;
    public static final String SYSTEM_HTTP_PROXY_USER = "proxy-benutzer";
    public static final int SYSTEM_HTTP_PROXY_USER_NR = 15;
    public static final String SYSTEM_HTTP_PROXY_PWD = "proxy-passwort";
    public static final int SYSTEM_HTTP_PROXY_PWD_NR = 16;
    public static final String SYSTEM_HTTP_PROXY_ON = "proxy-an";
    public static final int SYSTEM_HTTP_PROXY_ON_NR = 17;
    //Einstellungen Sender
    public static final String SYSTEM_MAX_DOWNLOAD = "maxDownload";
    public static final int SYSTEM_MAX_DOWNLOAD_NR = 18;
    public static final String SYSTEM_LEITUNG_LOW = "Leitung";
    public static final int SYSTEM_LEITUNG_LOW_NR = 19;
    public static final String SYSTEM_SWR_LISTE = "nr-in-liste";
    public static final int SYSTEM_SWR_LISTE_NR = 20;
    //Einstellungen Im-Export
    public static final String SYSTEM_PFAD_EXPORT_ABOS = "Pfad_Export_Abos";
    public static final int SYSTEM_PFAD_EXPORT_ABOS_NR = 21;
    public static final String SYSTEM_IMPORT_DATEI = "system-import-datei";
    public static final int SYSTEM_IMPORT_DATEI_NR = 22;
    public static final String SYSTEM_URL_FILMLISTEN = "system-url-filmlisten";
    public static final int SYSTEM_URL_FILMLISTEN_NR = 23;
    public static final String SYSTEM_IMPORT_URL_MANUELL = "system-import-url-manuell";
    public static final int SYSTEM_IMPORT_URL_MANUELL_NR = 24;
    public static final String SYSTEM_EXPORT_DATEI = "system-export-datei";
    public static final int SYSTEM_EXPORT_DATEI_NR = 25;
    //##############
    public static final String SYSTEM_WARTEN = "warten";
    public static final int SYSTEM_WARTEN_NR = 26;
    public static final String SYSTEM_UDATE_SUCHEN = "update-suchen";
    public static final int SYSTEM_UPDATE_SUCHEN_NR = 27;
    public static final String SYSTEM_UPDATE_DATUM = "update-datum";
    public static final int SYSTEM_UPDATE_DATUM_NR = 28;
    public static final String SYSTEM_IMPORT_ART_FILME = "update-filme"; // url automatisch suchen - oder nur manuell
    public static final int SYSTEM_IMPORT_ART_FILME_NR = 29;
    // Filter
    public static final String SYSTEM_FILTER_TAGE = "system-filter-tage"; // index im Array GuiFilme.COMBO_ZEIT_INT
    public static final int SYSTEM_FILTER_TAGE_NR = 30;
    public static final String SYSTEM_FILTER_KEINE_ABO = "system-filter-abo";
    public static final int SYSTEM_FILTER_KEINE_ABO_NR = 31;
    public static final String SYSTEM_FILTER_KEINE_GESEHENE = "system-filter-gesehen";
    public static final int SYSTEM_FILTER_KEINE_GESEHENE_NR = 32;
    public static final String SYSTEM_FILTER_NUR_HD = "system-filter-hd";
    public static final int SYSTEM_FILTER_NUR_HD_NR = 33;
    // Programmpfade
    public static final String SYSTEM_PFAD_VLC = "pfad-vlc";
    public static final int SYSTEM_PFAD_VLC_NR = 34;
    public static final String SYSTEM_PFAD_FLVSTREAMER = "pfad-flvstreamer";
    public static final int SYSTEM_PFAD_FLVSTREAMER_NR = 35;
    public static final String SYSTEM_PFAD_MPLAYER = "pfad-mplayer";
    public static final int SYSTEM_PFAD_MPLAYER_NR = 36;
    //#######################
    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_FILME = "Eigenschaften-Tabellen-Filme";
    public static final int SYSTEM_EIGENSCHAFTEN_TABELLE_FILME_NR = 37;
    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS = "Eigenschaften-Tabellen-Downloads";
    public static final int SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS_NR = 38;
    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS = "Eigenschaften-Tabellen-Abos";
    public static final int SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS_NR = 39;
    //#######################
    public static final String SYSTEM_ABOS_SOFORT_SUCHEN = "Abos-sofort-suchen";
    public static final int SYSTEM_ABOS_SOFORT_SUCHEN_NR = 40;
    public static final String SYSTEM_NUR_ASCII = "nur-Ascii-Zeichen";
    public static final int SYSTEM_NUR_ASCII_NR = 41;
    public static final String SYSTEM_UNICODE_AENDERN = "Unicode-aendern";
    public static final int SYSTEM_UNICODE_AENDERN_NR = 42;
    public static final String SYSTEM_HINWEIS_NR_ANGEZEIGT = "Hinweis-Nr-angezeigt";
    public static final int SYSTEM_HINWEIS_NR_ANGEZEIGT_NR = 43;
    public static final String SYSTEM_ORDNER_OEFFNEN = "Download-Ordner-oeffnen";
    public static final int SYSTEM_ORDNER_OEFFNEN_NR = 44;
    public static final String SYSTEM_URL_OEFFNEN = "Programm-Url-oeffnen";
    public static final int SYSTEM_URL_OEFFNEN_NR = 45;
    public static final String SYSTEM_BLACKLIST_ZUKUNFT__NICHT_ANZEIGEN = "Blacklist-Zukunft-nicht-anzeigen";
    public static final int SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN_NR = 46;
    public static final String SYSTEM_BLACKLIST_AUCH_ABO = "Blacklist-auch-Abo";
    public static final int SYSTEM_BLACKLIST_AUCH_ABO_NR = 47;
    public static final String SYSTEM_BLACKLIST_AUSGESCHALTET = "Blacklist-ausgeschaltet";
    public static final int SYSTEM_BLACKLIST_AUSGESCHALTET_NR = 48;
    public static final String SYSTEM_BLACKLIST_IST_WHITELIST = "Blacklist-ist-Whitelist";
    public static final int SYSTEM_BLACKLIST_IST_WHITELIST_NR = 49;
    public static final String SYSTEM_BLACKLIST_FILMLAENGE = "Blacklist-Filmlaenge";
    public static final int SYSTEM_BLACKLIST_FILMLAENGE_NR = 50;
    public static final String SYSTEM_VERSION_PROGRAMMSET = "Version-Programmset";
    public static final int SYSTEM_VERSION_PROGRAMMSET_NR = 51;
    public static final String SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN = "Meldungsfenster-Systemmeldungen";
    public static final int SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN_NR = 52;
    public static final String SYSTEM_MEDUNGSFENSTER_UMBRECHEN_FEHLERMELDUNGEN = "Meldungsfenster-Fehlermeldungen";
    public static final int SYSTEM_MEDUNGSFENSTER_UMBRECHEN_FEHLERMELDUNGEN_NR = 53;
    public static final String SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN = "Meldungsfenster-Playermeldungen";
    public static final int SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN_NR = 54;
    public static final String SYSTEM_ANSICHT_SET_LANG = "Ansicht-Set-lang";
    public static final int SYSTEM_ANSICHT_SET_LANG_NR = 55;
    public static final String SYSTEM_ICON_STANARD = "Icon-Standard";
    public static final int SYSTEM_ICON_STANDARD_NR = 56;
    public static final String SYSTEM_ICON_PFAD = "Icon-Pfad";
    public static final int SYSTEM_ICON_PFAD_NR = 57;
    public static final String SYSTEM_BREITE_MELDUNGEN = "breite-Meldungen";
    public static final int SYSTEM_BREITE_MELDUNGEN_NR = 58;
    public static final String SYSTEM_BANDBREITE = "Bandbreite";
    public static final int SYSTEM_BANDBREITE_KBYTE_NR = 59;
    public static final String SYSTEM_FILMLISTE_UMBENENNEN = "Filmliste-umbenennen";
    public static final int SYSTEM_FILMLISTE_UMBENENNEN_NR = 60;
    public static final String SYSTEM_DOWNLOAD_SOFORT_STARTEN = "Download-sofort-starten";
    public static final int SYSTEM_DOWNLOAD_SOFORT_STARTEN_NR = 61;
    public static final String SYSTEM_NOTIFICATION = "Notification-anzeigen";
    public static final int SYSTEM_NOTIFICATION_NR = 62;
    public static final String SYSTEM_DIALOG_DOWNLOAD_D_STARTEN = "Dialog-Download-D-Starten";
    public static final int SYSTEM_DIALOG_DOWNLOAD_D_STARTEN_NR = 63;
    public static final String SYSTEM_PLAYER_ABSPIELEN = "Player-zum-Abspielen";
    public static final int SYSTEM_PLAYER_ABSPIELEN_NR = 64;
    public static final String SYSTEM_PFADE_SPEICHERN = "Pfade-zum-Speichern";
    public static final int SYSTEM_PFADE_SPEICHERN_NR = 65;
    public static final String SYSTEM_BUTTON_TOOLBAR = "Toolbar-Button";
    public static final int SYSTEM_BUTTON_TOOLBAR_NR = 66;
    // Extrafenster
    public static final String SYSTEM_FENSTER_DOWNLOAD = "Fenster-Download";
    public static final int SYSTEM_FENSTER_DOWNLOAD_NR = 67;
    public static final String SYSTEM_VIS_DOWNLOAD = "Vis-Download";
    public static final int SYSTEM_VIS_DOWNLOAD_NR = 68;
    public static final String SYSTEM_GROESSE_DOWNLOAD = "Groesse-Download";
    public static final int SYSTEM_GROESSE_DOWNLOAD_NR = 69;
    public static final String SYSTEM_FENSTER_ABO = "Fenster-Abo";
    public static final int SYSTEM_FENSTER_ABO_NR = 70;
    public static final String SYSTEM_VIS_ABO = "Vis-Abo";
    public static final int SYSTEM_VIS_ABO_NR = 71;
    public static final String SYSTEM_GROESSE_ABO = "Groesse-Abo";
    public static final int SYSTEM_GROESSE_ABO_NR = 72;
    public static final String SYSTEM_FENSTER_MELDUNGEN = "Fenster-Meldungen";
    public static final int SYSTEM_FENSTER_MELDUNGEN_NR = 73;
    public static final String SYSTEM_VIS_MELDUNGEN = "Vis-Meldungen";
    public static final int SYSTEM_VIS_MELDUNGEN_NR = 73;
    public static final String SYSTEM_GROESSE_MELDUNGEN = "Groesse-Meldungen";
    public static final int SYSTEM_GROESSE_MELDUNGEN_NR = 74;
    public static final String SYSTEM = "system";
    public static final int SYSTEM_MAX_ELEM = 75;
    public static final String[] SYSTEM_COLUMN_NAMES = {SYSTEM_ECHTZEITSUCHE, SYSTEM_LOOK, SYSTEM_USER_AGENT_AUTO, SYSTEM_USER_AGENT, SYSTEM_VERSION,
        SYSTEM_GROESSE, SYSTEM_GROESSE_EINSTELLUNGEN,
        SYSTEM_ICON_KLEIN, SYSTEM_FENSTER_MAX, SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, SYSTEM_PANEL_MELDUNGEN_ANZEIGEN, SYSTEM_PANEL_FILTER_ANZEIGEN,
        SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN,
        SYSTEM_HTTP_PROXY_SERVER, SYSTEM_HTTP_PROXY_PORT, SYSTEM_HTTP_PROXY_USER, SYSTEM_HTTP_PROXY_PWD, SYSTEM_HTTP_PROXY_ON,
        SYSTEM_MAX_DOWNLOAD, SYSTEM_LEITUNG_LOW,
        SYSTEM_SWR_LISTE, SYSTEM_PFAD_EXPORT_ABOS, SYSTEM_IMPORT_DATEI, SYSTEM_URL_FILMLISTEN, SYSTEM_IMPORT_URL_MANUELL,
        SYSTEM_EXPORT_DATEI, SYSTEM_WARTEN, SYSTEM_UDATE_SUCHEN, SYSTEM_UPDATE_DATUM, SYSTEM_IMPORT_ART_FILME,
        SYSTEM_FILTER_TAGE, SYSTEM_FILTER_KEINE_ABO, SYSTEM_FILTER_KEINE_GESEHENE, SYSTEM_FILTER_NUR_HD,
        SYSTEM_PFAD_VLC, SYSTEM_PFAD_FLVSTREAMER, SYSTEM_PFAD_MPLAYER,
        SYSTEM_EIGENSCHAFTEN_TABELLE_FILME, SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS, SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS,
        SYSTEM_ABOS_SOFORT_SUCHEN, SYSTEM_NUR_ASCII, SYSTEM_UNICODE_AENDERN, SYSTEM_HINWEIS_NR_ANGEZEIGT,
        SYSTEM_ORDNER_OEFFNEN, SYSTEM_URL_OEFFNEN,
        SYSTEM_BLACKLIST_ZUKUNFT__NICHT_ANZEIGEN, SYSTEM_BLACKLIST_AUCH_ABO, SYSTEM_BLACKLIST_AUSGESCHALTET, SYSTEM_BLACKLIST_IST_WHITELIST, SYSTEM_BLACKLIST_FILMLAENGE,
        SYSTEM_VERSION_PROGRAMMSET,
        SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN, SYSTEM_MEDUNGSFENSTER_UMBRECHEN_FEHLERMELDUNGEN, SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN,
        SYSTEM_ANSICHT_SET_LANG, SYSTEM_ICON_STANARD, SYSTEM_ICON_PFAD, SYSTEM_BREITE_MELDUNGEN, SYSTEM_BANDBREITE, SYSTEM_FILMLISTE_UMBENENNEN, SYSTEM_DOWNLOAD_SOFORT_STARTEN,
        SYSTEM_NOTIFICATION, SYSTEM_DIALOG_DOWNLOAD_D_STARTEN, SYSTEM_PLAYER_ABSPIELEN, SYSTEM_PFADE_SPEICHERN, SYSTEM_BUTTON_TOOLBAR,
        SYSTEM_FENSTER_DOWNLOAD, SYSTEM_VIS_DOWNLOAD, SYSTEM_GROESSE_DOWNLOAD,
        SYSTEM_FENSTER_ABO, SYSTEM_VIS_ABO, SYSTEM_GROESSE_ABO,
        SYSTEM_FENSTER_MELDUNGEN, SYSTEM_VIS_MELDUNGEN, SYSTEM_GROESSE_MELDUNGEN
    };
    //
}
