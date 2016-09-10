/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;

public class MVConfig {

    public enum Configs {
        SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN("__system-parameter__download-timeout-sekunden", "250"),//250 Sekunden, wie bei Firefox
        SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART("__system-parameter__download-max-restart", "5"),// max. Startversuche für fehlgeschlagene Downloads (insgesamt: restart * restart_http Versuche)
        SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART_HTTP("__system-parameter__download-max-restart-http", "10"),// max. Startversuche für fehlgeschlagene Downloads, direkt beim Download
        SYSTEM_PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN("__system-parameter__download-weiterfuehren-sekunden", "60"), //Beim Dialog "Download weiterführen" wird in dieser Zeit der Download weitergeführt
        SYSTEM_PARAMETER_DOWNLOAD_ERRORMSG_IN_SEKUNDEN("__system-parameter__download-fehlermeldung-sekunden", "120"),//Downloadfehlermeldung wird xx Sedunden lang angezeigt
        SYSTEM_BUILD_NR("BuildNr"),
        SYSTEM_ECHTZEITSUCHE("Echtzeitsuche"),
        SYSTEM_TABS_TOP("Tabs-oben"),
        SYSTEM_TABS_ICON("Tabs-Icon"),
        SYSTEM_USE_TRAY("Tray-anzeigen"),
        SYSTEM_LOOK("System-look"),
        SYSTEM_USER_AGENT_AUTO("User-Agent-Auto"),
        SYSTEM_USER_AGENT("User-Agent"),
        SYSTEM_UPDATE_SUCHEN("update-suchen"),
        SYSTEM_UPDATE_DATUM("update-datum"),
        SYSTEM_ABOS_SOFORT_SUCHEN("Abos-sofort-suchen"),
        SYSTEM_ZIELNAMEN_ANPASSEN("Zielnamen-anpassen"),
        SYSTEM_USE_REPLACETABLE("Ersetzungstabelle-verwenden"),
        SYSTEM_ONLY_ASCII("nur-ascii"),
        SYSTEM_HINWEIS_NR_ANGEZEIGT("Hinweis-Nr-angezeigt"),
        SYSTEM_ORDNER_OEFFNEN("Download-Ordner-oeffnen"),
        SYSTEM_URL_OEFFNEN("Programm-Url-oeffnen"),
        SYSTEM_LINUX_SHUTDOWN("Programm-Linux-Shutdown"),
        SYSTEM_NOTIFICATION("Notification-anzeigen"),
        SYSTEM_PLAYER_ABSPIELEN("Player-zum-Abspielen"),
        SYSTEM_GEO_MELDEN("Geo-melden"),
        SYSTEM_GEO_STANDORT("Geo-Standort"),
        SYSTEM_GROESSE_GUI("Groesse"),
        SYSTEM_GROESSE_EINSTELLUNGEN("Groesse-Einstellungen"),
        SYSTEM_GROESSE_INFODIALOG("Groesse-Infodialog"),
        SYSTEM_DIVIDER_INFODIALOG("Divider-Infodialog"),
        SYSTEM_FENSTER_MAX("programmfenster-maximieren"),
        SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN("system-panel-videoplayer-anzeigen"),
        SYSTEM_PANEL_MELDUNGEN_ANZEIGEN("system-panel-meldungen-anzeigen"),
        SYSTEM_PANEL_FILME_DIVIDER("system-panel-filme-divider"),
        SYSTEM_FONT_SIZE("system-font-size"), // -5 ... 0 .... 5

        SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN("system-filme-beschreibung-anzeigen"),
        SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN("system-download-beschreibung-anzeigen"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_FILME("Eigenschaften-Tabellen-Filme"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS("Eigenschaften-Tabellen-Downloads"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS("Eigenschaften-Tabellen-Abos"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_MEDIA_DB("Eigenschaften-Tabellen-MediaDB"),
        SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN("Meldungsfenster-Systemmeldungen"),
        SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN("Meldungsfenster-Playermeldungen"),
        SYSTEM_ANSICHT_SET_LANG("Ansicht-Set-lang"),
        SYSTEM_BANDWIDTH_MONITOR_VISIBLE("Bandwidthmonitor-visible"),
        SYSTEM_ICON_STANDARD("Icon-Standard"),
        SYSTEM_ICON_PFAD("Icon-Pfad"),
        SYSTEM_BREITE_MELDUNGEN("breite-Meldungen"),
        SYSTEM_TOOLBAR_ALLES_ANZEIGEN("Toolbar-Alles-anzeigen"),
        SYSTEM_TOOLBAR_ALLES("Toolbar-Alles"),
        SYSTEM_TOOLBAR_DOWNLOAD_EXTERN("Toolbar-Download-Extern"),
        SYSTEM_TOOLBAR_ABO_EXTERN("Toolbar-Abo-Extern"),
        SYSTEM_TOOLBAR_MELDUNGEN("Toolbar-Meldungen"),
        SYSTEM_ICON_KLEIN_ALLES("system-icon-groesse-alles"),
        SYSTEM_ICON_KLEIN_DOWNLOADS_EXTERN("system-icon-groesse-Download-Extern"),
        SYSTEM_ICON_KLEIN_ABOS_EXTERN("system-icon-groesse-Abos-Extern"),
        SYSTEM_ICON_KLEIN("system-icon-klein"),
        SYSTEM_TOOLBAR_FILME("Toolbar-Filme"),
        SYSTEM_ICON_KLEIN_FILME("system-icon-groesse-Filme"),
        SYSTEM_TOOLBAR_DOWNLOAD("Toolbar-Download"),
        SYSTEM_ICON_KLEIN_DOWNLOAD("system-icon-groesse-Download"),
        SYSTEM_TOOLBAR_ABO("Toolbar-Abo"),
        SYSTEM_ICON_KLEIN_ABO("system-icon-groesse-Abo"),
        SYSTEM_TAB_FILME_ICON_ANZEIGEN("system-tab-filme-icon-anzeigen"),
        SYSTEM_TAB_DOWNLOAD_ICON_ANZEIGEN("system-tab-download-icon-anzeigen"),
        SYSTEM_TAB_ABO_ICON_ANZEIGEN("system-tab-abo-icon-anzeigen"),
        SYSTEM_TAB_FILME_ICON_KLEIN("system-tab-filme-icon-klein"),
        SYSTEM_TAB_DOWNLOAD_ICON_KLEIN("system-tab-download-icon-klein"),
        SYSTEM_TAB_ABO_ICON_KLEIN("system-tab-abo-icon-klein"),
        SYSTEM_FENSTER_DOWNLOAD("Fenster-Download"),
        SYSTEM_GROESSE_DOWNLOAD("Groesse-Download"),
        SYSTEM_FENSTER_ABO("Fenster-Abo"),
        SYSTEM_GROESSE_ABO("Groesse-Abo"),
        SYSTEM_FENSTER_MELDUNGEN("Fenster-Meldungen"),
        SYSTEM_VIS_MELDUNGEN("Vis-Meldungen"),
        SYSTEM_GROESSE_MELDUNGEN("Groesse-Meldungen"),
        SYSTEM_FENSTER_FILTER("Fenster-Filter"),
        SYSTEM_VIS_FILTER("Vis-Filter"),
        SYSTEM_GROESSE_FILTER("Groesse-Filter"),
        SYSTEM_IMPORT_ART_FILME("update-filme"), // url automatisch suchen - oder nur manuell
        SYSTEM_URL_FILMLISTEN("system-url-filmlisten"),
        SYSTEM_IMPORT_URL_MANUELL("system-import-url-manuell"),
        SYSTEM_EXPORT_DATEI("system-export-datei"),
        SYSTEM_ANZ_TAGE_FILMLISTE("system-anz-tage-filmilste"), // es werden nur die x letzten Tage geladen

        SYSTEM_FILTER_TAGE("filter-tage-start"), // in Tagen
        SYSTEM_FILTER_DAUER("filter-dauer-start"), // in Minuten
        SYSTEM_FILTER_PROFILE__DAUER("filter-dauer"),
        SYSTEM_FILTER_PROFILE__TAGE("filter-tage"), // index im Array GuiFilme.COMBO_ZEIT_INT
        SYSTEM_FILTER_PROFILE__KEINE_ABO("filter-keineAbo"),
        SYSTEM_FILTER_PROFILE__KEINE_GESEHENE("filter-keineGesehen"),
        SYSTEM_FILTER_PROFILE__NUR_HD("filter-nurHd"),
        SYSTEM_FILTER_PROFILE__NUR_NEUE("filter-nurNeue"),
        SYSTEM_FILTER_PROFILE__BLACKLIST_ON("filter-blacklist-aus"),
        SYSTEM_FILTER_PROFILE__NAME("filter-name"),
        SYSTEM_FILTER_PROFILE__SENDER("filter-sender"),
        SYSTEM_FILTER_PROFILE__THEMA("filter-thema"),
        SYSTEM_FILTER_PROFILE__TITEL("filter-titel"),
        SYSTEM_FILTER_PROFILE__THEMA_TITEL("filter-themaTitel"),
        SYSTEM_FILTER_PROFILE__TT("filter-TT-oder-irgendwo"),
        SYSTEM_FILTER_PROFILE__ANZAHL_FILTER("filter-anzahl"),
        SYSTEM_PFAD_VLC("pfad-vlc"),
        SYSTEM_PFAD_FLVSTREAMER("pfad-flvstreamer"),
        SYSTEM_PFAD_FFMPEG("pfad-ffmpeg"),
        SYSTEM_VERSION_PROGRAMMSET("Version-Programmset"),
        SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN("Blacklist-Zukunft-nicht-anzeigen"),
        SYSTEM_BLACKLIST_GEO_NICHT_ANZEIGEN("Blacklist-Geo-nicht-anzeigen"),
        SYSTEM_BLACKLIST_AUCH_ABO("Blacklist-auch-Abo"),
        SYSTEM_BLACKLIST_START_ON("Blacklist-Start-ausgeschaltet"),
        SYSTEM_BLACKLIST_ON("Blacklist-ausgeschaltet"),
        SYSTEM_BLACKLIST_IST_WHITELIST("Blacklist-ist-Whitelist"),
        SYSTEM_BLACKLIST_FILMLAENGE("Blacklist-Filmlaenge"),
        SYSTEM_DIALOG_DOWNLOAD_D_STARTEN("Dialog-Download-D-Starten"), // DialogDownload: Download sofort starten
        SYSTEM_DIALOG_DOWNLOAD_STARTEN_ZEIT("Dialog-Download-Starten-Zeit"),
        SYSTEM_DIALOG_DOWNLOAD_SHUTDOWN("Dialog-Download-Shutdown"),
        SYSTEM_DOWNLOAD_SOFORT_STARTEN("Download-sofort-starten"),
        SYSTEM_DOWNLOAD_BEEP("Download-Beep"),
        SYSTEM_BANDBREITE_KBYTE("maxBandbreite"),
        SYSTEM_MAX_DOWNLOAD("maxDownload"),
        SYSTEM_MAX_1_DOWNLOAD_PRO_SERVER("max1DownloadProServer"), // nur ein Download pro Server - sonst max 2
        SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN("Pfade-zum-Speichern"), // gesammelten Downloadpfade im Downloaddialog
        SYSTEM_DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN("Letzen-Pfad-anzeigen"),
        SYSTEM_ABO_MIN_SIZE("Abo-Mindestdauer-Minuten"),
        SYSTEM_MEDIA_DB_DIALOG_GROESSE("Media_DB_Dialog-Groesse"),
        SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN("Media_DB_Dialog-anzeigen"),
        SYSTEM_MEDIA_DB_ECHTZEITSUCHE("Media_DB_Echtzeitsuche"),
        SYSTEM_MEDIA_DB_SUFFIX("Media_DB_Suffix"),
        SYSTEM_MEDIA_DB_SUFFIX_OHNE("Media_DB_ohne-Suffix"),
        SYSTEM_MEDIA_DB_EXPORT_DATEI("Media_DB_export-datei");

        public final String cValue;
        public final String initValue;

        Configs(String value) {
            cValue = value;
            initValue = "";
        }

        Configs(String value, String init) {
            cValue = value;
            initValue = init;
        }

        public static Configs find(String value) {
            for (Configs conf : values()) {
                if (conf.cValue.equals(value)) {
                    return conf;
                }
            }
            return null;
        }
    }

//    //Programmparameter fürs Konfigfile
//    public static final int PARAMETER_TIMEOUT_SEKUNDEN = 250; //250 Sekunden, wie bei Firefox
//    public static final int PARAMETER_DOWNLOAD_MAX_RESTART = 5; // max. Startversuche für fehlgeschlagene Downloads (insgesamt: restart * restart_http Versuche)
//    public static final int PARAMETER_DOWNLOAD_MAX_RESTART_HTTP = 10; // max. Startversuche für fehlgeschlagene Downloads, direkt beim Download
//    public static final int PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN = 60; //Beim Dialog "Download weiterführen" wird in dieser Zeit der Download weitergeführt
//    public static final int PARAMETER_DOWNLOAD_ERRORMSG_IN_SEKUNDEN = 120; //Downloadfehlermeldung wird xx Sedunden lang angezeigt
    //Programmparameter
//    public static final String SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN = "__system-parameter__download-timeout-sekunden__" + PARAMETER_TIMEOUT_SEKUNDEN;
//    public static final String SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART = "__system-parameter__download-max-restart__" + PARAMETER_DOWNLOAD_MAX_RESTART;
//    public static final String SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART_HTTP = "__system-parameter__download-max-restart-http__" + PARAMETER_DOWNLOAD_MAX_RESTART_HTTP;
//    public static final String SYSTEM_PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN = "__system-parameter__download-weiterfuehren-sekunden__" + PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN;
//    public static final String SYSTEM_PARAMETER_DOWNLOAD_ERRORMSG_IN_SEKUNDEN = "__system-parameter__download-fehlermeldung-sekunden__" + PARAMETER_DOWNLOAD_ERRORMSG_IN_SEKUNDEN;
    public final static String TRENNER = "#=#";

    // ################################
    // Tags System
    // ################################
    public static final String SYSTEM = "system";
    // allgemein
//    public static final String SYSTEM_BUILD_NR = "BuildNr";
//    public static final String SYSTEM_ECHTZEITSUCHE = "Echtzeitsuche";
//    public static final String SYSTEM_TABS_TOP = "Tabs-oben";
//    public static final String SYSTEM_TABS_ICON = "Tabs-Icon";
//    public static final String SYSTEM_USE_TRAY = "Tray-anzeigen";
//    public static final String SYSTEM_LOOK = "System-look";
//    public static final String SYSTEM_USER_AGENT_AUTO = "User-Agent-Auto";
//    public static final String SYSTEM_USER_AGENT = "User-Agent";
//    public static final String SYSTEM_UPDATE_SUCHEN = "update-suchen";
//    public static final String SYSTEM_UPDATE_DATUM = "update-datum";
//    public static final String SYSTEM_ABOS_SOFORT_SUCHEN = "Abos-sofort-suchen";
//    public static final String SYSTEM_ZIELNAMEN_ANPASSEN = "Zielnamen-anpassen";
//    public static final String SYSTEM_USE_REPLACETABLE = "Ersetzungstabelle-verwenden";
//    public static final String SYSTEM_ONLY_ASCII = "nur-ascii";
//    public static final String SYSTEM_HINWEIS_NR_ANGEZEIGT = "Hinweis-Nr-angezeigt";
//    public static final String SYSTEM_ORDNER_OEFFNEN = "Download-Ordner-oeffnen";
//    public static final String SYSTEM_URL_OEFFNEN = "Programm-Url-oeffnen";
//    public static final String SYSTEM_LINUX_SHUTDOWN = "Programm-Linux-Shutdown";
//    public static final String SYSTEM_NOTIFICATION = "Notification-anzeigen";
//    public static final String SYSTEM_PLAYER_ABSPIELEN = "Player-zum-Abspielen";
//    public static final String SYSTEM_GEO_MELDEN = "Geo-melden";
//    public static final String SYSTEM_GEO_STANDORT = "Geo-Standort";
    // Fenstereinstellungen
//    public static final String SYSTEM_GROESSE_GUI = "Groesse";
//    public static final String SYSTEM_GROESSE_EINSTELLUNGEN = "Groesse-Einstellungen";

//    public static final String SYSTEM_GROESSE_INFODIALOG = "Groesse-Infodialog";
//    public static final String SYSTEM_DIVIDER_INFODIALOG = "Divider-Infodialog";
//    public static final String SYSTEM_FENSTER_MAX = "programmfenster-maximieren";
//    public static final String SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN = "system-panel-videoplayer-anzeigen";
//    public static final String SYSTEM_PANEL_MELDUNGEN_ANZEIGEN = "system-panel-meldungen-anzeigen";
//    public static final String SYSTEM_PANEL_FILME_DIVIDER = "system-panel-filme-divider";
//    public static final String SYSTEM_FONT_SIZE = "system-font-size"; // -5 ... 0 .... 5
//    public static final String SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN = "system-filme-beschreibung-anzeigen";
//    public static final String SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN = "system-download-beschreibung-anzeigen";
//    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_FILME = "Eigenschaften-Tabellen-Filme";
//    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS = "Eigenschaften-Tabellen-Downloads";
//    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS = "Eigenschaften-Tabellen-Abos";
//    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_MEDIA_DB = "Eigenschaften-Tabellen-MediaDB";
//    public static final String SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN = "Meldungsfenster-Systemmeldungen";
//    public static final String SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN = "Meldungsfenster-Playermeldungen";
//    public static final String SYSTEM_ANSICHT_SET_LANG = "Ansicht-Set-lang";
//    public static final String SYSTEM_BANDWIDTH_MONITOR_VISIBLE = "Bandwidthmonitor-visible";
//    public static final String SYSTEM_ICON_STANDARD = "Icon-Standard";
//    public static final String SYSTEM_ICON_PFAD = "Icon-Pfad";
//    public static final String SYSTEM_BREITE_MELDUNGEN = "breite-Meldungen";
//    public static final String SYSTEM_TOOLBAR_ALLES_ANZEIGEN = "Toolbar-Alles-anzeigen";
//    public static final String SYSTEM_TOOLBAR_ALLES = "Toolbar-Alles";
//    public static final String SYSTEM_TOOLBAR_DOWNLOAD_EXTERN = "Toolbar-Download-Extern";
//    public static final String SYSTEM_TOOLBAR_ABO_EXTERN = "Toolbar-Abo-Extern";
//    public static final String SYSTEM_TOOLBAR_MELDUNGEN = "Toolbar-Meldungen";
//    public static final String SYSTEM_ICON_KLEIN_ALLES = "system-icon-groesse-alles";
//    public static final String SYSTEM_ICON_KLEIN_DOWNLOADS_EXTERN = "system-icon-groesse-Download-Extern";
//    public static final String SYSTEM_ICON_KLEIN_ABOS_EXTERN = "system-icon-groesse-Abos-Extern";
//    public static final String SYSTEM_ICON_KLEIN = "system-icon-klein";
//    public static final String SYSTEM_TOOLBAR_FILME = "Toolbar-Filme";
//    public static final String SYSTEM_ICON_KLEIN_FILME = "system-icon-groesse-Filme";
//    public static final String SYSTEM_TOOLBAR_DOWNLOAD = "Toolbar-Download";
//    public static final String SYSTEM_ICON_KLEIN_DOWNLOAD = "system-icon-groesse-Download";
//    public static final String SYSTEM_TOOLBAR_ABO = "Toolbar-Abo";
//    public static final String SYSTEM_ICON_KLEIN_ABO = "system-icon-groesse-Abo";
//    public static final String SYSTEM_TAB_FILME_ICON_ANZEIGEN = "system-tab-filme-icon-anzeigen";
//    public static final String SYSTEM_TAB_DOWNLOAD_ICON_ANZEIGEN = "system-tab-download-icon-anzeigen";
//    public static final String SYSTEM_TAB_ABO_ICON_ANZEIGEN = "system-tab-abo-icon-anzeigen";
//    public static final String SYSTEM_TAB_FILME_ICON_KLEIN = "system-tab-filme-icon-klein";
//    public static final String SYSTEM_TAB_DOWNLOAD_ICON_KLEIN = "system-tab-download-icon-klein";
//    public static final String SYSTEM_TAB_ABO_ICON_KLEIN = "system-tab-abo-icon-klein";
    // Extrafenster
//    public static final String SYSTEM_FENSTER_DOWNLOAD = "Fenster-Download";
//    public static final String SYSTEM_GROESSE_DOWNLOAD = "Groesse-Download";
//    public static final String SYSTEM_FENSTER_ABO = "Fenster-Abo";
//    public static final String SYSTEM_GROESSE_ABO = "Groesse-Abo";
//    public static final String SYSTEM_FENSTER_MELDUNGEN = "Fenster-Meldungen";
//    public static final String SYSTEM_VIS_MELDUNGEN = "Vis-Meldungen";
//    public static final String SYSTEM_GROESSE_MELDUNGEN = "Groesse-Meldungen";
//    public static final String SYSTEM_FENSTER_FILTER = "Fenster-Filter";
//    public static final String SYSTEM_VIS_FILTER = "Vis-Filter";
//    public static final String SYSTEM_GROESSE_FILTER = "Groesse-Filter";
    //Einstellungen Filmliste
//    public static final String SYSTEM_IMPORT_ART_FILME = "update-filme"; // url automatisch suchen - oder nur manuell
//    public static final String SYSTEM_URL_FILMLISTEN = "system-url-filmlisten";
//    public static final String SYSTEM_IMPORT_URL_MANUELL = "system-import-url-manuell";
//    public static final String SYSTEM_EXPORT_DATEI = "system-export-datei";
//    public static final String SYSTEM_ANZ_TAGE_FILMLISTE = "system-anz-tage-filmilste"; // es werden nur die x letzten Tage geladen
    // Filter
//    public static final String SYSTEM_FILTER_TAGE = "filter-tage-start"; // in Tagen
//    public static final String SYSTEM_FILTER_DAUER = "filter-dauer-start"; // in Minuten
//    public static final String SYSTEM_FILTER_PROFILE__DAUER = "filter-dauer";
//    public static final String SYSTEM_FILTER_PROFILE__TAGE = "filter-tage"; // index im Array GuiFilme.COMBO_ZEIT_INT
//    public static final String SYSTEM_FILTER_PROFILE__KEINE_ABO = "filter-keineAbo";
//    public static final String SYSTEM_FILTER_PROFILE__KEINE_GESEHENE = "filter-keineGesehen";
//    public static final String SYSTEM_FILTER_PROFILE__NUR_HD = "filter-nurHd";
//    public static final String SYSTEM_FILTER_PROFILE__NUR_NEUE = "filter-nurNeue";
//    public static final String SYSTEM_FILTER_PROFILE__BLACKLIST_ON = "filter-blacklist-aus";
//    public static final String SYSTEM_FILTER_PROFILE__NAME = "filter-name";
    //
//    public static final String SYSTEM_FILTER_PROFILE__SENDER = "filter-sender";
//    public static final String SYSTEM_FILTER_PROFILE__THEMA = "filter-thema";
//    public static final String SYSTEM_FILTER_PROFILE__TITEL = "filter-titel";
//    public static final String SYSTEM_FILTER_PROFILE__THEMA_TITEL = "filter-themaTitel";
//    public static final String SYSTEM_FILTER_PROFILE__TT = "filter-TT-oder-irgendwo";
//    public static final String SYSTEM_FILTER_PROFILE__ANZAHL_FILTER = "filter-anzahl";
    // Programmpfade
//    public static final String SYSTEM_PFAD_VLC = "pfad-vlc";
//    public static final String SYSTEM_PFAD_FLVSTREAMER = "pfad-flvstreamer";
//    public static final String SYSTEM_PFAD_FFMPEG = "pfad-ffmpeg";
//    public static final String SYSTEM_VERSION_PROGRAMMSET = "Version-Programmset";
    // Blacklist
//    public static final String SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN = "Blacklist-Zukunft-nicht-anzeigen";
//    public static final String SYSTEM_BLACKLIST_GEO_NICHT_ANZEIGEN = "Blacklist-Geo-nicht-anzeigen";
//    public static final String SYSTEM_BLACKLIST_AUCH_ABO = "Blacklist-auch-Abo";
//    public static final String SYSTEM_BLACKLIST_START_ON = "Blacklist-Start-ausgeschaltet";
//    public static final String SYSTEM_BLACKLIST_ON = "Blacklist-ausgeschaltet";
//    public static final String SYSTEM_BLACKLIST_IST_WHITELIST = "Blacklist-ist-Whitelist";
//    public static final String SYSTEM_BLACKLIST_FILMLAENGE = "Blacklist-Filmlaenge";
    // Download
//    public static final String SYSTEM_DIALOG_DOWNLOAD_D_STARTEN = "Dialog-Download-D-Starten"; // DialogDownload: Download sofort starten
//    public static final String SYSTEM_DIALOG_DOWNLOAD_STARTEN_ZEIT = "Dialog-Download-Starten-Zeit";
//    public static final String SYSTEM_DIALOG_DOWNLOAD_SHUTDOWN = "Dialog-Download-Shutdown";
//    public static final String SYSTEM_DOWNLOAD_SOFORT_STARTEN = "Download-sofort-starten";
//    public static final String SYSTEM_DOWNLOAD_BEEP = "Download-Beep";
//    public static final String SYSTEM_BANDBREITE_KBYTE = "maxBandbreite";
//    public static final String SYSTEM_MAX_DOWNLOAD = "maxDownload";
//    public static final String SYSTEM_MAX_1_DOWNLOAD_PRO_SERVER = "max1DownloadProServer"; // nur ein Download pro Server - sonst max 2
//    public static final String SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN = "Pfade-zum-Speichern"; // gesammelten Downloadpfade im Downloaddialog
//    public static final String SYSTEM_DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN = "Letzen-Pfad-anzeigen";
    // Abo
//    public static final String SYSTEM_ABO_MIN_SIZE = "Abo-Mindestdauer-Minuten";
    // MediaDB
//    public static final String SYSTEM_MEDIA_DB_DIALOG_GROESSE = "Media_DB_Dialog-Groesse";
//    public static final String SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN = "Media_DB_Dialog-anzeigen";
//    public static final String SYSTEM_MEDIA_DB_ECHTZEITSUCHE = "Media_DB_Echtzeitsuche";
//    public static final String SYSTEM_MEDIA_DB_SUFFIX = "Media_DB_Suffix";
//    public static final String SYSTEM_MEDIA_DB_SUFFIX_OHNE = "Media_DB_ohne-Suffix";
//    public static final String SYSTEM_MEDIA_DB_EXPORT_DATEI = "Media_DB_export-datei";
    private static final HashMap<String, String> hashmap = new HashMap<>();

    public static synchronized void add(String key, String value) {
        hashmap.put(key, value);
    }

    public static synchronized void add(Configs key, String value) {
        hashmap.put(key.cValue, value);
    }

    public static synchronized void add(Configs key, String value, int i, int max) {
        boolean ok = false;
        String[] sa = {""};
        String s = hashmap.get(key.cValue);
        if (s != null) {
            sa = split(s);
            if (sa.length == max) {
                sa[i] = value;
                ok = true;
            }
        }
        if (!ok) {
            // dann anlegen
            sa = new String[max];
            for (int k = 0; k < max; ++k) {
                sa[k] = "";
            }
            sa[i] = value;
        }
        // und jetzt eintragen
        s = "";
        for (int k = 0; k < max; ++k) {
            s += sa[k];
            if (k < max - 1) {
                s += TRENNER;
            }
        }
        hashmap.put(key.cValue, s);
    }

    public static synchronized String get(String key) {
        String s = hashmap.get(key);
        return s == null ? "" : s;
    }

    public static synchronized String get(Configs key) {
        String s = hashmap.get(key.cValue);
        return s == null ? "" : s;
    }

    public static synchronized int getInt(Configs key) {
        int ret;
        try {
            ret = Integer.parseInt(hashmap.get(key.cValue));
        } catch (Exception ignore) {
            MVConfig.add(key.cValue, key.initValue);
            try {
                ret = Integer.parseInt(hashmap.get(key.cValue));
            } catch (Exception ig) {
                ret = 0;
            }
        }
        return ret;
    }

    public static synchronized String get(Configs key, int i) {
        String[] sa;
        String s = hashmap.get(key.cValue);
        if (s == null) {
            return "";
        } else {
            sa = split(s);
        }
        if (sa.length <= i) {
            hashmap.remove(key.cValue);
            return "";
        } else {
            return sa[i];
        }
    }

    public static synchronized String[][] getAll() {
        LinkedList<String[]> liste = new LinkedList<>();
        String[] setArray = hashmap.keySet().toArray(new String[]{});
        for (String entry : setArray) {
            String[] s = new String[2];
            s[0] = entry;
            s[1] = hashmap.get(entry);
            liste.add(s);
        }
        listeSort(liste, 0);
        return liste.toArray(new String[][]{});
    }

    private static String[] split(String sIn) {
        ArrayList<String> l = new ArrayList<>();
        String s = sIn;
        while (s.contains(TRENNER)) {
            l.add(s.substring(0, s.indexOf(TRENNER)));
            s = s.substring(s.indexOf(TRENNER) + TRENNER.length());
        }
        l.add(s);
        return l.toArray(new String[]{});

    }

    private static void listeSort(LinkedList<String[]> liste, int stelle) {
        //Stringliste alphabetisch sortieren
        mSearch.tool.GermanStringSorter sorter = mSearch.tool.GermanStringSorter.getInstance();
        if (liste != null) {
            String str1;
            String str2;
            for (int i = 1; i < liste.size(); ++i) {
                for (int k = i; k > 0; --k) {
                    str1 = liste.get(k - 1)[stelle];
                    str2 = liste.get(k)[stelle];
                    // if (str1.compareToIgnoreCase(str2) > 0) {
                    if (sorter.compare(str1, str2) > 0) {
                        liste.add(k - 1, liste.remove(k));
                    } else {
                        break;
                    }
                }
            }
        }
    }
}
