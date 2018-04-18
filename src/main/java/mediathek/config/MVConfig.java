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

import com.jidesoft.utils.SystemInfo;
import mSearch.daten.DatenFilm;
import mSearch.tool.Log;
import mediathek.controller.MVBandwidthTokenBucket;
import mediathek.tool.GuiFunktionenProgramme;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Set;

public class MVConfig {

    public final static String TRENNER = "#=#";
    public static final String SYSTEM = "system";
    private static final HashMap<String, String> HASHMAP = new HashMap<>();

    //das kann zukünftig entfernt werden da keine Erklärungen in der config drin sein müssen
    /*public static String PARAMETER_INFO = '\n'
            + "\t" + "\"__system-parameter__xxx\" können nur im Konfigfile geändert werden\n"
            + '\t' + "und sind auch nicht für ständige Änderungen gedacht.\n"
            + '\t' + "Wird eine Zeile gelöscht, wird der Parameter wieder mit dem Standardwert angelegt.\n"
            + '\n'
            + '\t' + Configs.SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN.cValue + '\n'
            + '\t' + "Timeout für direkte Downloads, Standardwert: " + Configs.SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN.initValue + "\n\n"
            + '\t' + Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART.cValue + '\n'
            + '\t' + "max. Startversuche für fehlgeschlagene Downloads, am Ende aller Downloads\n"
            + '\t' + "(Versuche insgesamt: DOWNLOAD_MAX_RESTART * DOWNLOAD_MAX_RESTART_HTTP), Standardwert: " + Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART.initValue + "\n\n"
            + '\t' + Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART_HTTP.cValue + '\n'
            + '\t' + "max. Startversuche für fehlgeschlagene Downloads, direkt beim Download,\n"
            + '\t' + "(Versuche insgesamt: DOWNLOAD_MAX_RESTART * DOWNLOAD_MAX_RESTART_HTTP), Standardwert: " + Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART_HTTP.initValue + "\n\n"
            + '\t' + Configs.SYSTEM_PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN.cValue + '\n'
            + '\t' + "Beim Dialog \"Download weiterführen\" wird nach dieser Zeit der Download weitergeführt, Standardwert: " + Configs.SYSTEM_PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN.initValue + "\n\n"
            + '\t' + Configs.SYSTEM_PARAMETER_DOWNLOAD_ERRORMSG_IN_SEKUNDEN.cValue + '\n'
            + '\t' + "Downloadfehlermeldung wird xx Sedunden lang angezeigt, Standardwert: " + Configs.SYSTEM_PARAMETER_DOWNLOAD_ERRORMSG_IN_SEKUNDEN.initValue + "\n\n"
            + '\t' + Configs.SYSTEM_PARAMETER_DOWNLOAD_PROGRESS.cValue + '\n'
            + '\t' + "Downloadprogress im Terminal (-auto) anzeigen: " + Configs.SYSTEM_PARAMETER_DOWNLOAD_PROGRESS.initValue + "\n\n"
            + '\t' + Configs.SYSTEM_PARAMETER_USERAGENT.cValue + '\n'
            + '\t' + "Useragent für direkte Downloads, Standardwert: " + Configs.SYSTEM_PARAMETER_USERAGENT.initValue + '\n';
            */

    public enum Configs {
        //============================================
        //Programm-Configs, änderbar nur im Konfig-File
        SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN("__system-parameter__download-timeout-sekunden_250__", "250"),//250 Sekunden, wie bei Firefox
        SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART("__system-parameter__download-max-restart_5__", "5"),// max. Startversuche für fehlgeschlagene Downloads (insgesamt: restart * restart_http Versuche)
        SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART_HTTP("__system-parameter__download-max-restart-http_10__", "10"),// max. Startversuche für fehlgeschlagene Downloads, direkt beim Download
        SYSTEM_PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN("__system-parameter__download-weiterfuehren-sekunden_60__", "60"), //Beim Dialog "Download weiterführen" wird in dieser Zeit der Download weitergeführt
        SYSTEM_PARAMETER_DOWNLOAD_ERRORMSG_IN_SEKUNDEN("__system-parameter__download-fehlermeldung-sekunden_120__", "120"),//Downloadfehlermeldung wird xx Sedunden lang angezeigt
        SYSTEM_PARAMETER_USERAGENT("__system-parameter__useragent_" + "__", Konstanten.USER_AGENT_DEFAULT),//Useragent für direkte Downloads
        SYSTEM_PARAMETER_DOWNLOAD_PROGRESS("__system-parameter__dl_progress_", Boolean.TRUE.toString()), //Downloadprogress im Terminal (-auto) anzeigen

        //============================================
        //Programm-Configs, änderbar über Gui
        SYSTEM_ECHTZEITSUCHE("Echtzeitsuche", Boolean.TRUE.toString()),
        SYSTEM_TABS_TOP("Tabs-oben", SystemInfo.isMacOSX() ? Boolean.TRUE.toString() : Boolean.FALSE.toString()),
        SYSTEM_TABS_ICON("Tabs-Icon", SystemInfo.isMacOSX() ? Boolean.FALSE.toString() : Boolean.TRUE.toString()),
        SYSTEM_USE_TRAY("Tray-anzeigen", Boolean.FALSE.toString()),
        SYSTEM_LOOK("System-look", "0"),
        SYSTEM_ABOS_SOFORT_SUCHEN("Abos-sofort-suchen", Boolean.TRUE.toString()),
        SYSTEM_USE_REPLACETABLE("Ersetzungstabelle-verwenden", SystemInfo.isLinux() || SystemInfo.isMacOSX() ? Boolean.TRUE.toString() : Boolean.FALSE.toString()),// wegen des Problems mit ext. Programmaufrufen und Leerzeichen
        SYSTEM_ONLY_ASCII("nur-ascii", Boolean.FALSE.toString()),
        SYSTEM_HINWEIS_NR_ANGEZEIGT("Hinweis-Nr-angezeigt"),
        SYSTEM_ORDNER_OEFFNEN("Download-Ordner-oeffnen"),
        SYSTEM_URL_OEFFNEN("Programm-Url-oeffnen"),
        SYSTEM_LINUX_SHUTDOWN("Programm-Linux-Shutdown"),
        SYSTEM_NOTIFICATION("Notification-anzeigen", Boolean.TRUE.toString()),
        SYSTEM_PLAYER_ABSPIELEN("Player-zum-Abspielen"),
        SYSTEM_GEO_MELDEN("Geo-melden", Boolean.TRUE.toString()),
        SYSTEM_GEO_STANDORT("Geo-Standort", DatenFilm.GEO_DE),
        // Fenstereinstellungen
        SYSTEM_GROESSE_GUI("Groesse"),
        SYSTEM_GROESSE_EINSTELLUNGEN("Groesse-Einstellungen"),
        SYSTEM_GROESSE_INFODIALOG("Groesse-Infodialog"),
        SYSTEM_FENSTER_MAX("programmfenster-maximieren"),
        SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN("system-panel-videoplayer-anzeigen"),
        SYSTEM_PANEL_DOWNLOAD_DIVIDER("system-panel-download-divider", Konstanten.GUIDOWNLOAD_DIVIDER_LOCATION),
        SYSTEM_PANEL_ABO_DIVIDER("system-panel-abo-divider", Konstanten.GUIDOWNLOAD_DIVIDER_LOCATION),
        SYSTEM_FONT_SIZE("system-font-size", "0"), // -5 ... 0 .... 5
        SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN("system-filme-beschreibung-anzeigen", Boolean.TRUE.toString()),
        SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN("system-download-beschreibung-anzeigen", Boolean.TRUE.toString()),
        SYSTEM_TAB_FILME_ANZAHL_BUTTON("system-tab-filme-anzahl-button", "4"),
        SYSTEM_FILM_INFO_TOP("system-film-info-top", Boolean.TRUE.toString()), //immer onTop anzeigen
        SYSTEM_FILM_INFO_DECORATED("system-film-info-border", Boolean.TRUE.toString()),
        SYSTEM_DOWNLOAD_INFO_TOP("system-download-info-top", Boolean.TRUE.toString()), //immer onTop anzeigen
        SYSTEM_DOWNLOAD_INFO_DECORATED("system-download-info-border", Boolean.FALSE.toString()),
        SYSTEM_EIGENSCHAFTEN_TABELLE_FILME("Eigenschaften-Tabellen-Filme"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS("Eigenschaften-Tabellen-Downloads"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS("Eigenschaften-Tabellen-Abos"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_MEDIA_DB("Eigenschaften-Tabellen-MediaDB"),
        SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN("Meldungsfenster-Systemmeldungen"),
        SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN("Meldungsfenster-Playermeldungen"),
        SYSTEM_ANSICHT_SET_LANG("Ansicht-Set-lang"),
        SYSTEM_BANDWIDTH_MONITOR_VISIBLE("Bandwidthmonitor-visible"),
        SYSTEM_ICON_STANDARD("Icon-Standard", Boolean.TRUE.toString()),
        SYSTEM_ICON_PFAD("Icon-Pfad"),
        SYSTEM_BREITE_MELDUNGEN("breite-Meldungen"),
        SYSTEM_TOOLBAR_ALLES_ANZEIGEN("Toolbar-Alles-anzeigen", Boolean.TRUE.toString()),
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
        SYSTEM_TAB_FILME_ICON_ANZEIGEN("system-tab-filme-icon-anzeigen", Boolean.TRUE.toString()),
        SYSTEM_TAB_DOWNLOAD_ICON_ANZEIGEN("system-tab-download-icon-anzeigen", Boolean.TRUE.toString()),
        SYSTEM_TAB_ABO_ICON_ANZEIGEN("system-tab-abo-icon-anzeigen", Boolean.TRUE.toString()),
        SYSTEM_TAB_FILME_ICON_KLEIN("system-tab-filme-icon-klein", Boolean.TRUE.toString()),
        SYSTEM_TAB_DOWNLOAD_ICON_KLEIN("system-tab-download-icon-klein", Boolean.TRUE.toString()),
        SYSTEM_TAB_DOWNLOAD_FILTER_VIS("system-tab-download-filter-vis", Boolean.TRUE.toString()),
        SYSTEM_TAB_ABO_FILTER_VIS("system-tab-abo-filter-vis", Boolean.FALSE.toString()),
        SYSTEM_TAB_ABO_ICON_KLEIN("system-tab-abo-icon-klein", Boolean.TRUE.toString()),
        SYSTEM_TAB_FILME_LINEBREAK("system-tab-filme-linebreak", Boolean.FALSE.toString()),
        SYSTEM_TAB_DOWNLOAD_LINEBREAK("system-tab-download-linebreak", Boolean.FALSE.toString()),
        SYSTEM_TAB_ABO_LINEBREAK("system-tab-abo-linebreak", Boolean.FALSE.toString()),
        // Extrafenster
        SYSTEM_FENSTER_DOWNLOAD("Fenster-Download"),
        SYSTEM_GROESSE_DOWNLOAD("Groesse-Download"),
        SYSTEM_FENSTER_ABO("Fenster-Abo"),
        SYSTEM_GROESSE_ABO("Groesse-Abo"),
        SYSTEM_FENSTER_MELDUNGEN("Fenster-Meldungen"),
        SYSTEM_VIS_MELDUNGEN("Vis-Meldungen"),
        SYSTEM_GROESSE_MELDUNGEN("Groesse-Meldungen"),
        SYSTEM_FENSTER_FILTER("Fenster-Filter"),
        SYSTEM_GROESSE_FILTER("Groesse-Filter"),
        //Einstellungen Filmliste
        SYSTEM_IMPORT_ART_FILME("update-filme"), // url automatisch suchen - oder nur manuell
        SYSTEM_URL_FILMLISTEN("system-url-filmlisten"),
        SYSTEM_IMPORT_URL_MANUELL("system-import-url-manuell"),
        SYSTEM_EXPORT_DATEI("system-export-datei"),
        SYSTEM_ANZ_TAGE_FILMLISTE("system-anz-tage-filmilste", "0"), // es werden nur die x letzten Tage geladen
        // Filter
        //TODO die auskommentierten Dinge später entfernen
        /*SYSTEM_FILTER_TAGE("filter-tage-start", "15"), // in Tagen
        SYSTEM_FILTER_DAUER("filter-dauer-start", "0"), // in Minuten
        SYSTEM_FILTER_DAUER_MIN("filter-dauer-min-start", Boolean.TRUE.toString()), // Dauer ist Min 
        SYSTEM_FILTER_PROFILE__DAUER("filter-dauer", "0"),
        SYSTEM_FILTER_PROFILE__DAUER_MIN("filter-dauer-min", Boolean.TRUE.toString()),
        SYSTEM_FILTER_PROFILE__TAGE("filter-tage", "15"), // index im Array GuiFilme.COMBO_ZEIT_INT
        SYSTEM_FILTER_PROFILE__KEINE_ABO("filter-keineAbo"),
        SYSTEM_FILTER_PROFILE__KEINE_GESEHENE("filter-keineGesehen"),
        SYSTEM_FILTER_PROFILE__NUR_HD("filter-nurHd"),
        SYSTEM_FILTER_PROFILE__NUR_UT("filter-nurUt"),
        SYSTEM_FILTER_PROFILE__NUR_NEUE("filter-nurNeue"),
        SYSTEM_FILTER_PROFILE__BLACKLIST_ON("filter-blacklist-aus"),
        SYSTEM_FILTER_PROFILE__NAME("filter-name"),
        SYSTEM_FILTER_PROFILE__SENDER("filter-sender"),
        SYSTEM_FILTER_PROFILE__THEMA("filter-thema"),
        SYSTEM_FILTER_PROFILE__TITEL("filter-titel"),
        SYSTEM_FILTER_PROFILE__THEMA_TITEL("filter-themaTitel"),
        SYSTEM_FILTER_PROFILE__TT("filter-TT-oder-irgendwo", Boolean.TRUE.toString()),
        SYSTEM_FILTER_PROFILE__SORT_KEY("filter-sortkey"),
        SYSTEM_FILTER_PROFILE__SORT_KEY_UPDOWN("filter-sortkey-updown"),
        SYSTEM_FILTER_PROFILE__ANZAHL_FILTER("filter-anzahl"),*/
        // Programmpfade
        SYSTEM_PFAD_VLC("pfad-vlc", GuiFunktionenProgramme.getMusterPfadVlc()),
        SYSTEM_PFAD_FLVSTREAMER("pfad-flvstreamer", GuiFunktionenProgramme.getMusterPfadFlv()),
        SYSTEM_PFAD_FFMPEG("pfad-ffmpeg", GuiFunktionenProgramme.getMusterPfadFFmpeg()),
        SYSTEM_VERSION_PROGRAMMSET("Version-Programmset"),
        // Blacklist
        SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN("Blacklist-Zukunft-nicht-anzeigen"),
        SYSTEM_BLACKLIST_GEO_NICHT_ANZEIGEN("Blacklist-Geo-nicht-anzeigen"),
        SYSTEM_BLACKLIST_AUCH_ABO("Blacklist-auch-Abo"),
        SYSTEM_BLACKLIST_START_ON("Blacklist-Start-ausgeschaltet", Boolean.FALSE.toString()),
        SYSTEM_BLACKLIST_ON("Blacklist-ausgeschaltet", Boolean.FALSE.toString()),
        SYSTEM_BLACKLIST_IST_WHITELIST("Blacklist-ist-Whitelist"),
        SYSTEM_BLACKLIST_FILMLAENGE("Blacklist-Filmlaenge", "0"),
        // Download
        SYSTEM_DIALOG_DOWNLOAD_D_STARTEN("Dialog-Download-D-Starten", Boolean.TRUE.toString()), // DialogDownload: Download sofort starten
        SYSTEM_DIALOG_DOWNLOAD_STARTEN_ZEIT("Dialog-Download-Starten-Zeit"),
        SYSTEM_DIALOG_DOWNLOAD_SHUTDOWN("Dialog-Download-Shutdown"),
        SYSTEM_DOWNLOAD_SOFORT_STARTEN("Download-sofort-starten", Boolean.FALSE.toString()),
        SYSTEM_DOWNLOAD_BEEP("Download-Beep"),
        SYSTEM_DOWNLOAD_ERRORMSG("download-error-msg", Boolean.TRUE.toString()),
        SYSTEM_BANDBREITE_KBYTE("maxBandbreite", String.valueOf(MVBandwidthTokenBucket.BANDWIDTH_MAX_KBYTE)),
        SYSTEM_MAX_DOWNLOAD("maxDownload", "1"),
        SYSTEM_MAX_1_DOWNLOAD_PRO_SERVER("max1DownloadProServer"), // nur ein Download pro Server - sonst max 2
        SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN("Pfade-zum-Speichern"), // gesammelten Downloadpfade im Downloaddialog
        SYSTEM_DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN("Letzen-Pfad-anzeigen"),
        // Abo
        SYSTEM_ABO_MIN_SIZE("Abo-Mindestdauer-Minuten"),
        // MediaDB
        SYSTEM_MEDIA_DB_DIALOG_GROESSE("Media_DB_Dialog-Groesse"),
        SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN("Media_DB_Dialog-anzeigen"),
        SYSTEM_MEDIA_DB_ECHTZEITSUCHE("Media_DB_Echtzeitsuche", Boolean.TRUE.toString()),
        SYSTEM_MEDIA_DB_SUFFIX("Media_DB_Suffix"),
        SYSTEM_MEDIA_DB_SUFFIX_OHNE("Media_DB_ohne-Suffix"),
        SYSTEM_MEDIA_DB_EXPORT_DATEI("Media_DB_export-datei"),
        //Farben
        FARBE__FILM_LIVESTREAM("FARBE_FILM_LIVESTREAM"),
        FARBE__FILM_HISTORY("FARBE_FILM_HISTORY"),
        FARBE__FILM_NEU("FARBE_FILM_NEU"),
        FARBE__FILM_GEOBLOCK_BACKGROUND("FARBE_FILM_GEOBLOCK_BACKGROUND"),
        FARBE__FILM_GEOBLOCK_BACKGROUND_SEL("FARBE_FILM_GEOBLOCK_BACKGROUND_SEL"),
        FARBE__DOWNLOAD_IST_ABO("FARBE_DOWNLOAD_IST_ABO"),
        FARBE__DOWNLOAD_IST_DIREKTER_DOWNLOAD("FARBE_DOWNLOAD_IST_DIREKTER_DOWNLOAD"),
        FARBE__DOWNLOAD_ANSEHEN("FARBE_DOWNLOAD_ANSEHEN"),
        FARBE__DOWNLOAD_WAIT("FARBE_DOWNLOAD_WAIT"),
        FARBE__DOWNLOAD_WAIT_SEL("FARBE_DOWNLOAD_WAIT_SEL"),
        FARBE__DOWNLOAD_RUN("FARBE_DOWNLOAD_RUN"),
        FARBE__DOWNLOAD_RUN_SEL("FARBE_DOWNLOAD_RUN_SEL"),
        FARBE__DOWNLOAD_FERTIG("FARBE_DOWNLOAD_FERTIG"),
        FARBE__DOWNLOAD_FERTIG_SEL("FARBE_DOWNLOAD_FERTIG_SEL"),
        FARBE__DOWNLOAD_FEHLER("FARBE_DOWNLOAD_FEHLER"),
        FARBE__DOWNLOAD_FEHLER_SEL("FARBE_DOWNLOAD_FEHLER_SEL"),
        FARBE__ABO_AUSGESCHALTET("FARBE_ABO_AUSGESCHALTET"),
        FARBE__ABO_AUSGESCHALTET_SEL("FARBE_ABO_AUSGESCHALTET_SEL"),
        FARBE__FILTER_REGEX("FARBE_FILTER_REGEX"),
        FARBE__FILTER_REGEX_FEHLER("FARBE_FILTER_REGEX_FEHLER"),
        FARBE__BUTTON_SET_ABSPIELEN("FARBE_BUTTON_SET_ABSPIELEN"),
        FARBE__FILMLISTE_LADEN_AKTIV("FARBE_FILMLISTE_LADEN_AKTIV"),
        FARBE__DOWNLOAD_DATEINAME_EXISTIERT("FARBE_DOWNLOAD_DATEINAME_EXISTIERT"),
        FARBE__DOWNLOAD_DATEINAME_NEU("FARBE_DOWNLOAD_DATEINAME_NEU"),
        FARBE__DOWNLOAD_DATEINAME_ALT("FARBE_DOWNLOAD_DATEINAME_ALT");

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

        public static boolean find(String value) {
            for (Configs conf : values()) {
                if (conf.cValue.equals(value)) {
                    return true;
                }
            }
            return false;
        }
    }

    public static void loadSystemParameter() {
        //einmal die leeren mit den inits füllen
        for (Configs key : Configs.values()) {
            String s = HASHMAP.get(key.cValue);
            if (s == null || s.isEmpty()) {
                MVConfig.add(key, key.initValue);
            }
        }

        if (Daten.isDebug()) {
            MVConfig.add(MVConfig.Configs.SYSTEM_IMPORT_ART_FILME, String.valueOf(Konstanten.UPDATE_FILME_AUS));
        }
        MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON)); // Zustand Blacklist beim Start setzen

        check(Configs.SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN, 5, 1000);
        check(Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART, 0, 100);
        check(Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART_HTTP, 0, 100);
        check(Configs.SYSTEM_PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN, 5, 1000);
        check(Configs.SYSTEM_PARAMETER_DOWNLOAD_ERRORMSG_IN_SEKUNDEN, 5, 1000);

        Log.sysLog("");
        Log.sysLog("=======================================");
        Log.sysLog("Systemparameter");
        Log.sysLog("-----------------");
        Log.sysLog("Download-Timeout [s]: " + MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN));
        Log.sysLog("max. Download-Restart: " + MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART));
        Log.sysLog("max. Download-Restart-Http: " + MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART_HTTP));
        Log.sysLog("Download weiterführen in [s]: " + MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_WEITERFUEHREN_IN_SEKUNDEN));
        Log.sysLog("Download Fehlermeldung anzeigen [s]: " + MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_ERRORMSG_IN_SEKUNDEN));
        Log.sysLog("Downoadprogress anzeigen: " + MVConfig.get(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_PROGRESS));
        Log.sysLog("Useragent: " + MVConfig.get(MVConfig.Configs.SYSTEM_PARAMETER_USERAGENT));
        Log.sysLog("=======================================");
        Log.sysLog("");
    }

    public static void check(Configs key, int min, int max) {
        int v = getInt(key);
        if (v < min || v > max) {
            add(key, key.initValue);
        }
    }

    public static synchronized void add(String key, String value) {
        HASHMAP.put(key, value);
    }

    public static synchronized void add(Configs key, String value) {
        HASHMAP.put(key.cValue, value);
    }

    private static final int MAX_FILTER = 5; //old filter profile code setting
    public static synchronized void add(Configs key, String value, int i) {
        boolean ok = false;
        String[] sa = {""};
        String s = HASHMAP.get(key.cValue);
        if (s != null) {
            sa = split(s);
            if (sa.length == MAX_FILTER) {
                sa[i] = value;
                ok = true;
            }
        }
        if (!ok) {
            // dann anlegen
            sa = initArray(key);
            sa[i] = value;
        }
        // und jetzt eintragen
        s = addArray(sa);
        HASHMAP.put(key.cValue, s);
    }

    public static synchronized String get(Configs key) {
        String s = HASHMAP.get(key.cValue);
        if (s == null) {
            s = key.initValue;
        }
        return s == null ? "" : s;
    }

    public static synchronized int getInt(Configs key) {
        int ret;
        try {
            ret = Integer.parseInt(get(key));
        } catch (Exception ignore) {
            ret = 0;
        }
        return ret;
    }

    public static synchronized boolean getBool(Configs key) {
        return Boolean.parseBoolean(get(key));
    }

    public static synchronized String get(Configs key, int i) {
        String[] sa;
        String s = HASHMAP.get(key.cValue);
        if (s == null) {
            return key.initValue;
        } else {
            sa = split(s);
        }
        if (sa.length <= i) {
            HASHMAP.remove(key.cValue);
            return key.initValue;
        } else {
            return sa[i];
        }
    }

    public static synchronized String[][] getAll() {
        final LinkedList<String[]> liste = new LinkedList<>();
        final Set<String> strings = HASHMAP.keySet();
        final String[] setArray = strings.toArray(new String[strings.size()]);
        for (String entry : setArray) {
            String[] s = new String[2];
            s[0] = entry;
            s[1] = HASHMAP.get(entry);
            liste.add(s);
        }
        listeSort(liste);

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

        return l.toArray(new String[l.size()]);

    }

    private static String addArray(String[] arr) {
        if (arr == null) {
            return "";
        }

        StringBuilder sb = new StringBuilder();
        for (int k = 0; k < arr.length; ++k) {
            sb.append(arr[k]);
            if (k < arr.length - 1) {
                sb.append(TRENNER);
            }
        }
        return sb.toString();
    }

    private static String[] initArray(Configs key) {
        String[] sa = new String[MAX_FILTER];
        for (int k = 0; k < MAX_FILTER; ++k) {
            sa[k] = key.initValue;
        }
        return sa;
    }

    private static void listeSort(LinkedList<String[]> liste) {
        //Stringliste alphabetisch sortieren
        mSearch.tool.GermanStringSorter sorter = mSearch.tool.GermanStringSorter.getInstance();
        if (liste != null) {
            String str1;
            String str2;
            for (int i = 1; i < liste.size(); ++i) {
                for (int k = i; k > 0; --k) {
                    str1 = liste.get(k - 1)[0];
                    str2 = liste.get(k)[0];
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
