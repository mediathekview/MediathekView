package mediathek.config;

import mediathek.tool.*;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Set;

public class MVConfig {

    public final static String TRENNER = "#=#";
    public static final String SYSTEM = "system";
    private static final Logger logger = LogManager.getLogger(MVConfig.class);
    private static final HashMap<String, String> HASHMAP = new HashMap<>();
    private static final int MAX_FILTER = 5; //old filter profile code setting

    public static void loadSystemParameter() {
        //einmal die leeren mit den inits füllen
        for (Configs key : Configs.values()) {
            String s = HASHMAP.get(key.cValue);
            if (s == null || s.isEmpty()) {
                MVConfig.add(key, key.initValue);
            }
        }

        if (Config.isDebugModeEnabled()) {
            logger.debug("Setting FilmList import mode to MANUAL");
            GuiFunktionen.setImportArtFilme(FilmListUpdateType.MANUAL);
        }

        MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_ON, MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_START_ON)); // Zustand Blacklist beim Start setzen

        logger.debug("User-Agent: " + ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_USER_AGENT));
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
        final String[] setArray = strings.toArray(new String[0]);
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

        return l.toArray(new String[0]);

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
        GermanStringSorter sorter = GermanStringSorter.getInstance();
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

    public enum Configs {
        //Programm-Configs, änderbar über Gui
        SYSTEM_ABOS_SOFORT_SUCHEN("Abos-sofort-suchen", Boolean.TRUE.toString()),
        SYSTEM_USE_REPLACETABLE("Ersetzungstabelle-verwenden", SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_MAC_OSX ? Boolean.TRUE.toString() : Boolean.FALSE.toString()),// wegen des Problems mit ext. Programmaufrufen und Leerzeichen
        SYSTEM_ONLY_ASCII("nur-ascii", Boolean.FALSE.toString()),
        SYSTEM_HINWEIS_NR_ANGEZEIGT("Hinweis-Nr-angezeigt"),
        SYSTEM_ORDNER_OEFFNEN("Download-Ordner-oeffnen"),
        SYSTEM_URL_OEFFNEN("Programm-Url-oeffnen"),
        SYSTEM_LINUX_SHUTDOWN("Programm-Linux-Shutdown"),
        SYSTEM_PLAYER_ABSPIELEN("Player-zum-Abspielen"),
        // Fenstereinstellungen
        SYSTEM_GROESSE_EINSTELLUNGEN("Groesse-Einstellungen"),
        SYSTEM_GROESSE_INFODIALOG("Groesse-Infodialog"),

        SYSTEM_EIGENSCHAFTEN_TABELLE_FILME("Eigenschaften-Tabellen-Filme"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS("Eigenschaften-Tabellen-Downloads"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS("Eigenschaften-Tabellen-Abos"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_MEDIA_DB("Eigenschaften-Tabellen-MediaDB"),
        SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN("Meldungsfenster-Systemmeldungen"),
        SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN("Meldungsfenster-Playermeldungen"),
        SYSTEM_ANSICHT_SET_LANG("Ansicht-Set-lang"),
        SYSTEM_TAB_FILME_ICON_ANZEIGEN("system-tab-filme-icon-anzeigen", Boolean.TRUE.toString()),
        SYSTEM_TAB_FILME_LINEBREAK("system-tab-filme-linebreak", Boolean.FALSE.toString()),
        SYSTEM_TAB_FILME_ICON_KLEIN("system-tab-filme-icon-klein", Boolean.TRUE.toString()),
        SYSTEM_TAB_DOWNLOAD_ICON_ANZEIGEN("system-tab-download-icon-anzeigen", Boolean.TRUE.toString()),
        SYSTEM_TAB_DOWNLOAD_ICON_KLEIN("system-tab-download-icon-klein", Boolean.TRUE.toString()),
        SYSTEM_TAB_DOWNLOAD_FILTER_VIS("system-tab-download-filter-vis", Boolean.TRUE.toString()),
        SYSTEM_TAB_DOWNLOAD_LINEBREAK("system-tab-download-linebreak", Boolean.FALSE.toString()),
        SYSTEM_TAB_ABO_ICON_ANZEIGEN("system-tab-abo-icon-anzeigen", Boolean.TRUE.toString()),
        SYSTEM_TAB_ABO_ICON_KLEIN("system-tab-abo-icon-klein", Boolean.TRUE.toString()),
        // Extrafenster
        SYSTEM_GROESSE_MANAGE_ABO("manage-abo-dialog-size"),
        //Einstellungen Filmliste
        SYSTEM_IMPORT_ART_FILME("update-filme"), // url automatisch suchen - oder nur manuell
        SYSTEM_IMPORT_URL_MANUELL("system-import-url-manuell"),
        // Programmpfade
        SYSTEM_PFAD_VLC("pfad-vlc", GuiFunktionenProgramme.getMusterPfadVlc()),
        SYSTEM_PFAD_FFMPEG("pfad-ffmpeg", GuiFunktionenProgramme.getMusterPfadFFmpeg()),
        SYSTEM_VERSION_PROGRAMMSET("Version-Programmset"),
        // Blacklist
        SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN("Blacklist-Zukunft-nicht-anzeigen"),
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
        SYSTEM_DOWNLOAD_ERRORMSG("download-error-msg", Boolean.TRUE.toString()),
        SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN("Pfade-zum-Speichern"), // gesammelten Downloadpfade im Downloaddialog
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
        FARBE__FILM_BOOKMARKED("FARBE_FILM_BOOKMARKED"),
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
}
