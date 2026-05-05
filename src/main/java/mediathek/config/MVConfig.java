package mediathek.config;

import mediathek.tool.*;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;

public class MVConfig {

    public final static String TRENNER = "#=#";
    public static final String SYSTEM = "system";
    private static final Logger logger = LogManager.getLogger(MVConfig.class);
    private static final HashMap<String, String> HASHMAP = new HashMap<>();

    public static void loadSystemParameter() {
        //einmal die leeren mit den inits füllen
        for (Configs key : Configs.values()) {
            String s = HASHMAP.get(key.cValue);
            if (s == null || s.isEmpty()) {
                MVConfig.add(key, key.initValue);
            }
        }

        if (Config.isDebugModeEnabled()) {
            logger.debug("Debug mode enabled - Setting FilmList import mode to MANUAL");
            GuiFunktionen.setFilmListUpdateType(FilmListUpdateType.MANUAL);
        }

        logger.debug("User-Agent: {}", ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_USER_AGENT));
    }

    public static synchronized void add(String key, String value) {
        HASHMAP.put(key, value);
    }

    public static synchronized void add(Configs key, String value) {
        HASHMAP.put(key.cValue, value);
    }

    public static synchronized void remove(String key) {
        HASHMAP.remove(key);
    }

    public static synchronized void remove(Configs key) {
        HASHMAP.remove(key.cValue);
    }

    public static synchronized String get(Configs key) {
        String s = HASHMAP.get(key.cValue);
        if (s == null) {
            s = key.initValue;
        }
        return s == null ? "" : s;
    }

    public static synchronized String get(String key) {
        String value = HASHMAP.get(key);
        return value == null ? "" : value;
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

    public static synchronized List<String[]> getSortedKVList() {
        final List<String[]> liste = new ArrayList<>();

        for (String entry : HASHMAP.keySet()) {
            liste.add(new String[]{entry, HASHMAP.get(entry)});
        }

        GermanStringSorter sorter = GermanStringSorter.INSTANCE;
        liste.sort((o1, o2) -> sorter.compare(o1[0],o2[0]));

        return liste;
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

        SYSTEM_EIGENSCHAFTEN_TABELLE_FILME("Eigenschaften-Tabellen-Filme"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS("Eigenschaften-Tabellen-Downloads"),
        SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS("Eigenschaften-Tabellen-Abos"),
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
        SYSTEM_BLACKLIST_IST_WHITELIST("Blacklist-ist-Whitelist"),
        SYSTEM_BLACKLIST_FILMLAENGE("Blacklist-Filmlaenge", "0"),
        // Download
        SYSTEM_DOWNLOAD_SOFORT_STARTEN("Download-sofort-starten", Boolean.FALSE.toString()),
        SYSTEM_DOWNLOAD_ERRORMSG("download-error-msg", Boolean.TRUE.toString()),
        SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN("Pfade-zum-Speichern"), // gesammelten Downloadpfade im Downloaddialog
        // Abo
        SYSTEM_ABO_MIN_SIZE("Abo-Mindestdauer-Minuten");

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

        private static final EnumSet<MVConfig.Configs> CONFIGS_ENUM_SET = EnumSet.allOf(MVConfig.Configs.class);

        public static boolean find(@NonNull final String value) {
            return CONFIGS_ENUM_SET.stream().anyMatch(e -> e.cValue.equals(value));
        }
    }
}
