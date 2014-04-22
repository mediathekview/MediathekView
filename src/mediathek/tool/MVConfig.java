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
package mediathek.tool;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;

public class MVConfig {

    public final static String TRENNER = "#=#";

    // ################################
    // Tags System
    // ################################
    public static final String SYSTEM = "system";
    // allgemein
    public static final String SYSTEM_ECHTZEITSUCHE = "Echtzeitsuche";
    public static final String SYSTEM_LOOK = "System-look";
    public static final String SYSTEM_USER_AGENT_AUTO = "User-Agent-Auto";
    public static final String SYSTEM_USER_AGENT = "User-Agent";
    public static final String SYSTEM_UPDATE_SUCHEN = "update-suchen";
    public static final String SYSTEM_UPDATE_DATUM = "update-datum";
    public static final String SYSTEM_ABOS_SOFORT_SUCHEN = "Abos-sofort-suchen";
    public static final String SYSTEM_ZIELNAMEN_ANPASSEN = "Zielnamen-anpassen";
    public static final String SYSTEM_ZIELNAMEN_UNICODE = "Zielnamen-Unicode";
    public static final String SYSTEM_HINWEIS_NR_ANGEZEIGT = "Hinweis-Nr-angezeigt";
    public static final String SYSTEM_ORDNER_OEFFNEN = "Download-Ordner-oeffnen";
    public static final String SYSTEM_URL_OEFFNEN = "Programm-Url-oeffnen";
    public static final String SYSTEM_NOTIFICATION = "Notification-anzeigen";
    public static final String SYSTEM_PLAYER_ABSPIELEN = "Player-zum-Abspielen";
    public static final String SYSTEM_GEO_MELDEN = "Geo-melden";
    public static final String SYSTEM_GEO_STANDORT = "Geo-Standort";
    // Fenstereinstellungen
    public static final String SYSTEM_GROESSE = "Groesse";
    public static final String SYSTEM_GROESSE_EINSTELLUNGEN = "Groesse-Einstellungen";
    public static final String SYSTEM_FENSTER_MAX = "programmfenster-maximieren";
    public static final String SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN = "system-panel-videoplayer-anzeigen";
    public static final String SYSTEM_PANEL_MELDUNGEN_ANZEIGEN = "system-panel-meldungen-anzeigen";
    public static final String SYSTEM_PANEL_FILME_DIVIDER = "system-panel-filme-divider";

    //public static final String SYSTEM_PANEL_FILTER_ANZEIGEN = "system-panel-filter-anzeigen";
    public static final String SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN = "system-panel-beschreibung-anzeigen";
    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_FILME = "Eigenschaften-Tabellen-Filme";
    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS = "Eigenschaften-Tabellen-Downloads";
    public static final String SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS = "Eigenschaften-Tabellen-Abos";
    public static final String SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN = "Meldungsfenster-Systemmeldungen";
    public static final String SYSTEM_MEDUNGSFENSTER_UMBRECHEN_FEHLERMELDUNGEN = "Meldungsfenster-Fehlermeldungen";
    public static final String SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN = "Meldungsfenster-Playermeldungen";
    public static final String SYSTEM_ANSICHT_SET_LANG = "Ansicht-Set-lang";
    public static final String SYSTEM_ICON_STANDARD = "Icon-Standard";
    public static final String SYSTEM_ICON_PFAD = "Icon-Pfad";
    public static final String SYSTEM_BREITE_MELDUNGEN = "breite-Meldungen";
    public static final String SYSTEM_TOOLBAR_ALLES_ANZEIGEN = "Toolbar-Alles-anzeigen";
    public static final String SYSTEM_TOOLBAR_ALLES = "Toolbar-Alles";
    public static final String SYSTEM_TOOLBAR_DOWNLOAD_EXTERN = "Toolbar-Download-Extern";
    public static final String SYSTEM_TOOLBAR_ABO_EXTERN = "Toolbar-Abo-Extern";
    public static final String SYSTEM_TOOLBAR_MELDUNGEN = "Toolbar-Meldungen";
    public static final String SYSTEM_ICON_KLEIN_ALLES = "system-icon-groesse-alles";
    public static final String SYSTEM_ICON_KLEIN_DOWNLOADS_EXTERN = "system-icon-groesse-Download-Extern";
    public static final String SYSTEM_ICON_KLEIN_ABOS_EXTERN = "system-icon-groesse-Abos-Extern";
    // Extrafenster
    public static final String SYSTEM_FENSTER_DOWNLOAD = "Fenster-Download";
    public static final String SYSTEM_VIS_DOWNLOAD = "Vis-Download";
    public static final String SYSTEM_GROESSE_DOWNLOAD = "Groesse-Download";
    public static final String SYSTEM_FENSTER_ABO = "Fenster-Abo";
    public static final String SYSTEM_VIS_ABO = "Vis-Abo";
    public static final String SYSTEM_GROESSE_ABO = "Groesse-Abo";
    public static final String SYSTEM_FENSTER_MELDUNGEN = "Fenster-Meldungen";
    public static final String SYSTEM_VIS_MELDUNGEN = "Vis-Meldungen";
    public static final String SYSTEM_GROESSE_MELDUNGEN = "Groesse-Meldungen";
    public static final String SYSTEM_FENSTER_FILTER = "Fenster-Filter";
    public static final String SYSTEM_VIS_FILTER = "Vis-Filter";
    public static final String SYSTEM_GROESSE_FILTER = "Groesse-Filter";
    //Einstellungen Filmliste
    public static final String SYSTEM_IMPORT_ART_FILME = "update-filme"; // url automatisch suchen - oder nur manuell
    public static final String SYSTEM_URL_FILMLISTEN = "system-url-filmlisten";
    public static final String SYSTEM_IMPORT_URL_MANUELL = "system-import-url-manuell";
    public static final String SYSTEM_EXPORT_DATEI = "system-export-datei";
    // Filter
    public static final String SYSTEM_FILTER_DAUER = "filter-dauer";
    public static final String SYSTEM_FILTER_TAGE = "filter-tage"; // index im Array GuiFilme.COMBO_ZEIT_INT
    public static final String SYSTEM_FILTER_KEINE_ABO = "filter-keineAbo";
    public static final String SYSTEM_FILTER_KEINE_GESEHENE = "filter-keineGesehen";
    public static final String SYSTEM_FILTER_NUR_HD = "filter-nurHd";
    public static final String SYSTEM_FILTER_NUR_NEUE = "filter-nurNeue";
    //
    public static final String SYSTEM_FILTER_SENDER = "filter-sender";
    public static final String SYSTEM_FILTER_THEMA = "filter-thema";
    public static final String SYSTEM_FILTER_TITEL = "filter-titel";
    public static final String SYSTEM_FILTER_THEMA_TITEL = "filter-themaTitel";
    public static final String SYSTEM_FILTER_TT = "filter-TT-oder-irgendwo";
    public static final String SYSTEM_FILTER_ANZAHL = "filter-anzahl";

    //public static final String SYSTEM_FILTER_IRGENDWO = "filter-irgendwo";
    // Programmpfade
    public static final String SYSTEM_PFAD_VLC = "pfad-vlc";
    public static final String SYSTEM_PFAD_FLVSTREAMER = "pfad-flvstreamer";
    public static final String SYSTEM_PFAD_FFMPEG = "pfad-ffmpeg";
    public static final String SYSTEM_PFAD_MPLAYER = "pfad-mplayer";
    public static final String SYSTEM_VERSION_PROGRAMMSET = "Version-Programmset";
    // Blacklist
    public static final String SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN = "Blacklist-Zukunft-nicht-anzeigen";
    public static final String SYSTEM_BLACKLIST_AUCH_ABO = "Blacklist-auch-Abo";
    public static final String SYSTEM_BLACKLIST_AUSGESCHALTET = "Blacklist-ausgeschaltet";
    public static final String SYSTEM_BLACKLIST_IST_WHITELIST = "Blacklist-ist-Whitelist";
    public static final String SYSTEM_BLACKLIST_FILMLAENGE = "Blacklist-Filmlaenge";
    // Download
    public static final String SYSTEM_DIALOG_DOWNLOAD_D_STARTEN = "Dialog-Download-D-Starten"; // DialogDownload: Download sofort starten
    public static final String SYSTEM_DOWNLOAD_SOFORT_STARTEN = "Download-sofort-starten";
    public static final String SYSTEM_BANDBREITE_KBYTE = "Bandbreite";
    public static final String SYSTEM_MAX_DOWNLOAD = "maxDownload";
    public static final String SYSTEM__DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN = "Pfade-zum-Speichern"; // gesammelten Downloadpfade im Downloaddialog
    public static final String SYSTEM__DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN = "Letzen-Pfad-anzeigen";

    private final HashMap<String, String> hashmap = new HashMap<>();

    public synchronized void add(String key, String value) {
        hashmap.put(key, value);
    }

    public synchronized void add(String key, String value, int i, int max) {
        boolean ok = false;
        String[] sa = {""};
        String s = hashmap.get(key);
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
//        for (String sv : sa) {
//            if (s.isEmpty()) {
//                s = sv;
//            } else {
//                s += TRENNER + sv;
//            }
//        }
        hashmap.put(key, s);
    }

    public synchronized String get(String key) {
        String s = hashmap.get(key);
        return s == null ? "" : s;
    }

    public synchronized String get(String key, int i) {
        String[] sa = {""};
        String s = hashmap.get(key);
        if (s == null) {
            return "";
        } else {
            sa = split(s);
        }
        if (sa.length <= i) {
            hashmap.remove(key);
            return "";
        } else {
            return sa[i];
        }
    }

    public synchronized String[][] getAll() {
        LinkedList<String[]> liste = new LinkedList<>();
        String[] setArray = hashmap.keySet().toArray(new String[]{});
        for (int i = 0; i < setArray.length; ++i) {
            String[] s = new String[2];
            s[0] = setArray[i];
            s[1] = hashmap.get(setArray[i]);
            liste.add(s);
        }
        listeSort(liste, 0);
        return liste.toArray(new String[][]{});
    }

    private String[] split(String sIn) {
        ArrayList<String> l = new ArrayList<>();
        String s = sIn;
        while (s.contains(TRENNER)) {
            l.add(s.substring(0, s.indexOf(TRENNER)));
            s = s.substring(s.indexOf(TRENNER) + TRENNER.length());
        }
        l.add(s);
        return l.toArray(new String[]{});

    }

    private void listeSort(LinkedList<String[]> liste, int stelle) {
        //Stringliste alphabetisch sortieren
        msearch.tool.GermanStringSorter sorter = msearch.tool.GermanStringSorter.getInstance();
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
