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

import java.io.File;
import java.util.Iterator;
import mediathek.Log;
import mediathek.daten.DDaten;
import mediathek.daten.DatenPgruppe;
import mediathek.daten.DatenProg;
import mediathek.gui.dialog.DialogHilfe;

public class GuiFunktionenProgramme {

    private static final String PIPE = "| ";
    private static final String LEER = "      ";
    private static final String PFEIL = " -> ";

    public static void addStandardprogrammeAbo(DDaten daten) {
        // Erster Start und noch nichts eingerichtet, initialisieren
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            addStandardprogrammeAboWin(daten);
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            addStandardprogrammeAboLinux(daten);
        } else {
            addStandardprogrammeAboWin(daten);
            addStandardprogrammeAboLinux(daten);
        }
    }

    private static void addStandardprogrammeAboWin(DDaten daten) {
        // Neustart und noch kein File, initialisieren
        DatenProg prog;
        DatenPgruppe gruppe;
        //Gruppe Windows
        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABO_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Programmgruppe Windows";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%T-%N.mp4";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        //Prog
        prog = new DatenProg();
        prog.arr[DatenProg.PROGRAMM_NAME_NR] = "flvstreamer";
        prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR] = "flvstreamer_win32_latest.exe";
        prog.arr[DatenProg.PROGRAMM_SCHALTER_NR] = "%f -o \"**\" --resume";
        prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = "-";
        prog.arr[DatenProg.PROGRAMM_SUFFIX_NR] = "";
        prog.arr[DatenProg.PROGRAMM_RESTART_NR] = Boolean.TRUE.toString();
        gruppe.addProg(prog);
        //Prog
        prog = new DatenProg();
        prog.arr[DatenProg.PROGRAMM_NAME_NR] = "vlc";
        prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR] = getWindowsVlcPath();
        prog.arr[DatenProg.PROGRAMM_SCHALTER_NR] = "%f :sout=#transcode{acodec=mpga,ab=128,channels=2}:duplicate{dst=std{access=file,mux=ts,dst=\"**\"}} -I \"dummy\" --play-and-exit";
        prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = "";
        prog.arr[DatenProg.PROGRAMM_SUFFIX_NR] = "";
        gruppe.addProg(prog);
        daten.listePgruppe.add(gruppe);
    }

    private static void addStandardprogrammeAboLinux(DDaten daten) {
        // Neustart und noch kein File, initialisieren
        DatenProg prog;
        DatenPgruppe gruppe;
        //Gruppe Linux
        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABO_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Programmgruppe Linux";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%T-%N";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        //Prog
        prog = new DatenProg();
        prog.arr[DatenProg.PROGRAMM_NAME_NR] = "flvstreamer";
        prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR] = "flvstreamer";
        prog.arr[DatenProg.PROGRAMM_SCHALTER_NR] = "%f -o ** --resume";
        prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = "-";
        prog.arr[DatenProg.PROGRAMM_SUFFIX_NR] = "";
        prog.arr[DatenProg.PROGRAMM_RESTART_NR] = Boolean.TRUE.toString();
        gruppe.addProg(prog);
        //Prog
        prog = new DatenProg();
        prog.arr[DatenProg.PROGRAMM_NAME_NR] = "Aufzeichnen Vlc";
        prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR] = "vlc";
        prog.arr[DatenProg.PROGRAMM_SCHALTER_NR] = "%f :sout=#transcode{acodec=mpga,ab=128,channels=2}:duplicate{dst=std{access=file,mux=ts,dst=**}} -I dummy --play-and-exit";
        prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = "mms";
        prog.arr[DatenProg.PROGRAMM_SUFFIX_NR] = "";
        gruppe.addProg(prog);
        //Prog
        prog = new DatenProg();
        prog.arr[DatenProg.PROGRAMM_NAME_NR] = "mplayer";
        prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR] = "mplayer";
        prog.arr[DatenProg.PROGRAMM_SCHALTER_NR] = "-cache 8192 -cache-min 50 -playlist %f -dumpstream -dumpfile **";
        prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = "";
        prog.arr[DatenProg.PROGRAMM_SUFFIX_NR] = "";
        gruppe.addProg(prog);
        daten.listePgruppe.add(gruppe);
    }

    public static void addStandardprogrammeButton(DDaten daten) {
        // Erster Start und noch nichts eingerichtet, initialisieren
        boolean is64bit = false;
        Log.systemMeldung(System.getProperty("os.name"));
//        Properties prop = System.getProperties();
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            addStandardprogrammeButtonWin(daten);
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            addStandardprogrammeButtonLinux(daten);
        } else if (System.getProperty("os.name").toLowerCase().contains("mac")) {
            addStandardprogrammeButtonMac(daten);
        } else {
            addStandardprogrammeButtonWin(daten);
            addStandardprogrammeButtonLinux(daten);
            addStandardprogrammeButtonMac(daten);
        }
    }

    private static void addStandardprogrammeButtonMac(DDaten daten) {
        // Erster Start und noch nichts eingerichtet, initialisieren
        DatenPgruppe gruppe;

        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABSPIELEN_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Abspielen Vlc";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = "";
        gruppe.addProg(new DatenProg("Mac - Abspielen Vlc", "/Applications/VLC.app/Contents/MacOS/VLC", "%f", Boolean.FALSE.toString()));
        daten.listePgruppe.add(gruppe);
    }

    private static void addStandardprogrammeButtonLinux(DDaten daten) {
        // Erster Start und noch nichts eingerichtet, initialisieren
        DatenPgruppe gruppe;

        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABSPIELEN_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Abspielen Vlc";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = "";
        gruppe.addProg(new DatenProg("Linux - Abspielen Vlc", "/usr/bin/vlc", "%f", Boolean.FALSE.toString()));
        daten.listePgruppe.add(gruppe);

        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Aufzeichnen mplayer";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%n";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        gruppe.addProg(new DatenProg("Linux - Aufzeichnen mplayer", "mplayer", "-cache 8192 -cache-min 50 -playlist %f -dumpstream -dumpfile **", Boolean.FALSE.toString()));
        daten.listePgruppe.add(gruppe);

        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Aufzeichnen mplayer mit Pfad";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%p";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        gruppe.addProg(new DatenProg("Linux - Aufzeichnen mplayer mit Pfad", "/usr/bin/mplayer", "-cache 8192 -cache-min 50 -playlist %f -dumpstream -dumpfile **", Boolean.FALSE.toString()));
        daten.listePgruppe.add(gruppe);

        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_SPEICHERN_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Aufzeichnen VLC mit Pfad";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%p";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        gruppe.addProg(new DatenProg("Linux - nicht transcodiertes Aufnehmen ohne Anschauen", "vlc",
                "%f :sout=#duplicate{dst=std{access=file,mux=asf,dst=**}} -I dummy --play-and-exit", Boolean.FALSE.toString()));
        daten.listePgruppe.add(gruppe);

        //flvstreamer
        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Aufzeichnen flvstreamer";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%n";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        gruppe.addProg(new DatenProg("flvstreamer", "flvstreamer", "%f -o ** --resume", Boolean.TRUE.toString()));
        daten.listePgruppe.add(gruppe);
////////////////////        daten.listePgruppe.setDoppelklick(0);
    }

    private static void addStandardprogrammeButtonWin(DDaten daten) {
        // Erster Start und noch nichts eingerichtet, initialisieren
        DatenPgruppe gruppe;

        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABSPIELEN_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Abspielen Vlc";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = "";
        gruppe.addProg(new DatenProg("Win - Abspielen Vlc", getWindowsVlcPath(), "%f", Boolean.FALSE.toString()));
        daten.listePgruppe.add(gruppe);

        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_SPEICHERN_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Aufzeichnen Vlc";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%p";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        gruppe.addProg(
                new DatenProg("Win - Aufzeichnen Vlc mit Pfad",
                getWindowsVlcPath(),
                "%f :sout=#transcode{acodec=mpga,ab=128,channels=2}:duplicate{dst=std{access=file,mux=ts,dst=\"**\"}} -I \"dummy\" --play-and-exit", Boolean.FALSE.toString()));
        daten.listePgruppe.add(gruppe);

        //flvstreamer
        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Aufzeichnen flvstreamer";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%p";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        gruppe.addProg(
                new DatenProg("flvstreamer",
                "flvstreamer_win32_latest.exe",
                "%f -o \"**\" --resume", Boolean.TRUE.toString()));
        daten.listePgruppe.add(gruppe);
/////////////////        daten.listePgruppe.setDoppelklick(0);
    }

    public static boolean programmePruefen(DDaten daten) {
        boolean ret = true;
        String text = "";
        Iterator<DatenPgruppe> itPgruppe = daten.listePgruppe.iterator();
        DatenPgruppe datenPgruppe;
        DatenProg datenProg;
        while (itPgruppe.hasNext()) {
            datenPgruppe = itPgruppe.next();
            ret = true;
            if (!datenPgruppe.isFreeLine() && !datenPgruppe.isLable()) {
                // nur wenn kein Lable oder freeline
                text += "++++++++++++++++++++++++++++++++++++++++++++" + "\n";
                text += PIPE + "Programmgruppe: " + datenPgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] + "\n";
                String zielPfad = datenPgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR];
                if (zielPfad.equals("")) {
                    // beim nur Abspielen wird er nicht gebraucht
                    if (datenPgruppe.needsPath()) {
                        ret = false;
                        text += PIPE + LEER + "Zielpfad fehlt!\n";
                    }
                } else {
                    File pfad = new File(zielPfad);
                    if (!pfad.isDirectory()) {
                        ret = false;
                        text += PIPE + LEER + "Falscher Zielpfad!\n";
                        text += PIPE + LEER + PFEIL + "Zielpfad \"" + zielPfad + "\" ist kein Verzeichnis!" + "\n";
                    } else {
                        // Pfad beschreibbar?
                        if (!checkPfadBeschreibbar(zielPfad)) {
                            //da Pfad-leer und "kein" Pfad schon abgeprüft
                            ret = false;
                            text += PIPE + LEER + "Falscher Zielpfad!\n";
                            text += PIPE + LEER + PFEIL + "Zielpfad \"" + zielPfad + "\" nicht beschreibbar!" + "\n";
                        }
                    }
                }
                Iterator<DatenProg> itProg = datenPgruppe.getListeProg().iterator();
                while (itProg.hasNext()) {
                    datenProg = itProg.next();
                    // Programmpfad prüfen
                    //if (!new File(datenProg.arr[Konstanten.PROGRAMM_PROGRAMMPFAD_NR]).exists()) {
                    if (!new File(datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR]).canExecute()) {
                        // Programme prüfen
                        ret = false;
                        text += PIPE + LEER + "Falscher Programmpfad!\n";
                        text += PIPE + LEER + PFEIL + "Programmname: " + datenProg.arr[DatenProg.PROGRAMM_NAME_NR] + "\n";
                        text += PIPE + LEER + LEER + "Pfad: " + datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR] + "\n";
                        if (!datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR].contains(File.separator)) {
                            text += PIPE + LEER + PFEIL + "Wenn das Programm nicht im Systempfad liegt, " + "\n";
                            text += PIPE + LEER + LEER + "wird der Start nicht klappen!" + "\n";
                        }
                    }
                }
                if (ret) {
                    //sollte alles passen
                    text += PIPE + PFEIL + "Ok!" + "\n";
                }
                text += "++++++++++++++++++++++++++++++++++++++++++++" + "\n\n\n";
            }
        }
        new DialogHilfe(null, true, text).setVisible(true);
        return ret;
    }

    public static boolean checkPfadBeschreibbar(String pfad) {
        boolean ret = false;
        File testPfad = new File(pfad);
        try {
            if (pfad.equals("")) {
            } else if (!testPfad.isDirectory()) {
            } else {
                if (testPfad.canWrite()) {
                    File tmpFile = File.createTempFile("mediathek", "tmp", testPfad);
                    tmpFile.delete();
                    ret = true;
                }
            }
        } catch (Exception ex) {
        }
        return ret;
    }

    private static String getWindowsVlcPath() {
        //Für Windows den Pfad des VLC ermitteln
        //sonst den deutschen Defaultpfad für Programme verwenden verwenden
        final String PFAD_WIN_VLC_DEFAULT = "C:\\Programme\\VideoLAN\\VLC\\vlc.exe";
        final String PFAD_WIN_VLC = "\\VideoLAN\\VLC\\vlc.exe";
        String vlcpath = "";
        try {
            if (System.getProperty("os.name").toLowerCase().contains("windows")) {
                if (System.getenv("ProgramFiles") != null) {
                    vlcpath = System.getenv("ProgramFiles") + PFAD_WIN_VLC;
                    if (new File(vlcpath).exists()) {
                        return vlcpath;
                    }
                }
            }
            if (System.getenv("ProgramFiles(x86)") != null) {
                vlcpath = System.getenv("ProgramFiles(x86)") + PFAD_WIN_VLC;
                if (new File(vlcpath).exists()) {
                    return vlcpath;
                }
            }
        } catch (Exception ex) {
        }
        return PFAD_WIN_VLC_DEFAULT;
    }

    public static boolean praefixTesten(String str, String uurl, boolean praefix) {
        //prüfen ob url beginnt/endet mit einem Argument in str
        //wenn str leer dann true
        boolean ret = false;
        String url = uurl.toLowerCase();
        String s1 = "";
        if (str.equals("")) {
            ret = true;
        } else {
            for (int i = 0; i < str.length(); ++i) {
                if (str.charAt(i) != ',') {
                    s1 += str.charAt(i);
                }
                if (str.charAt(i) == ',' || i >= str.length() - 1) {
                    if (praefix) {
                        //Präfix prüfen
                        if (url.startsWith(s1.toLowerCase())) {
                            ret = true;
                            break;
                        }
                    } else {
                        //Suffix prüfen
                        if (url.endsWith(s1.toLowerCase())) {
                            ret = true;
                            break;
                        }
                    }
                    s1 = "";
                }
            }
        }
        return ret;
    }
}
