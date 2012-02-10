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

import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.StringSelection;
import java.io.File;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.JFrame;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.daten.*;
import mediathek.gui.dialog.DialogHilfe;

public class GuiFunktionen {

    private static final String PIPE = "| ";
    private static final String LEER = "      ";
    private static final String PFEIL = " -> ";

    public static boolean setLook(JFrame frame) {
        int look;
        if (DDaten.system[Konstanten.SYSTEM_LOOK_NR].equals("")) {
            DDaten.system[Konstanten.SYSTEM_LOOK_NR] = "1";
        }
        look = Integer.parseInt(DDaten.system[Konstanten.SYSTEM_LOOK_NR]);
        if (look != 0) {
            return setLook(frame, look);
        }
        return true;
    }

    public static boolean setLook(JFrame frame, int look) {
        boolean ret = false;
        try {
            switch (look) {
                case 0:
                case 1:
                    //bei 0 egentlich nichts tun, wenn aber gewechselt, dann zurückschalten
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                    break;
                default:
                    UIManager.setLookAndFeel(GuiKonstanten.THEME[look][1]);
                    break;
            }
            SwingUtilities.updateComponentTreeUI(frame);
            for (Frame f : Frame.getFrames()) {
                SwingUtilities.updateComponentTreeUI(f);
                for (Window w : f.getOwnedWindows()) {
                    SwingUtilities.updateComponentTreeUI(w);
                }
            }
            ret = true;
        } catch (Exception e) {
            Log.fehlerMeldung("GuiFunktionen.setLook", e, "Kann das Look and Feel nicht ändern!");
        }
        return ret;
    }

    public static void copyToClipboard(String s) {
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(s), null);
    }

    public static void setProxy(DDaten daten) {
        if (Boolean.parseBoolean(DDaten.system[Konstanten.SYSTEM_PROXY_ON_NR])) {
            System.setProperty("proxySet", "true");
            System.setProperty("http.proxyHost", DDaten.system[Konstanten.SYSTEM_PROXY_SERVER_NR]);
            System.setProperty("http.proxyPort", DDaten.system[Konstanten.SYSTEM_PROXY_PORT_NR]);
            System.setProperty("http.proxyUser", DDaten.system[Konstanten.SYSTEM_PROXY_USER_NR]);
            System.setProperty("http.proxyPassword", DDaten.system[Konstanten.SYSTEM_PROXY_PWD_NR]);
        } else {
            System.setProperty("proxySet", "false");
        }
    }

    public static String replaceLeerDateiname(String pfad, boolean pfadtrennerEntfernen) {
        //verbotene Zeichen entfernen
        String ret = pfad;
        if (pfadtrennerEntfernen) {
            ret = ret.replace("/", "-");
            ret = ret.replace("\\", "-");
        }
        ret = ret.replace("\n", "_");
        ret = ret.replace("\"", "_");
        ret = ret.replace(",", "_");
        ret = ret.replace(";", "_");
        ret = ret.replace("(", "_");
        ret = ret.replace(")", "_");
        ret = ret.replace(" ", "_");
        ret = ret.replace("*", "_");
        ret = ret.replace("?", "_");
        ret = ret.replace("<", "_");
        ret = ret.replace(">", "_");
        ret = ret.replace(":", "_");
        ret = ret.replace("'", "_");
        return ret;
    }

    public static String addsPfad(String pfad1, String pfad2) {
        String ret = "";
        if (pfad1 != null && pfad2 != null) {
            if (!pfad1.equals("") && !pfad2.equals("")) {
                if (pfad1.charAt(pfad1.length() - 1) == File.separatorChar) {
                    ret = pfad1.substring(0, pfad1.length() - 1);
                } else {
                    ret = pfad1;
                }
                if (pfad2.charAt(0) == File.separatorChar) {
                    ret += pfad2;
                } else {
                    ret += File.separator + pfad2;
                }
            }
        }
        if (ret.equals("")) {
            Log.fehlerMeldung("GuiFunktionen.addsPfad", pfad1 + " - " + pfad2);
        }
        return ret;
    }

    public static String getDateiName(String pfad) {
        //Dateinamen einer URL extrahieren
        String ret = "";
        if (pfad != null) {
            if (!pfad.equals("")) {
                ret = pfad.substring(pfad.lastIndexOf("/") + 1);
            }
        }
        if (ret.contains("?")) {
            ret = ret.substring(0, ret.indexOf("?"));
        }
        if (ret.contains("&")) {
            ret = ret.substring(0, ret.indexOf("&"));
        }
        if (ret.equals("")) {
            Log.fehlerMeldung("GuiFunktionen.getDateiName", pfad);
        }
        return ret;
    }

    public static void listeSort(LinkedList<String> liste) {
        //Stringliste alphabetisch sortieren
        GermanStringSorter sorter = GermanStringSorter.getInstance();
        Collections.sort(liste, sorter);
    }

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
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = getHomePath();
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
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = getHomePath();
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
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        gruppe.addProg(new DatenProg("Linux - Aufzeichnen mplayer", "mplayer", "-cache 8192 -cache-min 50 -playlist %f -dumpstream -dumpfile **", Boolean.FALSE.toString()));
        daten.listePgruppe.add(gruppe);

        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Aufzeichnen mplayer mit Pfad";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%p";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        gruppe.addProg(new DatenProg("Linux - Aufzeichnen mplayer mit Pfad", "/usr/bin/mplayer", "-cache 8192 -cache-min 50 -playlist %f -dumpstream -dumpfile **", Boolean.FALSE.toString()));
        daten.listePgruppe.add(gruppe);

        gruppe = new DatenPgruppe();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_SPEICHERN_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(true);
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Aufzeichnen VLC mit Pfad";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = "%p";
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = getHomePath();
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
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = getHomePath();
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
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = getHomePath();
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
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = getHomePath();
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_PRAEFIX;
        gruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = GuiKonstanten.DIREKTE_DOWNLOAD_SUFFIX;
        gruppe.addProg(
                new DatenProg("flvstreamer",
                "flvstreamer_win32_latest.exe",
                "%f -o \"**\" --resume", Boolean.TRUE.toString()));
        daten.listePgruppe.add(gruppe);
/////////////////        daten.listePgruppe.setDoppelklick(0);
    }

    public static void addProgVorlagen(DDaten daten) {
        // Liste der Sandardprogramme
        // erst leeren
        daten.listeProgVorlagen.clear();
        // Windows
        daten.listeProgVorlagen.addProg("Win - Abspielen Vlc", getWindowsVlcPath(), "%f");
        daten.listeProgVorlagen.addProg("Win - Aufzeichnen Vlc",
                getWindowsVlcPath(),
                "%f :sout=#transcode{acodec=mpga,ab=128,channels=2}:duplicate{dst=std{access=file,mux=ts,dst=\"**\"}} -I \"dummy\" --play-and-exit");

        daten.listeProgVorlagen.addProg("Win - nicht transcodiertes Aufnehmen ohne Anschauen",
                getWindowsVlcPath(),
                "%f :sout=#duplicate{dst=std{access=file,mux=asf,dst=\"**\"}} -I \"dummy\" --play-and-exit");
        daten.listeProgVorlagen.addProg("Win - nicht transcodiertes Aufnehmen und Anschauen",
                getWindowsVlcPath(),
                "%f :sout=#duplicate{dst=std{access=file,mux=asf,dst=\"**\"},dst=display} --play-and-exit");
        daten.listeProgVorlagen.addProg("Win - Itunes lauffähiges Video",
                getWindowsVlcPath(),
                "%f :sout=#transcode{vcodec=mp4v,vb=1024,scale=1,width=480,height=272,acodec=mp4a,ab=128,channels=2,samplerate=48000}:duplicate{dst=std{access=file,mux=mp4,dst=\"**\"}} --play-and-exit");
        DatenProg prog = new DatenProg("Windows - Aufzeichnen flvstreamer", "flvstreamer_win32_latest.exe",
                "%f -o \"**\" --resume", Boolean.TRUE.toString());
        prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = "-";
        daten.listeProgVorlagen.add(prog);
        //
        //Linux
        daten.listeProgVorlagen.addProg("Linux - Abspielen Vlc", "/usr/bin/vlc", "%f");
        daten.listeProgVorlagen.addProg("Linux - Aufzeichnen mplayer", "/usr/bin/mplayer", "-cache 8192 -cache-min 50 -playlist %f -dumpstream -dumpfile **");
        daten.listeProgVorlagen.addProg("Linux - nicht transcodiertes Aufnehmen ohne Anschauen",
                "vlc",
                "%f :sout=#duplicate{dst=std{access=file,mux=asf,dst=**}} -I dummy --play-and-exit");
        prog = new DatenProg("Linux - Aufzeichnen flvstreamer", "/usr/bin/flvstreamer", "%f -o ** --resume", Boolean.TRUE.toString());
        prog.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = "-";
        daten.listeProgVorlagen.add(prog);

        //
        //Mac
        daten.listeProgVorlagen.addProg("Mac - Abspielen Vlc", "/Applications/VLC.app/Contents/MacOS/VLC", "%f");
        //
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

    public static String getHomePath() {
        //lifert den Pfad zum Homeverzeichnis
        return System.getProperty("user.home");
    }

    public static String[] addLeerListe(String[] str) {
        //ein Leerzeichen der Liste voranstellen
        int len = str.length + 1;
        String[] liste = new String[len];
        liste[0] = "";
        for (int i = 1; i < len; ++i) {
            liste[i] = str[i - 1];
        }
        return liste;
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

    public static void spaltenFilmLoeschen(JTable tabelle, boolean ziel, boolean zeit, boolean datei) {
        //abo anzeigen
        //ziel Downloadziel anzeigen
        //prog passendes Programm anzeigen
        for (int i = 0; i < tabelle.getColumnCount(); ++i) {
            if (!DDaten.debug) {
                if (!zeit && i == DatenFilm.FILM_ZEIT_NR
                        || i == DatenFilm.FILM_URL_ORG_NR
                        || i == DatenFilm.FILM_URL_RTMP_NR
                        || i == DatenFilm.FILM_URL_AUTH_NR
                        || i == DatenFilm.FILM_URL_THEMA_NR) {
                    tabelle.getColumnModel().getColumn(i).setMinWidth(0);
                    tabelle.getColumnModel().getColumn(i).setPreferredWidth(0);
                    tabelle.getColumnModel().getColumn(i).setMaxWidth(0);
                }
            }
        }
    }

    public static void spaltenFilmSetzen(JTable tabelle, boolean ziel) {
        //ziel Downloadziel anzeigen
        //prog passendes Programm anzeigen
        for (int i = 0; i < tabelle.getColumnCount(); ++i) {
            tabelle.getColumnModel().getColumn(i).setMinWidth(10);
            tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
            tabelle.getColumnModel().getColumn(i).setPreferredWidth(200);
            if (i == DatenFilm.FILM_NR_NR
                    || i == DatenFilm.FILM_DATUM_NR
                    || i == DatenFilm.FILM_ZEIT_NR
                    || i == DatenFilm.FILM_SENDER_NR) {
                tabelle.getColumnModel().getColumn(i).setMinWidth(10);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(100);
            } else if (i == DatenFilm.FILM_TITEL_NR) {
                tabelle.getColumnModel().getColumn(i).setMinWidth(10);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(300);
            } else if (i == DatenFilm.FILM_URL_NR) {
                tabelle.getColumnModel().getColumn(i).setMinWidth(10);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(500);
            }
        }
        spaltenFilmLoeschen(tabelle, ziel, true, true);
    }

    public static void spaltenAboSetzen(JTable tabelle) {
        for (int i = 0; i < tabelle.getColumnCount(); ++i) {
            tabelle.getColumnModel().getColumn(i).setMinWidth(10);
            tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
            if (i == DatenAbo.ABO_NR_NR) {
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(50);
            } else if (i == DatenAbo.ABO_THEMA_EXAKT_NR
                    || i == DatenAbo.ABO_EINGESCHALTET_NR
                    || i == DatenAbo.ABO_DOWN_DATUM_NR
                    || i == DatenAbo.ABO_SENDER_NR) {
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(100);
            } else {
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(200);
            }
        }
    }

    public static void spaltenDownloadSetzen(JTable tabelle) {
        for (int i = 0; i < tabelle.getColumnCount(); ++i) {
            if (i == DatenDownload.DOWNLOAD_URL_AUTH_NR
                    || i == DatenDownload.DOWNLOAD_URL_RTMP_NR
                    || i == DatenDownload.DOWNLOAD_ART_NR
                    || i == DatenDownload.DOWNLOAD_QUELLE_NR) {
                tabelle.getColumnModel().getColumn(i).setMinWidth(0);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(0);
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(0);
            } else {
                tabelle.getColumnModel().getColumn(i).setMinWidth(10);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
                if (i == DatenDownload.DOWNLOAD_NR_NR) {
                    tabelle.getColumnModel().getColumn(i).setPreferredWidth(50);
                } else if (i == DatenDownload.DOWNLOAD_DATUM_NR
                        || i == DatenDownload.DOWNLOAD_SENDER_NR
                        || i == DatenDownload.DOWNLOAD_THEMA_NR
                        || i == DatenDownload.DOWNLOAD_ZEIT_NR
                        || i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                    tabelle.getColumnModel().getColumn(i).setPreferredWidth(100);
                } else {
                    tabelle.getColumnModel().getColumn(i).setPreferredWidth(200);
                }
            }
        }
    }

    public static String textLaenge(String text) {
        if (text.length() != 80) {
            return textLaenge(80, text);
        } else {
            return text;
        }
    }

    public static String textLaenge(int max, String text) {
        final int MAX = max;
        if (text.length() > MAX) {
            //text = text.substring(0, MAX);
            text = text.substring(0, MAX - 1);
        }
        while (text.length() < MAX) {
            text = text + " ";
        }
        return text;
    }

    public static int getImportArtFilme() {
        int ret;
        try {
            ret = Integer.parseInt(DDaten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR]);
        } catch (Exception ex) {
            ret = GuiKonstanten.UPDATE_FILME_URL;
        }
        return ret;
    }

    public static void checkFlash(DDaten daten, String url, String befehlsString) {
        if (!System.getProperty("os.name").toLowerCase().contains("linux")) {
            // bei Linux klappts inzwischen meist auch mit anderen Playern
            if (url.startsWith("-r") || url.startsWith("rtmp") || url.startsWith("--host")) {
                if (!(befehlsString.contains("flvstreamer") || befehlsString.contains("rtmpdump"))) {
                    new Hinweis(daten).hinweisFlash();
                }
            }
        }
    }
}
