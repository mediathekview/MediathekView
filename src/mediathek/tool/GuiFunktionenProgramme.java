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
import java.io.InputStream;
import java.util.Iterator;
import javax.swing.JOptionPane;
import mediathek.controller.io.IoXmlLesen;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.dialog.DialogOk;
import mediathek.gui.dialogEinstellungen.DialogImportPset;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;

public class GuiFunktionenProgramme {

    public static final int OS_UNKNOWN = 0;
    public static final int OS_WIN_32BIT = 1;
    public static final int OS_WIN_64BIT = 2;
    public static final int OS_LINUX = 3;
    public static final int OS_MAC = 4;

    private static String getWindowsMplayerPath() {
        //Für Windows den Pfad des VLC ermitteln
        //sonst den deutschen Defaultpfad für Programme verwenden verwenden
        final String PFAD_WIN_DEFAULT = "C:\\Program Files\\SMPlayer\\mplayer\\mplayer.exe";
        final String PFAD_WIN = "\\SMPlayer\\mplayer\\mplayer.exe";
        String vlcPfad;
        try {
            if (System.getProperty("os.name").toLowerCase().contains("windows")) {
                if (System.getenv("ProgramFiles") != null) {
                    vlcPfad = System.getenv("ProgramFiles") + PFAD_WIN;
                    if (new File(vlcPfad).exists()) {
                        return vlcPfad;
                    }
                }
            }
            if (System.getenv("ProgramFiles(x86)") != null) {
                vlcPfad = System.getenv("ProgramFiles(x86)") + PFAD_WIN;
                if (new File(vlcPfad).exists()) {
                    return vlcPfad;
                }
            }
        } catch (Exception ex) {
        }
        return PFAD_WIN_DEFAULT;
    }

    private static String getWindowsVlcPath() {
        //Für Windows den Pfad des VLC ermitteln
        //sonst den deutschen Defaultpfad für Programme verwenden verwenden
        final String PFAD_WIN_DEFAULT = "C:\\Programme\\VideoLAN\\VLC\\vlc.exe";
        final String PFAD_WIN = "\\VideoLAN\\VLC\\vlc.exe";
        String vlcPfad;
        try {
            if (System.getProperty("os.name").toLowerCase().contains("windows")) {
                if (System.getenv("ProgramFiles") != null) {
                    vlcPfad = System.getenv("ProgramFiles") + PFAD_WIN;
                    if (new File(vlcPfad).exists()) {
                        return vlcPfad;
                    }
                }
            }
            if (System.getenv("ProgramFiles(x86)") != null) {
                vlcPfad = System.getenv("ProgramFiles(x86)") + PFAD_WIN;
                if (new File(vlcPfad).exists()) {
                    return vlcPfad;
                }
            }
        } catch (Exception ex) {
        }
        return PFAD_WIN_DEFAULT;
    }

    public static String getMusterPfadMplayer() {
        final String PFAD_LINUX = "/usr/bin/mplayer";
        final String PFAD_MAC = "/opt/local/bin/mplayer";
        String pfad = "";
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            pfad = getWindowsMplayerPath();
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            pfad = PFAD_LINUX;
        } else if (System.getProperty("os.name").toLowerCase().contains("mac")) {
            pfad = PFAD_MAC;
        }
        if (new File(pfad).exists()) {
            return pfad;
        } else {
            return "";
        }
    }

    public static String getMusterPfadVlc() {
        final String PFAD_LINUX_VLC = "/usr/bin/vlc";
        final String PFAD_MAC_VLC = "/Applications/VLC.app/Contents/MacOS/VLC";
        String pfad = "";
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            pfad = getWindowsVlcPath();
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            pfad = PFAD_LINUX_VLC;
        } else if (System.getProperty("os.name").toLowerCase().contains("mac")) {
            pfad = PFAD_MAC_VLC;
        }
        if (new File(pfad).exists()) {
            return pfad;
        } else {
            return "";
        }
    }

    public static String getMusterPfadFlv() {
        final String PFAD_LINUX_FLV = "/usr/bin/flvstreamer";
        final String PFAD_WINDOWS_FLV = "bin\\flvstreamer_win32_latest.exe";
        final String PFAD_MAC_FLV = "bin\\flvstreamer_macosx_intel_32bit_latest";
        String pfad;
        switch (getOs()) {
            case OS_LINUX:
                pfad = PFAD_LINUX_FLV;
                break;
            case OS_MAC:
                pfad = Funktionen.getPathJar() + PFAD_MAC_FLV;
                break;
            case OS_WIN_32BIT:
            case OS_WIN_64BIT:
            case OS_UNKNOWN:
            default:
                pfad = Funktionen.getPathJar() + PFAD_WINDOWS_FLV;
        }
        if (new File(pfad).exists()) {
            return pfad;
        } else {
            return "";
        }
    }

    public static String getPfadMplayer() {
        if (Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR].equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(false /* vlc */, false /* flvstreamer */, true /* mplayer */), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR];
    }

    public static String getPfadVlc() {
        if (Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR].equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(true /* vlc */, false /* flvstreamer */, false /* mplayer */), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR];
    }

    public static String getPfadFlv() {
        if (Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR].equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(false /* vlc */, true /* flvstreamer */, false /* mplayer */), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR];
    }

    public static int getOs() {
        int os = OS_UNKNOWN;
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            if (System.getenv("ProgramFiles") != null) {
                // win 32Bit
                os = OS_WIN_32BIT;
            } else if (System.getenv("ProgramFiles(x86)") != null) {
                // win 64Bit
                os = OS_WIN_64BIT;
            }
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            os = OS_LINUX;
        } else if (System.getProperty("os.name").toLowerCase().contains("mac")) {
            os = OS_MAC;
        }
        return os;
    }

    public static String getPfadScript() {
        String pfadScript;
        final String PFAD_LINUX_SCRIPT = "bin/flv.sh";
        final String PFAD_WINDOWS_SCRIPT = "bin\\flv.bat";
        switch (getOs()) {
            case OS_LINUX:
                pfadScript = Funktionen.getPathJar() + PFAD_LINUX_SCRIPT;
                break;
            case OS_MAC:
                pfadScript = Funktionen.getPathJar() + PFAD_LINUX_SCRIPT;
                break;
            case OS_WIN_32BIT:
            case OS_WIN_64BIT:
            case OS_UNKNOWN:
            default:
                pfadScript = Funktionen.getPathJar() + PFAD_WINDOWS_SCRIPT;
        }
        return pfadScript;
    }

    public static ListePset getStandardprogramme(DDaten ddaten) {
        ListePset pSet;
        InputStream datei;
        switch (getOs()) {
            case OS_LINUX:
                datei = new GetFile().getPsetVorlageLinux();
                break;
            case OS_MAC:
                datei = new GetFile().getPsetVorlageMac();
                break;
            case OS_WIN_32BIT:
            case OS_WIN_64BIT:
            case OS_UNKNOWN:
            default:
                datei = new GetFile().getPsetVorlageWindows();
        }
        // Standardgruppen laden
        pSet = IoXmlLesen.importPset(datei, true);
        return pSet;
    }

    public static boolean addVorlagen(DDaten ddaten, ListePset pSet, boolean auto) {
        // Standardgruppen laden
        if (pSet != null) {
            if (!auto) {
                DialogImportPset dialog = new DialogImportPset(null, true, ddaten, pSet);
                dialog.setVisible(true);
                if (!dialog.ok) {
                    return false;
                }
            }
            if (ddaten.listePset.addPset(pSet)) {
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_PSET, GuiFunktionenProgramme.class.getSimpleName());
                JOptionPane.showMessageDialog(null, pSet.size() + " Programmset importiert!",
                        "Ok", JOptionPane.INFORMATION_MESSAGE);
            }
            return true;
        } else {
            JOptionPane.showMessageDialog(null, "Die Datei wurde nicht importiert!",
                    "Fehler", JOptionPane.ERROR_MESSAGE);
            return false;
        }
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

    public static boolean checkPfadBeschreibbar(String pfad) {
        boolean ret = false;
        File testPfad = new File(pfad);
        try {
            if (!testPfad.exists()) {
                testPfad.mkdirs();
            }
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

    public static boolean programmePruefen(DDaten daten) {
        final String PIPE = "| ";
        final String LEER = "      ";
        final String PFEIL = " -> ";
        boolean ret = true;
        String text = "";
        Iterator<DatenPset> itPset = daten.listePset.iterator();
        DatenPset datenPset;
        DatenProg datenProg;
        while (itPset.hasNext()) {
            datenPset = itPset.next();
            ret = true;
            if (!datenPset.isFreeLine() && !datenPset.isLable()) {
                // nur wenn kein Lable oder freeline
                text += "++++++++++++++++++++++++++++++++++++++++++++" + "\n";
                text += PIPE + "Programmgruppe: " + datenPset.arr[DatenPset.PROGRAMMSET_NAME_NR] + "\n";
                String zielPfad = datenPset.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR];
                if (datenPset.progsContainPath()) {
                    // beim nur Abspielen wird er nicht gebraucht
                    if (zielPfad.equals("")) {
                        ret = false;
                        text += PIPE + LEER + "Zielpfad fehlt!\n";
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
                Iterator<DatenProg> itProg = datenPset.getListeProg().iterator();
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
}
