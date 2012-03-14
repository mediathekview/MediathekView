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
import java.security.CodeSource;
import java.util.Iterator;
import javax.swing.JOptionPane;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Main;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.controller.io.IoXmlLesen;
import mediathek.daten.DDaten;
import mediathek.daten.DatenPset;
import mediathek.daten.DatenProg;
import mediathek.daten.ListePset;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.DialogImportPset;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;
import mediathek.gui.dialogEinstellungen.PanelPsetImport;

public class GuiFunktionenProgramme {

    public static String getPathJar() {
        String pFilePath = "pFile";
        File propFile = new File(pFilePath);
        if (!propFile.exists()) {
            try {
                CodeSource cS = Main.class.getProtectionDomain().getCodeSource();
                File jarFile = new File(cS.getLocation().toURI().getPath());
                String jarDir = jarFile.getParentFile().getPath();
                propFile = new File(jarDir + File.separator + pFilePath);
            } catch (Exception ex) {
            }
        }
        return propFile.getAbsolutePath().replace(pFilePath, "");
    }

    private static String getWindowsVlcPath() {
        //Für Windows den Pfad des VLC ermitteln
        //sonst den deutschen Defaultpfad für Programme verwenden verwenden
        final String PFAD_WIN_VLC_DEFAULT = "C:\\Programme\\VideoLAN\\VLC\\vlc.exe";
        final String PFAD_WIN_VLC = "\\VideoLAN\\VLC\\vlc.exe";
        String vlcPfad = "";
        try {
            if (System.getProperty("os.name").toLowerCase().contains("windows")) {
                if (System.getenv("ProgramFiles") != null) {
                    vlcPfad = System.getenv("ProgramFiles") + PFAD_WIN_VLC;
                    if (new File(vlcPfad).exists()) {
                        return vlcPfad;
                    }
                }
            }
            if (System.getenv("ProgramFiles(x86)") != null) {
                vlcPfad = System.getenv("ProgramFiles(x86)") + PFAD_WIN_VLC;
                if (new File(vlcPfad).exists()) {
                    return vlcPfad;
                }
            }
        } catch (Exception ex) {
        }
        return PFAD_WIN_VLC_DEFAULT;
    }

    public static String getPfadVlc() {
        ///////////////////////
        if (!Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR].equals("")) {
            return Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR];
        }
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

    public static String getPfadFlv() {
        /////////////////////
        if (!Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR].equals("")) {
            return Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR];
        }
        final String PFAD_LINUX_FLV = "/usr/bin/flvstreamer";
        final String PFAD_WINDOWS_FLV = "bin\\flvstreamer_win32_latest.exe";
        String pfad = "";
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            pfad = GuiFunktionenProgramme.getPathJar() + PFAD_WINDOWS_FLV;
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            pfad = PFAD_LINUX_FLV;
        }
        if (new File(pfad).exists()) {
            return pfad;
        } else {
            return "";
        }
    }

    public static String getPfadScript() {
        String pfadScript;
        final String PFAD_LINUX_SCRIPT = "bin/flv.sh";
        final String PFAD_WINDOWS_SCRIPT = "bin\\flv.bat";
        if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            pfadScript = GuiFunktionenProgramme.getPathJar() + PFAD_LINUX_SCRIPT;
        } else if (System.getProperty("os.name").toLowerCase().contains("mac")) {
            pfadScript = GuiFunktionenProgramme.getPathJar() + PFAD_LINUX_SCRIPT;
        } else {
            pfadScript = GuiFunktionenProgramme.getPathJar() + PFAD_WINDOWS_SCRIPT;
        }
        return pfadScript;
    }

    public static boolean addStandardprogramme(DDaten ddaten, boolean auto) {
        ListePset pSet;
        InputStream datei;
        if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            datei = new GetFile().getPsetVorlageLinux();
        } else if (System.getProperty("os.name").toLowerCase().contains("mac")) {
            datei = new GetFile().getPsetVorlageMac();
        } else {
            datei = new GetFile().getPsetVorlageWindows();
        }
        // Standardgruppen laden
        pSet = IoXmlLesen.importPset(datei, true);
        return addVorlagen(ddaten, pSet, auto);
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
                Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_PSET, PanelPsetImport.class.getSimpleName());
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
                if (zielPfad.equals("")) {
                    // beim nur Abspielen wird er nicht gebraucht
                    if (datenPset.needsPath()) {
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
