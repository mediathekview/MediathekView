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

import java.awt.Cursor;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.swing.JOptionPane;
import mediathek.controller.io.starter.RuntimeExec;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.dialog.DialogOk;
import mediathek.gui.dialogEinstellungen.DialogImportPset;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;

public class GuiFunktionenProgramme extends GuiFunktionen {

    public static String getMusterPfadMplayer() {
        // liefert den Standardpfad für das entsprechende BS 
        // Programm muss auf dem Rechner instelliert sein
        final String PFAD_LINUX = "/usr/bin/mplayer";
        final String PFAD_MAC = "/opt/local/bin/mplayer";
        final String PFAD_WIN_DEFAULT = "C:\\Program Files\\SMPlayer\\mplayer\\mplayer.exe";
        final String PFAD_WIN = "\\SMPlayer\\mplayer\\mplayer.exe";
        String pfad;
        switch (getOs()) {
            case OS_LINUX:
                pfad = PFAD_LINUX;
                break;
            case OS_MAC:
                pfad = PFAD_MAC;
                break;
            case OS_WIN_32BIT:
            case OS_WIN_64BIT:
            case OS_UNKNOWN:
            default:
                if (System.getenv("ProgramFiles") != null) {
                    pfad = System.getenv("ProgramFiles") + PFAD_WIN;
                } else if (System.getenv("ProgramFiles(x86)") != null) {
                    pfad = System.getenv("ProgramFiles(x86)") + PFAD_WIN;
                } else {
                    pfad = PFAD_WIN_DEFAULT;
                }
        }
        if (!new File(pfad).exists()) {
            pfad = "";
        }
        return pfad;
    }

    public static String getMusterPfadVlc() {
        // liefert den Standardpfad für das entsprechende BS 
        // Programm muss auf dem Rechner instelliert sein
        final String PFAD_LINUX_VLC = "/usr/bin/vlc";
        final String PFAD_MAC_VLC = "/Applications/VLC.app/Contents/MacOS/VLC";
        final String PFAD_WIN_DEFAULT = "C:\\Programme\\VideoLAN\\VLC\\vlc.exe";
        final String PFAD_WIN = "\\VideoLAN\\VLC\\vlc.exe";
        String pfad;
        switch (getOs()) {
            case OS_LINUX:
                pfad = PFAD_LINUX_VLC;
                break;
            case OS_MAC:
                pfad = PFAD_MAC_VLC;
                break;
            case OS_WIN_32BIT:
            case OS_WIN_64BIT:
            case OS_UNKNOWN:
            default:
                pfad = PFAD_WIN_DEFAULT;
                if (new File(pfad).exists()) {
                    break;
                }
                if (System.getenv("ProgramFiles") != null) {
                    // für 32/64Bit Win und 32Bit VLC
                    // oder 64Bit Win und 64Bit VLC
                    pfad = System.getenv("ProgramFiles") + PFAD_WIN;
                }
                if (new File(pfad).exists()) {
                    break;
                }
                if (System.getenv("ProgramFiles(x86)") != null) {
                    pfad = System.getenv("ProgramFiles(x86)") + PFAD_WIN;
                }
        }
        if (!new File(pfad).exists()) {
            pfad = "";
        }
        return pfad;
    }

    public static String getMusterPfadFlv() {
        // liefert den Standardpfad für das entsprechende BS 
        // bei Win+Mac wird das Programm mitgeliefert und liegt 
        // im Ordner "bin" der mit dem Programm mitgeliefert wird
        // bei Linux muss das Programm auf dem Rechner instelliert sein
        final String PFAD_LINUX_FLV = "/usr/bin/flvstreamer";
        final String PFAD_MAC_FLV = "bin/flvstreamer_macosx_intel_32bit_latest";
        final String PFAD_WINDOWS_FLV = "bin\\flvstreamer_win32_latest.exe";
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

    public static String getPfadScript() {
        // liefert den Standardpfad zum Script "Ansehen" für das entsprechende BS 
        // liegt im Ordner "bin" der mit dem Programm mitgeliefert wird
        String pfadScript;
        final String PFAD_LINUX_SCRIPT = "bin/flv.sh";
        final String PFAD_WINDOWS_SCRIPT = "bin\\flv.bat";
        switch (getOs()) {
            case OS_LINUX:
                pfadScript = getPathJar() + PFAD_LINUX_SCRIPT;
                break;
            case OS_MAC:
                pfadScript = getPathJar() + PFAD_LINUX_SCRIPT;
                break;
            case OS_WIN_32BIT:
            case OS_WIN_64BIT:
            case OS_UNKNOWN:
            default:
                pfadScript = getPathJar() + PFAD_WINDOWS_SCRIPT;
        }
        return pfadScript;
    }

    public static String getPfadMplayer(DDaten dd) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR].equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(dd, false /* vlc */, false /* flvstreamer */, true /* mplayer */), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR];
    }

    public static String getPfadVlc(DDaten dd) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR].equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(dd, true /* vlc */, false /* flvstreamer */, false /* mplayer */), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR];
    }

    public static String getPfadFlv(DDaten dd) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR].equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(dd, false /* vlc */, true /* flvstreamer */, false /* mplayer */), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR];
    }

//    public static ListePset getStandardprogramme(DDaten ddaten) {
//        // liefert das Standard Programmset für das entsprechende BS
//        ListePset pSet;
//        InputStream datei;
//        switch (getOs()) {
//            case OS_LINUX:
//                datei = new GetFile().getPsetVorlageLinux();
//                break;
//            case OS_MAC:
//                datei = new GetFile().getPsetVorlageMac();
//                break;
//            case OS_WIN_32BIT:
//            case OS_WIN_64BIT:
//            case OS_UNKNOWN:
//            default:
//                datei = new GetFile().getPsetVorlageWindows();
//        }
//        // Standardgruppen laden
//        pSet = IoXmlLesen.importPset(ddaten, datei, true);
//        return pSet;
//    }
    public static boolean addVorlagen(PanelVorlage parent, DDaten ddaten, ListePset pSet, boolean auto) {
        // Standardgruppen laden
        if (pSet != null) {
            parent.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            for (DatenPset ps : pSet) {
                if (!ps.arr[DatenPset.PROGRAMMSET_ADD_ON_NR].equals("")) {
                    if (!addOnZip(ps.arr[DatenPset.PROGRAMMSET_ADD_ON_NR])) {
                        // und Tschüss
                        return false;
                    }
                }
            }
            parent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            if (!auto) {
                DialogImportPset dialog = new DialogImportPset(null, true, ddaten, pSet);
                dialog.setVisible(true);
                if (!dialog.ok) {
                    return false;
                }
            }
            if (ddaten.listePset.addPset(pSet)) {
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

    private static boolean addOnZip(String datei) {
        String zielPfad = addsPfad(getPathJar(), "bin");
        File zipFile;
        int timeout = 10000; //10 Sekunden
        int n;
        URLConnection conn;
        try {
            if (!GuiFunktionen.istUrl(datei)) {
                zipFile = new File(datei);
                if (!zipFile.exists()) {
                    // und Tschüss
                    return false;
                }
                if (datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
                    if (!entpacken(zipFile, new File(zielPfad))) {
                        // und Tschüss
                        return false;
                    }
                } else {
                    FileInputStream in = new FileInputStream(datei);
                    FileOutputStream fOut = new FileOutputStream(GuiFunktionen.addsPfad(zielPfad, datei));
                    final byte[] buffer = new byte[1024];
                    while ((n = in.read(buffer)) != -1) {
                        fOut.write(buffer, 0, n);
                    }
                    fOut.close();
                    in.close();
                }
            } else {
                conn = new URL(datei).openConnection();
                conn.setConnectTimeout(timeout);
                conn.setReadTimeout(timeout);
                conn.setRequestProperty("User-Agent", Daten.getUserAgent());
                if (datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {

                    File tmpFile = File.createTempFile("mediathek", null);
                    tmpFile.deleteOnExit();
                    BufferedInputStream in = new BufferedInputStream(conn.getInputStream());
                    FileOutputStream fOut = new FileOutputStream(tmpFile);
                    final byte[] buffer = new byte[1024];
                    while ((n = in.read(buffer)) != -1) {
                        fOut.write(buffer, 0, n);
                    }
                    fOut.close();
                    in.close();
                    if (!entpacken(tmpFile, new File(zielPfad))) {
                        // und Tschüss
                        return false;
                    }

                } else {
                    BufferedInputStream in = new BufferedInputStream(conn.getInputStream());
                    FileOutputStream fOut = new FileOutputStream(GuiFunktionen.addsPfad(zielPfad, datei));
                    final byte[] buffer = new byte[1024];
                    while ((n = in.read(buffer)) != -1) {
                        fOut.write(buffer, 0, n);
                    }
                    fOut.close();
                    in.close();
                }
            }
        } catch (Exception ex) {
        }
        return true;
    }

    private static boolean entpacken(File archive, File destDir) throws Exception {
        if (!destDir.exists()) {
            return false;
        }

        ZipFile zipFile = new ZipFile(archive);
        Enumeration entries = zipFile.entries();

        byte[] buffer = new byte[16384];
        int len;
        while (entries.hasMoreElements()) {
            ZipEntry entry = (ZipEntry) entries.nextElement();

            String entryFileName = entry.getName();

            File dir = buildDirectoryHierarchyFor(entryFileName, destDir);
            if (!dir.exists()) {
                dir.mkdirs();
            }

            if (!entry.isDirectory()) {
                BufferedOutputStream bos = new BufferedOutputStream(
                        new FileOutputStream(new File(destDir, entryFileName)));

                BufferedInputStream bis = new BufferedInputStream(zipFile
                        .getInputStream(entry));

                while ((len = bis.read(buffer)) > 0) {
                    bos.write(buffer, 0, len);
                }

                bos.flush();
                bos.close();
                bis.close();
            }
        }
        zipFile.close();
        return true;
    }

    private static File buildDirectoryHierarchyFor(String entryName, File destDir) {
        int lastIndex = entryName.lastIndexOf('/');
        String entryFileName = entryName.substring(lastIndex + 1);
        String internalPathToEntry = entryName.substring(0, lastIndex + 1);
        return new File(destDir, internalPathToEntry);
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
        // prüfen ob die eingestellten Programmsets passen
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
                    if (!new File(datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR]).canExecute()) {
                        // dann noch mit RuntimeExec versuchen
                        RuntimeExec r = new RuntimeExec(datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR]);
                        Process pr = r.exec();
                        if (pr != null) {
                            // dann passts ja
                            pr.destroy();
                        } else {
                            // läßt sich nicht starten
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
//    public static void vlcPfadSchreiben(DDaten dd) {
//        String befehl = GuiFunktionenProgramme.getPfadVlc(dd);
//        if (!befehl.equals("")) {
//            befehl = befehl + " --version";
//            Log.playerMeldung("");
//            Log.playerMeldung("====================================");
//            Log.playerMeldung("VLC-Version");
//            Log.playerMeldung("Befehl: " + befehl);
//            new RuntimeExec(befehl).exec();
//        }
//    }
}
