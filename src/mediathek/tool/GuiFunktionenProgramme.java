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

import mediathek.config.MVConfig;
import java.awt.Cursor;
import java.io.*;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import mSearch.Const;
import static mSearch.tool.Functions.getOs;
import mediathek.controller.starter.RuntimeExec;
import mediathek.config.Daten;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.dialogEinstellungen.DialogImportPset;

public class GuiFunktionenProgramme extends GuiFunktionen {

    private static final ArrayList<String> winPfade = new ArrayList<>();

    private static void setWinProgPfade() {
        String pfad;
        if (System.getenv("ProgramFiles") != null) {
            pfad = System.getenv("ProgramFiles");
            if (new File(pfad).exists() && !winPfade.contains(pfad)) {
                winPfade.add(pfad);
            }
        }
        if (System.getenv("ProgramFiles(x86)") != null) {
            pfad = System.getenv("ProgramFiles(x86)");
            if (new File(pfad).exists() && !winPfade.contains(pfad)) {
                winPfade.add(pfad);
            }
        }
        String[] PFAD = {"C:\\Program Files", "C:\\Programme", "C:\\Program Files (x86)"};
        for (String s : PFAD) {
            if (new File(s).exists() && !winPfade.contains(s)) {
                winPfade.add(s);
            }
        }
    }

    public static String getMusterPfadVlc() {
        // liefert den Standardpfad für das entsprechende BS 
        // Programm muss auf dem Rechner instelliert sein
        final String PFAD_LINUX_VLC = "/usr/bin/vlc";
        final String PFAD_FREEBSD = "/usr/local/bin/vlc";
        final String PFAD_MAC_VLC = "/Applications/VLC.app/Contents/MacOS/VLC";
        final String PFAD_WIN = "\\VideoLAN\\VLC\\vlc.exe";
        String pfad = "";
        switch (getOs()) {
            case LINUX:
                if (System.getProperty("os.name").toLowerCase().contains("freebsd")) {
                    pfad = PFAD_FREEBSD;
                } else {
                    pfad = PFAD_LINUX_VLC;
                }
                break;
            case MAC:
                pfad = PFAD_MAC_VLC;
                break;
            default:
                setWinProgPfade();
                for (String s : winPfade) {
                    pfad = s + PFAD_WIN;
                    if (new File(pfad).exists()) {
                        break;
                    }
                }
        }
        if (!new File(pfad).exists() && System.getenv("PATH_VLC") != null) {
            pfad = System.getenv("PATH_VLC");
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
        final String PFAD_FREEBSD = "/usr/local/bin/flvstreamer";
        final String PFAD_MAC_FLV = "bin/flvstreamer_macosx_intel_32bit_latest";
        final String PFAD_WINDOWS_FLV = "bin\\flvstreamer_win32_latest.exe";
        String pfad;
        switch (getOs()) {
            case LINUX:
                if (System.getProperty("os.name").toLowerCase().contains("freebsd")) {
                    pfad = PFAD_FREEBSD;
                } else {
                    pfad = PFAD_LINUX_FLV;
                }
                break;
            case MAC:
                pfad = MVFunctionSys.getPathJar() + PFAD_MAC_FLV;
                break;
            default:
                pfad = PFAD_WINDOWS_FLV;
        }
        if (!new File(pfad).exists() && System.getenv("PATH_FLVSTREAMER") != null) {
            pfad = System.getenv("PATH_FLVSTREAMER");
        }
        if (!new File(pfad).exists()) {
            pfad = "";
        }
        return pfad;
    }

    public static String getMusterPfadFFmpeg() {
        // liefert den Standardpfad für das entsprechende BS 
        // bei Win+Mac wird das Programm mitgeliefert und liegt 
        // im Ordner "bin" der mit dem Programm mitgeliefert wird
        // bei Linux muss das Programm auf dem Rechner installiert sein
        final String PFAD_LINUX_FFMPEG = "/usr/bin/ffmpeg";
        final String PFAD_FREEBSD_FFMPEG = "/usr/local/bin/ffmpeg";
        final String PFAD_MAC_FFMPEG = "bin/ffmpeg";
        final String PFAD_WINDOWS_FFMPEG = "bin\\ffmpeg.exe";
        String pfad;
        switch (getOs()) {
            case LINUX:
                if (System.getProperty("os.name").toLowerCase().contains("freebsd")) {
                    pfad = PFAD_FREEBSD_FFMPEG;
                } else {
                    pfad = PFAD_LINUX_FFMPEG;
                }
                break;
            case MAC:
                pfad = MVFunctionSys.getPathJar() + PFAD_MAC_FFMPEG;
                break;
            default:
                pfad = PFAD_WINDOWS_FFMPEG;
        }
        if (!new File(pfad).exists() && System.getenv("PATH_FFMPEG") != null) {
            pfad = System.getenv("PATH_FFMPEG");
        }
        if (!new File(pfad).exists()) {
            pfad = "";
        }
        return pfad;
    }

    public static String getPfadScript() {
        // liefert den Standardpfad zum Script "Ansehen" für das entsprechende BS 
        // liegt im Ordner "bin" der mit dem Programm mitgeliefert wird
        String pfadScript;
        final String PFAD_LINUX_SCRIPT = "bin/flv.sh";
        final String PFAD_WINDOWS_SCRIPT = "bin\\flv.bat";
        switch (getOs()) {
            case LINUX:
                pfadScript = getPathJar() + PFAD_LINUX_SCRIPT;
                break;
            case MAC:
                pfadScript = getPathJar() + PFAD_LINUX_SCRIPT;
                break;
            default:
                pfadScript = PFAD_WINDOWS_SCRIPT;
        }
        return pfadScript;
    }

    public static void addSetVorlagen(JFrame parent, Daten daten, ListePset pSet, boolean auto, boolean setVersion) {
        if (pSet == null) {
            if (!auto) {
                MVMessageDialog.showMessageDialog(null, "Die Datei wurde nicht importiert!",
                        "Fehler", JOptionPane.ERROR_MESSAGE);
            }
            return;
        }
        if (parent != null) {
            parent.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        }
        for (DatenPset ps : pSet) {
            if (!ps.arr[DatenPset.PROGRAMMSET_ADD_ON].equals("")) {
                if (!addOnZip(ps.arr[DatenPset.PROGRAMMSET_ADD_ON])) {
                    // und Tschüss
                    if (!auto) {
                        MVMessageDialog.showMessageDialog(null, "Die Datei wurde nicht importiert!",
                                "Fehler", JOptionPane.ERROR_MESSAGE);
                    }
                    return;
                }
            }
        }
        if (parent != null) {
            parent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
        if (auto) {
            if (Daten.listePset.addPset(pSet)) {
                if (setVersion) {
                    MVConfig.add(MVConfig.SYSTEM_VERSION_PROGRAMMSET, pSet.version);
                }
            }
        } else {
            DialogImportPset dialog = new DialogImportPset(parent, true, daten, pSet);
            dialog.setVisible(true);
            if (dialog.ok) {
                if (Daten.listePset.addPset(pSet)) {
                    if (setVersion) {
                        MVConfig.add(MVConfig.SYSTEM_VERSION_PROGRAMMSET, pSet.version);
                    }
                    MVMessageDialog.showMessageDialog(null, pSet.size() + " Programmset importiert!",
                            "Ok", JOptionPane.INFORMATION_MESSAGE);

                } else {
                    MVMessageDialog.showMessageDialog(null, "Die Datei wurde nicht importiert!",
                            "Fehler", JOptionPane.ERROR_MESSAGE);

                }
            }
        }
    }

    private static boolean addOnZip(String datei) {
        String zielPfad = addsPfad(getPathJar(), "bin");
        File zipFile;
        int timeout = 10_000; //10 Sekunden
        int n;
        URLConnection conn;
        try {
            if (!GuiFunktionen.istUrl(datei)) {
                zipFile = new File(datei);
                if (!zipFile.exists()) {
                    // und Tschüss
                    return false;
                }
                if (datei.endsWith(Const.FORMAT_ZIP)) {
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
                if (datei.endsWith(Const.FORMAT_ZIP)) {

                    File tmpFile = File.createTempFile("mediathek", null);
                    tmpFile.deleteOnExit();
                    try (BufferedInputStream in = new BufferedInputStream(conn.getInputStream())) {
                        FileOutputStream fOut = new FileOutputStream(tmpFile);
                        final byte[] buffer = new byte[1024];
                        while ((n = in.read(buffer)) != -1) {
                            fOut.write(buffer, 0, n);
                        }
                        fOut.close();
                    }
                    if (!entpacken(tmpFile, new File(zielPfad))) {
                        // und Tschüss
                        return false;
                    }

                } else {
                    String file = GuiFunktionen.getDateiName(datei);
                    File f = new File(GuiFunktionen.addsPfad(zielPfad, file));
                    try (BufferedInputStream in = new BufferedInputStream(conn.getInputStream())) {
                        FileOutputStream fOut = new FileOutputStream(f);
                        final byte[] buffer = new byte[1024];
                        while ((n = in.read(buffer)) != -1) {
                            fOut.write(buffer, 0, n);
                        }
                        fOut.close();
                    }
                }
            }
        } catch (Exception ignored) {
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
                BufferedInputStream bis;
                try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(new File(destDir, entryFileName)))) {
                    bis = new BufferedInputStream(zipFile
                            .getInputStream(entry));
                    while ((len = bis.read(buffer)) > 0) {
                        bos.write(buffer, 0, len);
                    }
                    bos.flush();
                }
                bis.close();
            }
        }
        zipFile.close();
        return true;
    }

    private static File buildDirectoryHierarchyFor(String entryName, File destDir) {
        int lastIndex = entryName.lastIndexOf('/');
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
        } catch (Exception ignored) {
        }
        return ret;
    }

    public static boolean programmePruefen(JFrame jFrame, Daten daten) {
        // prüfen ob die eingestellten Programmsets passen
        final String PIPE = "| ";
        final String LEER = "      ";
        final String PFEIL = " -> ";
        boolean ret = true;
        String text = "";

        for (DatenPset datenPset : Daten.listePset) {
            ret = true;
            if (!datenPset.isFreeLine() && !datenPset.isLable()) {
                // nur wenn kein Lable oder freeline
                text += "++++++++++++++++++++++++++++++++++++++++++++" + "\n";
                text += PIPE + "Programmgruppe: " + datenPset.arr[DatenPset.PROGRAMMSET_NAME] + "\n";
                String zielPfad = datenPset.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD];
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
                for (DatenProg datenProg : datenPset.getListeProg()) {
                    // Programmpfad prüfen
                    if (!new File(datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD]).canExecute()) {
                        // dann noch mit RuntimeExec versuchen
                        RuntimeExec r = new RuntimeExec(datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD]);
                        Process pr = r.exec(false /*log*/);
                        if (pr != null) {
                            // dann passts ja
                            pr.destroy();
                        } else {
                            // läßt sich nicht starten
                            ret = false;
                            text += PIPE + LEER + "Falscher Programmpfad!\n";
                            text += PIPE + LEER + PFEIL + "Programmname: " + datenProg.arr[DatenProg.PROGRAMM_NAME] + "\n";
                            text += PIPE + LEER + LEER + "Pfad: " + datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD] + "\n";
                            if (!datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].contains(File.separator)) {
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
        new DialogHilfe(jFrame, true, text).setVisible(true);
        return ret;
    }
}
