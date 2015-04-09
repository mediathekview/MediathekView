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
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import mediathek.controller.starter.RuntimeExec;
import mediathek.daten.Daten;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.dialog.DialogOk;
import mediathek.gui.dialogEinstellungen.DialogImportPset;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;
import msearch.tool.MSConst;

public class GuiFunktionenProgramme extends GuiFunktionen {

    private static ArrayList<String> winPfade = new ArrayList<>();

    private static void setWinProgPade() {
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

    public static String getMusterPfadMplayer() {
        // liefert den Standardpfad für das entsprechende BS 
        // Programm muss auf dem Rechner instelliert sein
        final String PFAD_LINUX = "/usr/bin/mplayer";
        final String PFAD_MAC = "/opt/local/bin/mplayer";
        final String PFAD_WIN = "\\SMPlayer\\mplayer\\mplayer.exe";
        String pfad = "";
        switch (getOs()) {
            case LINUX:
                pfad = PFAD_LINUX;
                break;
            case MAC:
                pfad = PFAD_MAC;
                break;
            default:
                setWinProgPade();
                for (String s : winPfade) {
                    pfad = s + PFAD_WIN;
                    if (new File(pfad).exists()) {
                        break;
                    }
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
        final String PFAD_WIN = "\\VideoLAN\\VLC\\vlc.exe";
        String pfad = "";
        switch (getOs()) {
            case LINUX:
                pfad = PFAD_LINUX_VLC;
                break;
            case MAC:
                pfad = PFAD_MAC_VLC;
                break;
            default:
                setWinProgPade();
                for (String s : winPfade) {
                    pfad = s + PFAD_WIN;
                    if (new File(pfad).exists()) {
                        break;
                    }
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
            case LINUX:
                pfad = PFAD_LINUX_FLV;
                if (!new File(pfad).exists()) {
                    pfad = "";
                }
                break;
            case MAC:
                pfad = Funktionen.getPathJar() + PFAD_MAC_FLV;
                break;
            default:
                pfad = PFAD_WINDOWS_FLV;
        }
        return pfad;
    }

    public static String getMusterPfadFFmpeg() {
        // liefert den Standardpfad für das entsprechende BS 
        // bei Win+Mac wird das Programm mitgeliefert und liegt 
        // im Ordner "bin" der mit dem Programm mitgeliefert wird
        // bei Linux muss das Programm auf dem Rechner installiert sein
        final String PFAD_LINUX_FFMPEG = "/usr/bin/ffmpeg";
        final String PFAD_MAC_FFMPEG = "bin/ffmpeg";
        final String PFAD_WINDOWS_FFMPEG = "bin\\ffmpeg.exe";
        String pfad;
        switch (getOs()) {
            case LINUX:
                pfad = PFAD_LINUX_FFMPEG;
                if (!new File(pfad).exists()) {
                    pfad = "";
                }
                break;
            case MAC:
                pfad = Funktionen.getPathJar() + PFAD_MAC_FFMPEG;
                break;
            default:
                pfad = PFAD_WINDOWS_FFMPEG;
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

    public static String getPfadMplayer(JFrame parent) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (System.getenv("PATH_MPLAYER") != null) {
            return System.getenv("PATH_MPLAYER");
        }
        if (Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_MPLAYER).equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(parent, false /* vlc */, false /* flvstreamer */, true /* mplayer */, false/*ffmpeg*/), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_MPLAYER);
    }

    public static String getPfadVlc(JFrame parent) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (System.getenv("PATH_VLC") != null) {
            return System.getenv("PATH_VLC");
        }
        if (Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_VLC).equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(parent, true /* vlc */, false /* flvstreamer */, false /* mplayer */, false/*ffmpeg*/), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_VLC);
    }

    public static String getPfadFlv(JFrame parent) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (System.getenv("PATH_FLVSTREAMER") != null) {
            return System.getenv("PATH_FLVSTREAMER");
        }
        if (Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_FLVSTREAMER).equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(parent, false /* vlc */, true /* flvstreamer */, false /* mplayer */, false/*ffmpeg*/), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_FLVSTREAMER);
    }

    public static String getPfadFFmpeg(JFrame parent) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (System.getenv("PATH_FFMPEG") != null) {
            return System.getenv("PATH_FFMPEG");
        }
        if (Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_FFMPEG).equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(parent, false /* vlc */, false /* flvstreamer */, false /* mplayer */, true /*ffmpeg*/), "Pfade Standardprogramme").setVisible(true);
        }
        return Daten.mVConfig.get(MVConfig.SYSTEM_PFAD_FFMPEG);
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
            if (!ps.arr[DatenPset.PROGRAMMSET_ADD_ON_NR].equals("")) {
                if (!addOnZip(ps.arr[DatenPset.PROGRAMMSET_ADD_ON_NR])) {
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
                    Daten.mVConfig.add(MVConfig.SYSTEM_VERSION_PROGRAMMSET, pSet.version);
                }
            }
        } else {
            DialogImportPset dialog = new DialogImportPset(parent, true, daten, pSet);
            dialog.setVisible(true);
            if (dialog.ok) {
                if (Daten.listePset.addPset(pSet)) {
                    if (setVersion) {
                        Daten.mVConfig.add(MVConfig.SYSTEM_VERSION_PROGRAMMSET, pSet.version);
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
                if (datei.endsWith(MSConst.FORMAT_ZIP)) {
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
                if (datei.endsWith(MSConst.FORMAT_ZIP)) {

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
        Iterator<DatenPset> itPset = Daten.listePset.iterator();
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
        new DialogHilfe(jFrame, true, text).setVisible(true);
        return ret;
    }
}
