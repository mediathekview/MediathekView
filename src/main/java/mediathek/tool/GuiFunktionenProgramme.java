package mediathek.tool;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.RuntimeExec;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.dialogEinstellungen.DialogImportPset;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import static mediathek.tool.Functions.getOs;

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
        try {
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
        } catch (Exception ignore) {
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
        String pfad = "";
        try {
            switch (getOs()) {
                case LINUX:
                    if (System.getProperty("os.name").toLowerCase().contains("freebsd")) {
                        pfad = PFAD_FREEBSD_FFMPEG;
                    } else {
                        pfad = PFAD_LINUX_FFMPEG;
                    }
                    break;
                case MAC:
                    pfad = PFAD_MAC_FFMPEG;
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
        } catch (Exception ignore) {
        }
        return pfad;
    }

    public static void addSetVorlagen(JFrame parent, Daten daten, ListePset pSet, boolean setVersion) {
        if (pSet == null) {
            MVMessageDialog.showMessageDialog(null, "Die Datei wurde nicht importiert!",
                    "Fehler", JOptionPane.ERROR_MESSAGE);
            return;
        }
        if (parent != null) {
            parent.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        }
        for (DatenPset ps : pSet) {
            if (!ps.arr[DatenPset.PROGRAMMSET_ADD_ON].isEmpty()) {
                if (!addOnZip(ps.arr[DatenPset.PROGRAMMSET_ADD_ON])) {
                    // und Tschüss
                    MVMessageDialog.showMessageDialog(null, "Die Datei wurde nicht importiert!",
                            "Fehler", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
        }
        if (parent != null) {
            parent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }

        DialogImportPset dialog = new DialogImportPset(parent, true, daten, pSet);
        dialog.setVisible(true);
        if (dialog.ok) {
            if (Daten.listePset.addPset(pSet)) {
                if (setVersion) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_VERSION_PROGRAMMSET, pSet.version);
                }
                MVMessageDialog.showMessageDialog(null, pSet.size() + " Programmset importiert!",
                        "Ok", JOptionPane.INFORMATION_MESSAGE);

            } else {
                MVMessageDialog.showMessageDialog(null, "Die Datei wurde nicht importiert!",
                        "Fehler", JOptionPane.ERROR_MESSAGE);

            }
        }
    }

    private static boolean addOnZip(String datei) {
        String zielPfad = addsPfad(getPathToApplicationJar(), "bin");
        File zipFile;
        int n;

        try {
            if (!NetUtils.isUrl(datei)) {
                zipFile = new File(datei);
                if (!zipFile.exists()) {
                    // und Tschüss
                    return false;
                }
                if (datei.endsWith(Konstanten.FORMAT_ZIP)) {
                    if (!entpacken(zipFile, new File(zielPfad))) {
                        // und Tschüss
                        return false;
                    }
                } else {
                    try (FileInputStream in = new FileInputStream(datei);
                         FileOutputStream fOut = new FileOutputStream(GuiFunktionen.addsPfad(zielPfad, datei))) {
                        final byte[] buffer = new byte[64 * 1024];
                        while ((n = in.read(buffer)) != -1) {
                            fOut.write(buffer, 0, n);
                        }
                    }
                }
            } else {
                final Request request = new Request.Builder().url(datei).get()
                        .header("User-Agent", ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_USER_AGENT))
                        .get().build();
                try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
                     ResponseBody body = response.body()) {
                    if (response.isSuccessful() && body != null) {
                        try (InputStream is = body.byteStream();
                             BufferedInputStream bis = new BufferedInputStream(is)) {
                            final byte[] buffer = new byte[64 * 1024];
                            if (datei.endsWith(Konstanten.FORMAT_ZIP)) {
                                File tmpFile = File.createTempFile("mediathek", null);
                                tmpFile.deleteOnExit();
                                try (FileOutputStream fOut = new FileOutputStream(tmpFile)) {
                                    while ((n = bis.read(buffer)) != -1) {
                                        fOut.write(buffer, 0, n);
                                    }
                                }
                                if (!entpacken(tmpFile, new File(zielPfad))) {
                                    // und Tschüss
                                    return false;
                                }
                            } else {
                                String file = GuiFunktionen.getDateiName(datei);
                                File f = new File(GuiFunktionen.addsPfad(zielPfad, file));
                                try (FileOutputStream fOut = new FileOutputStream(f)) {
                                    while ((n = bis.read(buffer)) != -1) {
                                        fOut.write(buffer, 0, n);
                                    }
                                }
                            }
                        }
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


        try (ZipFile zipFile = new ZipFile(archive)) {
            Enumeration<? extends ZipEntry> entries = zipFile.entries();

            byte[] buffer = new byte[16384];
            int len;
            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();

                String entryFileName = entry.getName();

                File dir = buildDirectoryHierarchyFor(entryFileName, destDir);
                if (!dir.exists()) {
                    dir.mkdirs();
                }

                if (!entry.isDirectory()) {
                    try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(new File(destDir, entryFileName)));
                         BufferedInputStream bis = new BufferedInputStream(zipFile.getInputStream(entry))) {
                        while ((len = bis.read(buffer)) > 0) {
                            bos.write(buffer, 0, len);
                        }
                        bos.flush();
                    }
                }
            }
        }

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
        if (str.isEmpty()) {
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
                    } else //Suffix prüfen
                        if (url.endsWith(s1.toLowerCase())) {
                            ret = true;
                            break;
                        }
                    s1 = "";
                }
            }
        }
        return ret;
    }

    private static final Logger logger = LogManager.getLogger(GuiFunktionenProgramme.class);

    /**
     * Test if a path is a directory and writeable.
     * Path directories will be created before trying write test.
     *
     * @param path path to the directory
     * @return true if we can write a file there, false if not.
     */
    public static boolean checkPathWriteable(@NotNull String path) {
        boolean ret = false;

        if (path.isEmpty())
            return false;

        File testFile = new File(path);
        try {
            if (!testFile.exists()) {
                testFile.mkdirs();
            }

            if (testFile.isDirectory()) {
                if (testFile.canWrite()) {
                    File tmpFile = File.createTempFile("mediathek", "tmp", testFile);
                    ret = tmpFile.delete();
//                    ret = true;
                }
            }
        } catch (Exception e) {
            logger.error("checkPathWriteable()", e);
        }

        return ret;
    }


    public static boolean programmePruefen(JFrame jFrame) {
        // prüfen ob die eingestellten Programmsets passen
        final String PIPE = "| ";
        final String LEER = "      ";
        final String PFEIL = " -> ";
        boolean ret = true;
        String text = "";

        for (DatenPset datenPset : Daten.listePset) {
            ret = true;
            if (!datenPset.isFreeLine() && !datenPset.isLabel()) {
                // nur wenn kein Label oder freeline
                text += "++++++++++++++++++++++++++++++++++++++++++++" + '\n';
                text += PIPE + "Programmgruppe: " + datenPset.arr[DatenPset.PROGRAMMSET_NAME] + '\n';
                String zielPfad = datenPset.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD];
                if (datenPset.progsContainPath()) {
                    // beim nur Abspielen wird er nicht gebraucht
                    if (zielPfad.isEmpty()) {
                        ret = false;
                        text += PIPE + LEER + "Zielpfad fehlt!\n";
                    } else // Pfad beschreibbar?
                        if (!checkPathWriteable(zielPfad)) {
                            //da Pfad-leer und "kein" Pfad schon abgeprüft
                            ret = false;
                            text += PIPE + LEER + "Falscher Zielpfad!\n";
                            text += PIPE + LEER + PFEIL + "Zielpfad \"" + zielPfad + "\" nicht beschreibbar!" + '\n';
                        }
                }
                for (DatenProg datenProg : datenPset.getListeProg()) {
                    // Programmpfad prüfen
                    if (datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].isEmpty()) {
                        ret = false;
                        text += PIPE + LEER + "Kein Programm angegeben!\n";
                        text += PIPE + LEER + PFEIL + "Programmname: " + datenProg.arr[DatenProg.PROGRAMM_NAME] + '\n';
                        text += PIPE + LEER + LEER + "Pfad: " + datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD] + '\n';
                    } else if (!new File(datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD]).canExecute()) {
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
                            text += PIPE + LEER + PFEIL + "Programmname: " + datenProg.arr[DatenProg.PROGRAMM_NAME] + '\n';
                            text += PIPE + LEER + LEER + "Pfad: " + datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD] + '\n';
                            if (!datenProg.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].contains(File.separator)) {
                                text += PIPE + LEER + PFEIL + "Wenn das Programm nicht im Systempfad liegt, " + '\n';
                                text += PIPE + LEER + LEER + "wird der Start nicht klappen!" + '\n';
                            }
                        }
                    }
                }
                if (ret) {
                    //sollte alles passen
                    text += PIPE + PFEIL + "Ok!" + '\n';
                }
                text += """
                        ++++++++++++++++++++++++++++++++++++++++++++


                        """;
            }
        }
        new DialogHilfe(jFrame, true, text).setVisible(true);
        return ret;
    }
}
