/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.dialogEinstellungen.DialogImportPset;
import mediathek.tool.http.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class GuiFunktionenProgramme {

    private static final ArrayList<String> winPfade = new ArrayList<>();
    private static final Logger logger = LogManager.getLogger();
    private static final String PFAD_LINUX_VLC = "/usr/bin/vlc";
    private static final String PFAD_MAC_VLC = "/Applications/VLC.app/Contents/MacOS/VLC";
    private static final String PFAD_WIN = "\\VideoLAN\\VLC\\vlc.exe";
    /**
     * Use another path var for VLC on windows. Introduced in Version 10.
     */
    private static final String ENV_WINDOWS_PATH_VLC = "PATH_VLC";
    private static final String PFAD_LINUX_FFMPEG = "/usr/bin/ffmpeg";
    private static final String PFAD_MAC_FFMPEG = "bin/ffmpeg";
    private static final String PFAD_WINDOWS_FFMPEG = "bin\\ffmpeg.exe";

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

    /**
     * Retrieve the path to the program jar file.
     *
     * @return The program jar file path with a separator added.
     */
    public static String getPathToApplicationJar() {
        // macht Probleme bei Win und Netzwerkpfaden, liefert dann Absolute Pfade zB. \\VBOXSVR\share\Mediathek\...
        final var pFilePath = "pFile";
        var propFile = new File(pFilePath);
        if (!propFile.exists()) {
            try {
                final var cS = GuiFunktionenProgramme.class.getProtectionDomain().getCodeSource();
                final var jarFile = new File(cS.getLocation().toURI().getPath());
                final var jarDir = jarFile.getParentFile().getPath();
                propFile = new File(jarDir + File.separator + pFilePath);
            }
            catch (Exception ignored) {
            }
        }

        var s = propFile.getAbsolutePath().replace(pFilePath, "");
        if (!s.endsWith(File.separator)) {
            s += File.separator;
        }
        return s;
    }

    /**
     * Liefert den Standardpfad für das entsprechende BS.
     * Programm muss auf dem Rechner installiert sein.
     *
     * @return Pfad als String
     */
    public static String getMusterPfadVlc() {
        String pfad = "";
        try {
            if (SystemUtils.IS_OS_LINUX) {
                pfad = PFAD_LINUX_VLC;
            }
            else if (SystemUtils.IS_OS_MAC_OSX) {
                pfad = PFAD_MAC_VLC;
            }
            else {
                setWinProgPfade();
                for (String s : winPfade) {
                    pfad = s + PFAD_WIN;
                    if (new File(pfad).exists()) {
                        break;
                    }
                }
            }

            if (!new File(pfad).exists() && System.getenv(ENV_WINDOWS_PATH_VLC) != null) {
                pfad = System.getenv(ENV_WINDOWS_PATH_VLC);
            }
            if (!new File(pfad).exists()) {
                pfad = "";
            }
        }
        catch (Exception ignore) {
        }
        return pfad;
    }

    /**
     * Liefert den Standardpfad für das entsprechende BS.
     * Bei Win+Mac wird das Programm mitgeliefert und liegt im Ordner "bin" der mit dem Programm
     * mitgeliefert wird.
     * Bei Linux muss das Programm auf dem Rechner installiert sein.
     *
     * @return Pfad als String
     */
    public static String getMusterPfadFFmpeg() {
        String pfad = "";
        try {
            if (SystemUtils.IS_OS_LINUX)
                pfad = PFAD_LINUX_FFMPEG;
            else if (SystemUtils.IS_OS_MAC_OSX)
                pfad = PFAD_MAC_FFMPEG;
            else
                pfad = PFAD_WINDOWS_FFMPEG;

            if (!new File(pfad).exists() && System.getenv("PATH_FFMPEG") != null) {
                pfad = System.getenv("PATH_FFMPEG");
            }
            if (!new File(pfad).exists()) {
                pfad = "";
            }
        }
        catch (Exception ignore) {
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

            }
            else {
                MVMessageDialog.showMessageDialog(null, "Die Datei wurde nicht importiert!",
                        "Fehler", JOptionPane.ERROR_MESSAGE);

            }
        }
    }

    /**
     * Return the path to our binary directory.
     *
     * @return the path to the bin directory.
     */
    public static Path getBinaryPath() {
        return Paths.get(GuiFunktionenProgramme.getPathToApplicationJar()).resolve("bin");
    }

    /**
     * On Windows exe files can also be located at res\bin...
     *
     * @return return the path to res\bin directory.
     */
    public static Path getResBinaryPath() {
        return Paths.get(GuiFunktionenProgramme.getPathToApplicationJar()).resolve("res").resolve("bin");
    }

    /**
     * Search for an executable on PATH plus our bin directory.
     *
     * @param name the executable name
     * @return the path INCLUDING the binary name.
     */
    public static Path findExecutableOnPath(String name) {
        var exeString = name;

        var path = System.getenv("PATH");
        path = path + File.pathSeparatorChar + getBinaryPath().toAbsolutePath();

        if (SystemUtils.IS_OS_WINDOWS) {
            exeString += ".exe";

            // add VLC "standard" path to path logic on windows
            path += File.pathSeparatorChar + "C:\\Program Files\\VideoLAN\\VLC";
            // on windows (mostly during coding) binaries do only exist in res\bin directory :(
            path = path + File.pathSeparatorChar + getResBinaryPath().toAbsolutePath();
        }

        if (SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_WINDOWS) {
            // also check Version 10 MV path var
            var vlcExtPathEnv = System.getenv(ENV_WINDOWS_PATH_VLC);
            if (vlcExtPathEnv != null) {
                path += File.pathSeparatorChar + vlcExtPathEnv;
            }
        }

        for (String dirname : path.split(File.pathSeparator)) {
            File file = new File(dirname, exeString);
            if (file.isFile()) {
                return file.toPath();
            }
        }
        throw new IllegalStateException(String.format("Should have found the executable %s", exeString));
    }

    private static boolean addOnZip(String datei) {
        String zielPfad = GuiFunktionen.addsPfad(getPathToApplicationJar(), "bin");
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
                }
                else {
                    try (FileInputStream in = new FileInputStream(datei);
                         FileOutputStream fOut = new FileOutputStream(GuiFunktionen.addsPfad(zielPfad, datei))) {
                        final byte[] buffer = new byte[64 * 1024];
                        while ((n = in.read(buffer)) != -1) {
                            fOut.write(buffer, 0, n);
                        }
                    }
                }
            }
            else {
                final Request request = new Request.Builder().url(datei).get()
                        .header("User-Agent", ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_USER_AGENT))
                        .get().build();
                try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
                     ResponseBody body = response.body()) {
                    if (response.isSuccessful()) {
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
                            }
                            else {
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
        }
        catch (Exception ignored) {
        }
        return true;
    }

    private static File buildDirectoryHierarchyFor(String entryName, File destDir) {
        int lastIndex = entryName.lastIndexOf('/');
        String internalPathToEntry = entryName.substring(0, lastIndex + 1);
        return new File(destDir, internalPathToEntry);
    }


    /**
     * Extracts archive entries into destination directory
     */
    private static boolean entpacken(File archive, File destDir) throws Exception {
        if (!destDir.exists()) {
            return false;
        }

        try (ZipFile zipFile = new ZipFile(archive)) {
            Enumeration<? extends ZipEntry> entries = zipFile.entries();

            byte[] buffer = new byte[16*1024];
            int len;
            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                String entryFileName = entry.getName();

                File dir = buildDirectoryHierarchyFor(entryFileName, destDir);
                if (!dir.exists()) {
                    if (!dir.mkdirs())
                        logger.error("entpacken(): Could not create directory {}", dir.getAbsolutePath());
                }

                if (!entry.isDirectory()) {
                    try (var fos = new FileOutputStream(new File(destDir, entryFileName));
                         var bos = new BufferedOutputStream(fos);
                         var bis = new BufferedInputStream(zipFile.getInputStream(entry))) {
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

    /**
     * Check if {@code url} starts with any of the comma-separated prefixes in {@code prefixes}.
     * Matching is case-insensitive.
     * Semantics:
     * - Empty {@code prefixes} -> returns true.
     * - Otherwise: return true if {@code url} starts with at least one prefix.
     */
    public static boolean checkPrefix(@NotNull String prefixes, @NotNull String url) {
        if (prefixes.isEmpty()) {
            return true;
        }

        final String lowerUrl = url.toLowerCase();
        final String lowerPrefixes = prefixes.toLowerCase();

        final int prefixesLen = lowerPrefixes.length();
        int tokenStart = 0;

        for (int i = 0; i <= prefixesLen; i++) {
            if (i == prefixesLen || lowerPrefixes.charAt(i) == ',') {
                if (i > tokenStart) {
                    final int tokenLen = i - tokenStart;

                    if (tokenLen <= lowerUrl.length()
                            && lowerUrl.regionMatches(0, lowerPrefixes, tokenStart, tokenLen)) {
                        return true;
                    }
                }
                tokenStart = i + 1;
            }
        }

        return false;
    }

    public static boolean checkSuffix(@NotNull String suffixes, @NotNull String url) {
        if (suffixes.isEmpty()) {
            return true;
        }

        final String lowerUrl = url.toLowerCase();
        final String lowerSuffixes = suffixes.toLowerCase();

        final int urlLen = lowerUrl.length();
        final int suffixesLen = lowerSuffixes.length();

        int tokenStart = 0;

        for (int i = 0; i <= suffixesLen; i++) {
            if (i == suffixesLen || lowerSuffixes.charAt(i) == ',') {
                int tokenLen = i - tokenStart;

                if (tokenLen > 0) {
                    if (tokenLen <= urlLen) {
                        int urlStart = urlLen - tokenLen;
                        if (lowerUrl.regionMatches(urlStart, lowerSuffixes, tokenStart, tokenLen)) {
                            return true;
                        }
                    }
                }

                tokenStart = i + 1;
            }
        }

        return false;
    }

    /**
     * Test if a path is a directory and writeable.
     * Path directories will be created before trying write test.
     *
     * @param path path to the directory
     * @return true if we can write a file there, false if not.
     */
    public static boolean checkPathWriteable(@NotNull String path) {
        if (path.isEmpty())
            return false;

        final Path directory = Paths.get(path);
        try {
            if (Files.notExists(directory)) {
                Files.createDirectories(directory);
            }

            if (!Files.isDirectory(directory)) {
                return false;
            }

            final Path tmpFile = Files.createTempFile(directory, "mediathek", ".tmp");
            return Files.deleteIfExists(tmpFile);
        }
        catch (Exception e) {
            logger.error("checkPathWriteable()", e);
            return false;
        }
    }
}
