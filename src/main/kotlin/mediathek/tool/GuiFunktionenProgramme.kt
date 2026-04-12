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

package mediathek.tool

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.MVConfig
import mediathek.daten.DatenPset
import mediathek.daten.ListePset
import mediathek.gui.dialogEinstellungen.DialogImportPset
import mediathek.tool.http.MVHttpClient
import okhttp3.Request
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.Cursor
import java.io.*
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.*
import java.util.zip.ZipFile
import javax.swing.JFrame
import javax.swing.JOptionPane

object GuiFunktionenProgramme {
    private val winPfade = ArrayList<String>()
    private val logger = LogManager.getLogger()

    private const val PFAD_LINUX_VLC = "/usr/bin/vlc"
    private const val PFAD_MAC_VLC = "/Applications/VLC.app/Contents/MacOS/VLC"
    private const val PFAD_WIN = "\\VideoLAN\\VLC\\vlc.exe"

    /**
     * Use another path var for VLC on windows. Introduced in Version 10.
     */
    private const val ENV_WINDOWS_PATH_VLC = "PATH_VLC"
    private const val PFAD_LINUX_FFMPEG = "/usr/bin/ffmpeg"
    private const val PFAD_MAC_FFMPEG = "bin/ffmpeg"
    private const val PFAD_WINDOWS_FFMPEG = "bin\\ffmpeg.exe"
    private const val MSG_FILE_NOT_IMPORTED = "Die Datei wurde nicht importiert!"

    private fun setWinProgPfade() {
        System.getenv("ProgramFiles")?.let { pfad ->
            if (File(pfad).exists() && pfad !in winPfade) {
                winPfade.add(pfad)
            }
        }
        System.getenv("ProgramFiles(x86)")?.let { pfad ->
            if (File(pfad).exists() && pfad !in winPfade) {
                winPfade.add(pfad)
            }
        }

        val pathArray = arrayOf("C:\\Program Files", "C:\\Programme", "C:\\Program Files (x86)")
        for (s in pathArray) {
            if (File(s).exists() && s !in winPfade) {
                winPfade.add(s)
            }
        }
    }

    /**
     * Retrieve the path to the program jar file.
     *
     * @return The program jar file path with a separator added.
     */
    @JvmStatic
    fun getPathToApplicationJar(): String {
        // macht Probleme bei Win und Netzwerkpfaden, liefert dann Absolute Pfade zB. \\VBOXSVR\share\Mediathek\...
        val pFilePath = "pFile"
        var propFile = File(pFilePath)
        if (!propFile.exists()) {
            try {
                val codeSource = GuiFunktionenProgramme::class.java.protectionDomain.codeSource
                val jarFile = File(codeSource.location.toURI().path)
                val jarDir = jarFile.parentFile.path
                propFile = File(jarDir + File.separator + pFilePath)
            } catch (_: Exception) {
            }
        }

        var path = propFile.absolutePath.replace(pFilePath, "")
        if (!path.endsWith(File.separator)) {
            path += File.separator
        }
        return path
    }

    /**
     * Liefert den Standardpfad für das entsprechende BS.
     * Programm muss auf dem Rechner installiert sein.
     *
     * @return Pfad als String
     */
    @JvmStatic
    fun getMusterPfadVlc(): String {
        var pfad = ""
        try {
            pfad = when {
                SystemUtils.IS_OS_LINUX -> PFAD_LINUX_VLC
                SystemUtils.IS_OS_MAC_OSX -> PFAD_MAC_VLC
                else -> {
                    setWinProgPfade()
                    winPfade.firstOrNull { File(it + PFAD_WIN).exists() }?.plus(PFAD_WIN).orEmpty()
                }
            }

            if (!File(pfad).exists() && System.getenv(ENV_WINDOWS_PATH_VLC) != null) {
                pfad = System.getenv(ENV_WINDOWS_PATH_VLC)
            }
            if (!File(pfad).exists()) {
                pfad = ""
            }
        } catch (_: Exception) {
        }
        return pfad
    }

    /**
     * Liefert den Standardpfad für das entsprechende BS.
     * Bei Win+Mac wird das Programm mitgeliefert und liegt im Ordner "bin" der mit dem Programm
     * mitgeliefert wird.
     * Bei Linux muss das Programm auf dem Rechner installiert sein.
     *
     * @return Pfad als String
     */
    @JvmStatic
    fun getMusterPfadFFmpeg(): String {
        var pfad = ""
        try {
            pfad = when {
                SystemUtils.IS_OS_LINUX -> PFAD_LINUX_FFMPEG
                SystemUtils.IS_OS_MAC_OSX -> PFAD_MAC_FFMPEG
                else -> PFAD_WINDOWS_FFMPEG
            }

            if (!File(pfad).exists() && System.getenv("PATH_FFMPEG") != null) {
                pfad = System.getenv("PATH_FFMPEG")
            }
            if (!File(pfad).exists()) {
                pfad = ""
            }
        } catch (_: Exception) {
        }
        return pfad
    }

    @JvmStatic
    fun addSetVorlagen(parent: JFrame?, daten: Daten, pSet: ListePset?, setVersion: Boolean) {
        if (pSet == null) {
            MVMessageDialog.showMessageDialog(
                null,
                MSG_FILE_NOT_IMPORTED,
                "Fehler",
                JOptionPane.ERROR_MESSAGE
            )
            return
        }
        parent?.cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
        for (ps: DatenPset in pSet) {
            if (ps.addOn.isNotEmpty() && !addOnZip(ps.addOn)) {
                // und Tschüss
                MVMessageDialog.showMessageDialog(
                    null,
                    MSG_FILE_NOT_IMPORTED,
                    "Fehler",
                    JOptionPane.ERROR_MESSAGE
                )
                return
            }
        }
        parent?.cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)

        val dialog = DialogImportPset(parent, true, daten, pSet)
        dialog.isVisible = true
        if (dialog.ok) {
            if (Daten.getInstance().listePset.addPset(pSet)) {
                if (setVersion) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_VERSION_PROGRAMMSET, pSet.version)
                }
                MVMessageDialog.showMessageDialog(
                    null,
                    "${pSet.size} Programmset importiert!",
                    "Ok",
                    JOptionPane.INFORMATION_MESSAGE
                )
            } else {
                MVMessageDialog.showMessageDialog(
                    null,
                    "Die Datei wurde nicht importiert!",
                    "Fehler",
                    JOptionPane.ERROR_MESSAGE
                )
            }
        }
    }

    /**
     * Return the path to our binary directory.
     *
     * @return the path to the bin directory.
     */
    @JvmStatic
    fun getBinaryPath(): Path = Paths.get(getPathToApplicationJar()).resolve("bin")

    /**
     * On Windows exe files can also be located at res\bin...
     *
     * @return return the path to res\bin directory.
     */
    @JvmStatic
    fun getResBinaryPath(): Path = Paths.get(getPathToApplicationJar()).resolve("res").resolve("bin")

    /**
     * Search for an executable on PATH plus our bin directory.
     *
     * @param name the executable name
     * @return the path INCLUDING the binary name.
     */
    @JvmStatic
    fun findExecutableOnPath(name: String): Path {
        var exeString = name
        var path = System.getenv("PATH") + File.pathSeparator + getBinaryPath().toAbsolutePath()

        if (SystemUtils.IS_OS_WINDOWS) {
            exeString += ".exe"

            // add VLC "standard" path to path logic on windows
            path += File.pathSeparator + "C:\\Program Files\\VideoLAN\\VLC"
            // on windows (mostly during coding) binaries do only exist in res\bin directory :(
            path += File.pathSeparator + getResBinaryPath().toAbsolutePath()
        }

        if (SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_WINDOWS) {
            // also check Version 10 MV path var
            val vlcExtPathEnv = System.getenv(ENV_WINDOWS_PATH_VLC)
            if (vlcExtPathEnv != null) {
                path += File.pathSeparator + vlcExtPathEnv
            }
        }

        for (dirname in path.split(File.pathSeparator)) {
            val file = File(dirname, exeString)
            if (file.isFile) {
                return file.toPath()
            }
        }
        throw IllegalStateException(String.format("Should have found the executable %s", exeString))
    }

    private fun addOnZip(datei: String): Boolean {
        val zielPfad = GuiFunktionen.addsPfad(getPathToApplicationJar(), "bin")
        var zipFile: File

        try {
            if (!NetUtils.isUrl(datei)) {
                zipFile = File(datei)
                if (!zipFile.exists()) {
                    // und Tschüss
                    return false
                }
                if (datei.endsWith(Konstanten.FORMAT_ZIP)) {
                    if (!entpacken(zipFile, File(zielPfad))) {
                        // und Tschüss
                        return false
                    }
                } else {
                    FileInputStream(datei).use { input ->
                        FileOutputStream(GuiFunktionen.addsPfad(zielPfad, datei)).use { output ->
                            input.copyTo(output, 64 * 1024)
                        }
                    }
                }
            } else {
                val request = Request.Builder().url(datei)
                    .get()
                    .header(
                        "User-Agent",
                        ApplicationConfiguration.getConfiguration()
                            .getString(ApplicationConfiguration.APPLICATION_USER_AGENT)
                    )
                    .get()
                    .build()
                MVHttpClient.getInstance().httpClient.newCall(request).execute().use { response ->
                    val body = response.body
                    if (response.isSuccessful) {
                        body.byteStream().use { inputStream ->
                            BufferedInputStream(inputStream).use { bufferedInput ->
                                if (datei.endsWith(Konstanten.FORMAT_ZIP)) {
                                    val tmpFile = File.createTempFile("mediathek", null)
                                    tmpFile.deleteOnExit()
                                    FileOutputStream(tmpFile).use { output ->
                                        bufferedInput.copyTo(output, 64 * 1024)
                                    }
                                    if (!entpacken(tmpFile, File(zielPfad))) {
                                        // und Tschüss
                                        return false
                                    }
                                } else {
                                    val file = GuiFunktionen.getDateiName(datei)
                                    val target = File(GuiFunktionen.addsPfad(zielPfad, file))
                                    FileOutputStream(target).use { output ->
                                        bufferedInput.copyTo(output, 64 * 1024)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (_: Exception) {
        }
        return true
    }

    private fun buildDirectoryHierarchyFor(entryName: String, destDir: File): File {
        val lastIndex = entryName.lastIndexOf('/')
        val internalPathToEntry = entryName.substring(0, lastIndex + 1)
        return File(destDir, internalPathToEntry)
    }

    /**
     * Extracts archive entries into destination directory
     */
    @Throws(Exception::class)
    private fun entpacken(archive: File, destDir: File): Boolean {
        if (!destDir.exists()) {
            return false
        }

        ZipFile(archive).use { zipFile ->
            val entries = zipFile.entries()
            val buffer = ByteArray(16 * 1024)
            while (entries.hasMoreElements()) {
                val entry = entries.nextElement()
                val entryFileName = entry.name

                val dir = buildDirectoryHierarchyFor(entryFileName, destDir)
                if (!dir.exists() && !dir.mkdirs()) {
                    logger.error("entpacken(): Could not create directory {}", dir.absolutePath)
                }

                if (!entry.isDirectory) {
                    FileOutputStream(File(destDir, entryFileName)).use { fileOutput ->
                        BufferedOutputStream(fileOutput).use { bufferedOutput ->
                            BufferedInputStream(zipFile.getInputStream(entry)).use { bufferedInput ->
                                var len = bufferedInput.read(buffer)
                                while (len > 0) {
                                    bufferedOutput.write(buffer, 0, len)
                                    len = bufferedInput.read(buffer)
                                }
                                bufferedOutput.flush()
                            }
                        }
                    }
                }
            }
        }

        return true
    }

    /**
     * Check if [url] starts with any of the comma-separated prefixes in [prefixes].
     * Matching is case-insensitive.
     * Semantics:
     * - Empty [prefixes] -> returns true.
     * - Otherwise: return true if [url] starts with at least one prefix.
     */
    @JvmStatic
    fun checkPrefix(prefixes: String, url: String): Boolean {
        if (prefixes.isEmpty()) {
            return true
        }

        val lowerUrl = url.lowercase(Locale.getDefault())
        val lowerPrefixes = prefixes.lowercase(Locale.getDefault())

        val prefixesLen = lowerPrefixes.length
        var tokenStart = 0

        for (i in 0..prefixesLen) {
            if (i == prefixesLen || lowerPrefixes[i] == ',') {
                if (i > tokenStart) {
                    val tokenLen = i - tokenStart
                    if (tokenLen <= lowerUrl.length && lowerUrl.regionMatches(
                            thisOffset = 0,
                            other = lowerPrefixes,
                            otherOffset = tokenStart,
                            length = tokenLen,
                            ignoreCase = false
                        )
                    ) {
                        return true
                    }
                }
                tokenStart = i + 1
            }
        }

        return false
    }

    @JvmStatic
    fun checkSuffix(suffixes: String, url: String): Boolean {
        if (suffixes.isEmpty()) {
            return true
        }

        val lowerUrl = url.lowercase(Locale.getDefault())
        val lowerSuffixes = suffixes.lowercase(Locale.getDefault())

        val urlLen = lowerUrl.length
        val suffixesLen = lowerSuffixes.length
        var tokenStart = 0

        for (i in 0..suffixesLen) {
            if (i == suffixesLen || lowerSuffixes[i] == ',') {
                val tokenLen = i - tokenStart

                if (tokenLen in 1..urlLen) {
                    val urlStart = urlLen - tokenLen
                    if (lowerUrl.regionMatches(
                            thisOffset = urlStart,
                            other = lowerSuffixes,
                            otherOffset = tokenStart,
                            length = tokenLen,
                            ignoreCase = false
                        )
                    ) {
                        return true
                    }
                }

                tokenStart = i + 1
            }
        }

        return false
    }

    /**
     * Test if a path is a directory and writeable.
     * Path directories will be created before trying write test.
     *
     * @param path path to the directory
     * @return true if we can write a file there, false if not.
     */
    @JvmStatic
    fun checkPathWriteable(path: String): Boolean {
        if (path.isEmpty()) {
            return false
        }

        val directory = Paths.get(path)
        return try {
            if (Files.notExists(directory)) {
                Files.createDirectories(directory)
            }

            if (!Files.isDirectory(directory)) {
                return false
            }

            canWriteTempFile(directory)
        } catch (e: Exception) {
            logger.error("checkPathWriteable()", e)
            false
        }
    }

    /**
     * Test if a path can be used as a writable directory without creating missing directories.
     *
     * @param path path to the directory
     * @return true if the directory exists and is writable, or the nearest existing parent is writable.
     */
    @JvmStatic
    fun checkPathWriteableWithoutCreating(path: String): Boolean {
        if (path.isEmpty()) {
            return false
        }

        val directory = Paths.get(path)
        return try {
            if (Files.exists(directory)) {
                return Files.isDirectory(directory) && canWriteTempFile(directory)
            }

            var parent = directory.parent
            while (parent != null && Files.notExists(parent)) {
                parent = parent.parent
            }

            parent != null && Files.isDirectory(parent) && canWriteTempFile(parent)
        } catch (e: Exception) {
            logger.error("checkPathWriteableWithoutCreating()", e)
            false
        }
    }

    @Throws(IOException::class)
    private fun canWriteTempFile(directory: Path): Boolean {
        val tmpFile = Files.createTempFile(directory, "mediathek", ".tmp")
        return Files.deleteIfExists(tmpFile)
    }
}
