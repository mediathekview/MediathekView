package mediathek.tool

import mediathek.mac.MacFileUtils
import mediathek.tool.http.MVHttpClient
import mediathek.windows.WindowsFileUtils
import okhttp3.Request
import org.apache.commons.lang3.SystemUtils
import java.io.File
import java.io.IOException
import java.nio.file.*
import java.text.StringCharacterIterator
import kotlin.math.abs

object FileUtils {
    /**
     * Move a file to the OS trash if supported, otherwise delete it.
     * @param filePath the pathe to the file to be deleted.
     */
    @JvmStatic
    fun moveToTrash(filePath: Path) {
        try {
            if (SystemUtils.IS_OS_MAC_OSX) {
                MacFileUtils.moveToTrash(filePath.toFile())
            } else if (SystemUtils.IS_OS_WINDOWS) {
                WindowsFileUtils.moveToTrash(filePath.toFile())
            } else {
                Files.deleteIfExists(filePath)
            }
        } catch (_: IOException) {
            Files.deleteIfExists(filePath)
        }
    }

    const val ONE_KB: Long = 1024
    const val ONE_MB: Long = ONE_KB * ONE_KB
    const val ONE_GB: Long = ONE_KB * ONE_MB

    @JvmStatic
    fun removeExtension(fileName: String): String {
        return File(fileName).nameWithoutExtension
    }

    @Throws(IOException::class)
    fun moveAtomicallyWithFallback(source: Path, target: Path) {
        try {
            Files.move(source, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
            return
        } catch (_: AtomicMoveNotSupportedException) {
            // Retry below without atomic semantics.
        }

        moveNonAtomicOrCopyDelete(source, target)
    }

    @Throws(IOException::class)
    private fun moveNonAtomicOrCopyDelete(source: Path, target: Path) {
        try {
            Files.move(source, target, StandardCopyOption.REPLACE_EXISTING)
        } catch (e: FileSystemException) {
            if (isCrossDeviceMoveError(source, target, e)) {
                Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
                Files.delete(source)
            } else {
                throw e
            }
        }
    }

    private fun isCrossDeviceMoveError(source: Path, target: Path, e: FileSystemException): Boolean {
        try {
            val targetProbe = if (Files.exists(target)) target else target.parent
            if (targetProbe != null && Files.getFileStore(source) != Files.getFileStore(targetProbe)) {
                return true
            }
        } catch (_: IOException) {
            // Fall back to reason parsing below.
        }

        val reason = e.reason ?: return false
        val normalized = reason.lowercase()
        return normalized.contains("cross-device") ||
            normalized.contains("exdev") ||
            (normalized.contains("link") && normalized.contains("device"))
    }

    @Throws(IOException::class)
    fun deletePathRecursively(rootPath: Path) {
        Files.walk(rootPath/*, FileVisitOption.FOLLOW_LINKS*/).use { walk ->
            walk.sorted(Comparator.reverseOrder())
                .map(Path::toFile)
                //.peek(System.out::println)
                .forEach(File::delete)
        }
    }

    @JvmStatic
    fun humanReadableByteCountBinary(bytes: Long): String {
        val absB = if (bytes == Long.MIN_VALUE) Long.MAX_VALUE else abs(bytes)
        if (absB < ONE_KB) {
            return "$bytes B"
        }
        var value = absB

        val ci = StringCharacterIterator("KMGTPE")
        var i = 40
        while (i >= 0 && absB > 0xfffccccccccccccL shr i) {
            value = value shr 10
            ci.next()
            i -= 10
        }
        value *= java.lang.Long.signum(bytes).toLong()
        return String.format("%.1f %ciB", value / 1024.0, ci.current())
    }

    /**
     * Downloads the given URI into a fresh temp file and returns its Path.
     * Caller should delete the file when done.
     */
    @Throws(IOException::class)
    fun downloadToTempFile(uri: String): Path {
        val target = Files.createTempFile("mv-download-", ".tmp")
        // Use a sibling temp to avoid partially-written files if you later decide to move/rename.
        val writing = target.resolveSibling(target.fileName.toString() + ".part")

        val request = Request.Builder().url(uri).get().build()

        try {
            MVHttpClient.httpClient.newCall(request).execute().use { response ->
                if (!response.isSuccessful) {
                    throw IOException("Download failed: HTTP " + response.code + " " + response.message)
                }
                response.body.use { body ->
                    body.byteStream().use { `in` ->
                        Files.copy(`in`, writing, StandardCopyOption.REPLACE_EXISTING)
                    }
                }
                moveAtomicallyWithFallback(writing, target)
                return target
            }
        } catch (e: IOException) {
            // Best-effort cleanup on failure
            try {
                Files.deleteIfExists(writing)
            } catch (_: IOException) {
            }
            try {
                Files.deleteIfExists(target)
            } catch (_: IOException) {
            }
            throw e
        }
    }
}
