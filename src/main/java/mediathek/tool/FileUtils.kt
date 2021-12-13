package mediathek.tool

import com.sun.jna.platform.FileUtils
import java.io.File
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.text.StringCharacterIterator
import kotlin.math.abs

object FileUtils {
    /**
     * Move a file to the OS trash if supported, otherwise delete it.
     * @param filePath the pathe to the file to be deleted.
     * @throws IOException any occuring exception.
     */
    @Throws(IOException::class)
    @JvmStatic
    fun moveToTrash(filePath: Path) {
        val fileUtils = FileUtils.getInstance()
        if (fileUtils.hasTrash()) fileUtils.moveToTrash(filePath.toFile()) else Files.deleteIfExists(filePath)
    }

    const val ONE_KB: Long = 1024
    const val ONE_MB: Long = ONE_KB * ONE_KB
    const val ONE_GB: Long = ONE_KB * ONE_MB

    @JvmStatic
    fun removeExtension(fileName: String): String {
        return File(fileName).nameWithoutExtension
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
}