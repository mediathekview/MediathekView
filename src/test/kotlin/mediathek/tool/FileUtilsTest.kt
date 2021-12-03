package mediathek.tool

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class FileUtilsTest {

    @Test
    fun removeExtension() {
        val testStr = "file.jpeg"
        val result = FileUtils.removeExtension(testStr)
        assertEquals("file", result)
    }

    @Test
    fun humanReadableByteCountBinary_megabytes() {
        val bytes:Long = 25165824
        val result = FileUtils.humanReadableByteCountBinary(bytes)

        val expected = String.format("%.1f MiB", (bytes / FileUtils.ONE_MB.toDouble()));
        assertEquals(expected, result)
    }

    @Test
    fun humanReadableByteCountBinary_kilobytes() {
        val bytes:Long = 24*1024
        val result = FileUtils.humanReadableByteCountBinary(bytes)

        val expected = String.format("%.1f KiB", (bytes / FileUtils.ONE_KB.toDouble()));
        assertEquals(expected, result)
    }
}