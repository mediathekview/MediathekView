package mediathek.tool

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.io.File

internal class GuiFunktionenTest {

    @Test
    fun concatPaths() {
        val separator = File.separator
        assertEquals("", GuiFunktionen.concatPaths(null, null))
        assertEquals("", GuiFunktionen.concatPaths(null, "b"))
        assertEquals("", GuiFunktionen.concatPaths("a", null))
        assertEquals("ab", GuiFunktionen.concatPaths("", "ab"))
        assertEquals("ab", GuiFunktionen.concatPaths("ab", ""))

        assertEquals("a${separator}b", GuiFunktionen.concatPaths("a", "b"))
        assertEquals("a${separator}b", GuiFunktionen.concatPaths("a$separator", "b"))
        assertEquals("a${separator}b", GuiFunktionen.concatPaths("a", "${separator}b"))
        assertEquals("a${separator}b", GuiFunktionen.concatPaths("a$separator", "${separator}b"))
        assertEquals("a${separator}b", GuiFunktionen.concatPaths("a$separator$separator", "b"))

        if (separator == "\\") {
            assertEquals("\\\\server\\share\\file", GuiFunktionen.concatPaths("\\\\server\\share", "file"))
            assertEquals("\\\\server\\share\\file", GuiFunktionen.concatPaths("\\\\server\\share\\", "file"))
        } else {
            assertEquals("//server/share/file", GuiFunktionen.concatPaths("//server/share", "file"))
            assertEquals("//server/share/file", GuiFunktionen.concatPaths("//server/share/", "file"))
        }
    }
}
