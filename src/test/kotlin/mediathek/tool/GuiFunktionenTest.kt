package mediathek.tool

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test
import java.io.File

internal class GuiFunktionenTest {

    @Test
    fun getFilmListUpdateType() {
        val result = GuiFunktionen.getFilmListUpdateType()
        assertSame(FilmListUpdateType.AUTOMATIC, result)
    }

    @Test
    fun getSuffixFromUrl() {
        val testStr = "https://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd"
        val result = GuiFunktionen.getSuffixFromUrl(testStr)

        assertEquals("m3u8", result)
    }

    @Test
    fun getFileNameWithoutExtension_web() {
        val testStr = "https://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd"
        val expected = "https://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av"
        val result = GuiFunktionen.getFileNameWithoutExtension(testStr)

        assertEquals(expected, result)
    }

    @Test
    fun getFileNameWithoutExtension_file() {
        val testStr = "/Users/derreisende/file1.mp4"
        val result = GuiFunktionen.getFileNameWithoutExtension(testStr)

        assertEquals("/Users/derreisende/file1", result)
    }

    @Test
    fun getFileNameWithoutExtension_fileWithQuestionMark() {
        val testStr = "/Users/derreisende/Downloads/mediathek/Die Nordreportage/Die Nordreportage-Wie geht das? Fertigung eines Windrades-0143177029.mp4"
        val expected = "/Users/derreisende/Downloads/mediathek/Die Nordreportage/Die Nordreportage-Wie geht das? Fertigung eines Windrades-0143177029"
        val result = GuiFunktionen.getFileNameWithoutExtension(testStr)

        assertEquals(expected, result)
    }

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
