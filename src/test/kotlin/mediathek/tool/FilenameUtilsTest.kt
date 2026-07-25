package mediathek.tool

import org.apache.commons.lang3.SystemUtils
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assumptions.assumeFalse
import org.junit.jupiter.api.Assumptions.assumeTrue
import org.junit.jupiter.api.Test

internal class FilenameUtilsTest {
    @Test
    fun trailing_test_with_leading_whitespace() {
        val result = FilenameUtils.removeWindowsTrailingDots(" betrifft: ... ")
        assertEquals(" betrifft:", result)
    }

    @Test
    fun trailing_test_without_leading_whitespace() {
        val result = FilenameUtils.removeWindowsTrailingDots("betrifft: ...")
        assertEquals("betrifft:", result)
    }

    @Test
    fun test_remove_starting_dots() {
        val result = FilenameUtils.stripStartingDots("....Paula")
        assertEquals("Paula", result)
    }

    @Test
    fun test_remove_starting_dots_with_leading_whitespace() {
        val testStr = " ....Paula"
        val result = FilenameUtils.stripStartingDots(testStr)
        assertEquals(testStr, result)
    }

    @Test
    fun test_utf_to_ascii_encoding() {
        val src = "Häuser Bäume Höfe Gärten daß Ü ü ö ä Ä Ö ß Â À Å Á Č Đ É ł Ł ?"
        val expected =
            if (SystemUtils.IS_OS_WINDOWS) {
                "Haeuser Baeume Hoefe Gaerten dass UE ue oe ae AE OE ss A A A A C D E l L _"
            } else {
                "Haeuser Baeume Hoefe Gaerten dass UE ue oe ae AE OE ss A A A A C D E l L ?"
            }
        val result = FilenameUtils.convertToASCIIEncoding(src, false)

        assertEquals(expected, result)
    }

    @Test
    fun removeWindowsTrailingDots() {
        val result = FilenameUtils.removeWindowsTrailingDots("betrifft: ... ")
        assertEquals("betrifft:", result)
    }

    @Test
    fun convertToASCIIEncoding() {
        val result = FilenameUtils.convertToASCIIEncoding("hellöworld.txt", false)
        assertEquals("helloeworld.txt", result)
    }

    @Test
    fun pathCleanupMatchesPathValidationForHiddenSegmentsOnUnix() {
        assumeFalse(SystemUtils.IS_OS_WINDOWS)

        val input = "folder/.hidden/file"
        val expected = "folder/hidden/file"

        assertEquals(expected, FilenameUtils.checkFilenameForIllegalCharacters(input, true))
        assertEquals(expected, FilenameUtils.replaceEmptyFilename(input, true, null, false))
    }

    @Test
    fun pathValidationPreservesUncPrefixOnWindows() {
        assumeTrue(SystemUtils.IS_OS_WINDOWS)

        val input = "\\\\server\\share\\folder"

        assertEquals(input, FilenameUtils.checkFilenameForIllegalCharacters(input, true))
    }
}
