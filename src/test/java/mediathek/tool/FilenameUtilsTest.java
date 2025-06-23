package mediathek.tool;

import com.ibm.icu.text.Transliterator;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class FilenameUtilsTest {
    @Test
    void trailing_test_with_leading_whitespace() {
        var testStr = " betrifft: ... ";
        var result = FilenameUtils.removeWindowsTrailingDots(testStr);
        Assertions.assertEquals(" betrifft:", result);
    }

    @Test
    void trailing_test_without_leading_whitespace() {
        var testStr = "betrifft: ...";
        var result = FilenameUtils.removeWindowsTrailingDots(testStr);
        assertEquals("betrifft:", result);
    }

    @Test
    void test_remove_starting_dots() {
        var testStr = "....Paula";
        final var expected = "Paula";

        var result = FilenameUtils.stripStartingDots(testStr);
        assertEquals(expected, result);
    }

    @Test
    void test_remove_starting_dots_with_leading_whitespace() {
        // this should not modify string as we have whitespace at beginning
        var testStr = " ....Paula";
        var result = FilenameUtils.stripStartingDots(testStr);
        assertEquals(testStr, result);
    }

    @Test
    void test_utf_to_ascii_encoding() {
        var src = "Häuser Bäume Höfe Gärten daß Ü ü ö ä Ä Ö ß Â À Å Á Č Đ É ł Ł \u003F";
        var expected = "Haeuser Baeume Hoefe Gaerten dass UE ue oe ae AE OE ss A A A A C D E l L ?";

        var transliterator = Transliterator.getInstance("de-ASCII");
        var res = transliterator.transliterate(src);

        assertEquals(expected, res);
    }

    @Test
    void removeWindowsTrailingDots() {
        var testStr = "betrifft: ... ";
        var result = FilenameUtils.removeWindowsTrailingDots(testStr);
        assertEquals("betrifft:", result);
    }

    @Test
    void convertToASCIIEncoding() {
        var testStr = "hellöworld.txt";
        var result = FilenameUtils.convertToASCIIEncoding(testStr, false);
        assertEquals("helloeworld.txt", result);
    }
}