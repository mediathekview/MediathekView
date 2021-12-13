package mediathek.tool;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

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
        Assertions.assertEquals("betrifft:", result);

    }

    @Test
    void test_remove_starting_dots() {
        var testStr = "....Paula";
        var result = StringUtils.stripStart(testStr, ".");
        Assertions.assertEquals("Paula", result);
    }

    @Test
    void test_remove_starting_dots_with_leading_whitespace() {
        // this should not modify string as we have whitespace at beginning
        var testStr = " ....Paula";
        var result = StringUtils.stripStart(testStr, ".");
        Assertions.assertEquals(testStr, result);
    }
}