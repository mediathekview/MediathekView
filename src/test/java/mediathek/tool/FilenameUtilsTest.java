package mediathek.tool;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class FilenameUtilsTest {
    @Test
    void trailing_test_with_leading_whitespace() {
        var testStr = " betrifft: ... ";
        var result = FilenameUtils.removeWindowsTrailingDots(testStr);
        Assertions.assertEquals(result, " betrifft:");
    }

    @Test
    void trailing_test_without_leading_whitespace() {
        var testStr = "betrifft: ...";
        var result = FilenameUtils.removeWindowsTrailingDots(testStr);
        Assertions.assertEquals(result, "betrifft:");

    }

    @Test
    void test_remove_starting_dots() {
        var testStr = "....Paula";
        var result = FilenameUtils.removeStartingDots(testStr);
        Assertions.assertEquals(result, "Paula");
    }
}