package mediathek.tool;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.text.SimpleDateFormat;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TimedTextMarkupLanguageParserTest {
    @Test
    void test1_belowHundred() throws Exception {
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            //gain access to private fields
            var ttmlFormtterField = TimedTextMarkupLanguageParser.class.getDeclaredField("ttmlFormat");
            ttmlFormtterField.setAccessible(true);
            var srtFormatterField = TimedTextMarkupLanguageParser.class.getDeclaredField("srtFormat");
            srtFormatterField.setAccessible(true);

            //get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormtterField.get(parser);
            SimpleDateFormat srtFormatter = (SimpleDateFormat) srtFormatterField.get(parser);

            var ttmlDate = ttmlFormatter.parse("00:03:00.080");
            var srtString = srtFormatter.format(ttmlDate);
            assertTrue(srtString.endsWith(",080"));
        }
    }

    @Test
    void test2_belowHundred() throws Exception {
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            //gain access to private fields
            var ttmlFormtterField = TimedTextMarkupLanguageParser.class.getDeclaredField("ttmlFormat");
            ttmlFormtterField.setAccessible(true);
            var srtFormatterField = TimedTextMarkupLanguageParser.class.getDeclaredField("srtFormat");
            srtFormatterField.setAccessible(true);

            //get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormtterField.get(parser);
            SimpleDateFormat srtFormatter = (SimpleDateFormat) srtFormatterField.get(parser);

            //variable fraction
            var ttmlDate = ttmlFormatter.parse("00:03:00.80");
            var srtString = srtFormatter.format(ttmlDate);
            assertTrue(srtString.endsWith(",080"));
        }
    }

    @Test
    void test_aboveHundred() throws Exception {
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            //gain access to private fields
            var ttmlFormtterField = TimedTextMarkupLanguageParser.class.getDeclaredField("ttmlFormat");
            ttmlFormtterField.setAccessible(true);
            var srtFormatterField = TimedTextMarkupLanguageParser.class.getDeclaredField("srtFormat");
            srtFormatterField.setAccessible(true);

            //get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormtterField.get(parser);
            SimpleDateFormat srtFormatter = (SimpleDateFormat) srtFormatterField.get(parser);

            var ttmlDate = ttmlFormatter.parse("00:03:04.400");
            var srtString = srtFormatter.format(ttmlDate);
            assertTrue(srtString.endsWith(",400"));
        }
    }

    @Test
    @DisplayName("Fail for unsupported TTML format")
    void unsupported_ttml_file_format() {
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            var file = new File("src/test/resources/ttml/ttml_fail_test_file.ttml");
            var res = parser.parse(file.toPath());
            assertFalse(res);
        }
    }
}