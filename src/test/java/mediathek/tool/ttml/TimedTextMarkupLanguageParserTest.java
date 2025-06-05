package mediathek.tool.ttml;

import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TimedTextMarkupLanguageParserTest {
    @Test
    void test1_belowHundred() throws Exception {
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            // gain access to private fields
            var ttmlFormtterField = TimedTextMarkupLanguageParser.class.getDeclaredField("ttmlFormat");
            ttmlFormtterField.setAccessible(true);
            var assFormatterMethod =
                    TimedTextMarkupLanguageParser.class.getDeclaredMethod("getAssTime", Date.class);
            assFormatterMethod.setAccessible(true);
            var srtFormatterField = TimedTextMarkupLanguageParser.class.getDeclaredField("srtFormat");
            srtFormatterField.setAccessible(true);

            // get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormtterField.get(parser);
            SimpleDateFormat srtFormatter = (SimpleDateFormat) srtFormatterField.get(parser);

            var ttmlDate = ttmlFormatter.parse("00:03:00.080");
            var assString = (String) assFormatterMethod.invoke(parser, ttmlDate);
            var srtString = srtFormatter.format(ttmlDate);
            assertTrue(assString.endsWith(".08"));
            assertTrue(srtString.endsWith(",080"));
        }
    }

    @Test
    void test2_belowHundred() throws Exception {
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            // gain access to private fields
            var ttmlFormtterField = TimedTextMarkupLanguageParser.class.getDeclaredField("ttmlFormat");
            ttmlFormtterField.setAccessible(true);
            var assFormatterMethod =
                    TimedTextMarkupLanguageParser.class.getDeclaredMethod("getAssTime", Date.class);
            assFormatterMethod.setAccessible(true);
            var srtFormatterField = TimedTextMarkupLanguageParser.class.getDeclaredField("srtFormat");
            srtFormatterField.setAccessible(true);

            // get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormtterField.get(parser);
            SimpleDateFormat srtFormatter = (SimpleDateFormat) srtFormatterField.get(parser);

            // variable fraction
            var ttmlDate = ttmlFormatter.parse("00:03:00.80");
            var assString = (String) assFormatterMethod.invoke(parser, ttmlDate);
            var srtString = srtFormatter.format(ttmlDate);
            assertTrue(assString.endsWith(".08"));
            assertTrue(srtString.endsWith(",080"));
        }
    }

    @Test
    void test_aboveHundred() throws Exception {
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            // gain access to private fields
            var ttmlFormtterField = TimedTextMarkupLanguageParser.class.getDeclaredField("ttmlFormat");
            ttmlFormtterField.setAccessible(true);
            var assFormatterMethod =
                    TimedTextMarkupLanguageParser.class.getDeclaredMethod("getAssTime", Date.class);
            assFormatterMethod.setAccessible(true);
            var srtFormatterField = TimedTextMarkupLanguageParser.class.getDeclaredField("srtFormat");
            srtFormatterField.setAccessible(true);

            // get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormtterField.get(parser);
            SimpleDateFormat srtFormatter = (SimpleDateFormat) srtFormatterField.get(parser);

            var ttmlDate = ttmlFormatter.parse("00:03:04.400");
            var assString = (String) assFormatterMethod.invoke(parser, ttmlDate);
            var srtString = srtFormatter.format(ttmlDate);
            assertTrue(assString.endsWith(".40"));
            assertTrue(srtString.endsWith(",400"));
        }
    }

    @Test
    void convert_ttml_to_ass() {
        Path tempAss = null;
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            var file = new File("src/test/resources/ttml/testcase1.ttml");
            var ttmlPath = file.toPath();
            var res = parser.parse(ttmlPath);
            assertTrue(res);

            tempAss = Files.createTempFile("converted_ass_test_case", ".ass");
            parser.toAss(tempAss);

            file = new File("src/test/resources/ttml/testcase1.ass");
            var expectedAssResultPath = file.toPath();

            try (var fr1 = new FileReader(expectedAssResultPath.toFile());
                 var fr2 = new FileReader(tempAss.toFile());
                 var r1 = new BufferedReader(fr1); var r2 = new BufferedReader(fr2)) {
                res = IOUtils.contentEqualsIgnoreEOL(r1, r2);
                assertTrue(res);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            if (tempAss != null) {
                try {
                    Files.deleteIfExists(tempAss);
                } catch (IOException ignored) {
                }
            }
        }
    }

    @Test
    void convert_ttml_to_srt() {
        Path tempSrt = null;
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            var file = new File("src/test/resources/ttml/testcase1.ttml");
            var ttmlPath = file.toPath();
            var res = parser.parse(ttmlPath);
            assertTrue(res);

            tempSrt = Files.createTempFile("converted_srt_test_case", ".srt");
            parser.toSrt(tempSrt);

            file = new File("src/test/resources/ttml/testcase1.srt");
            var expectedSrtResultPath = file.toPath();
            var mismatch = Files.mismatch(expectedSrtResultPath, tempSrt);
            assertEquals(-1L, mismatch);
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            if (tempSrt != null) {
                try {
                    Files.deleteIfExists(tempSrt);
                } catch (IOException ignored) {
                }
            }
        }
    }

    @Test
    @DisplayName("Check alignment conversion")
    void convertAlignment() {
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            // convert trash to 0
            int alignment = parser.convertAlignment("hello");
            assertEquals(0, alignment);
            //now check the real values
            alignment = parser.convertAlignment("start");
            assertEquals(-1, alignment, "start conversion failed");
            alignment = parser.convertAlignment("left");
            assertEquals(-1, alignment, "left conversion failed");
            alignment = parser.convertAlignment("center");
            assertEquals(0, alignment, "center conversion failed");
            alignment = parser.convertAlignment("right");
            assertEquals(1, alignment, "right conversion failed");
            alignment = parser.convertAlignment("end");
            assertEquals(1, alignment, "end conversion failed");
        }
    }

    @Test
    void convertRegion() {
        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            // convert trash
            int region = parser.convertRegion("hello");
            assertEquals(2, region, "trash conversion failed");
            region = parser.convertRegion("before");
            assertEquals(8, region, "before conversion failed");
            region = parser.convertRegion("center");
            assertEquals(5, region, "center conversion failed");
        }
    }
}
