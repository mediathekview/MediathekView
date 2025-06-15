/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool.ttml.exporters;

import mediathek.tool.ttml.ITtmlExporter;
import mediathek.tool.ttml.TimedTextMarkupLanguageParser;
import mediathek.tool.ttml.parsers.EbuTimedTextMarkupLanguageParser;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SubripTextExporterTest {
    @Test
    void convert_ttml_to_srt() {
        Path tempSrt = null;
        try (EbuTimedTextMarkupLanguageParser parser = new EbuTimedTextMarkupLanguageParser()) {
            var file = new File("src/test/resources/ttml/testcase1.ttml");
            var ttmlPath = file.toPath();
            var res = parser.parse(ttmlPath);
            assertTrue(res);

            tempSrt = Files.createTempFile("converted_srt_test_case", ".srt");
            ITtmlExporter exporter = new SubripTextExporter();
            exporter.write(parser, tempSrt);

            file = new File("src/test/resources/ttml/testcase1.srt");
            var expectedSrtResultPath = file.toPath();
            var mismatch = Files.mismatch(expectedSrtResultPath, tempSrt);
            assertEquals(-1L, mismatch);
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
        finally {
            if (tempSrt != null) {
                try {
                    Files.deleteIfExists(tempSrt);
                }
                catch (IOException ignored) {
                }
            }
        }
    }

    @Test
    void belowHundred() throws Exception {
        try (EbuTimedTextMarkupLanguageParser ebuParser = new EbuTimedTextMarkupLanguageParser()) {
            // gain access to private fields
            var ttmlFormatterField = TimedTextMarkupLanguageParser.class.getDeclaredField("ttmlFormat");
            ttmlFormatterField.setAccessible(true);
            var srtFormatterField = SubripTextExporter.class.getDeclaredField("srtFormat");
            srtFormatterField.setAccessible(true);

            // get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormatterField.get(ebuParser);
            SimpleDateFormat srtFormatter = (SimpleDateFormat) srtFormatterField.get(new SubripTextExporter());

            var ttmlDate = ttmlFormatter.parse("00:03:00.080");
            var srtString = srtFormatter.format(ttmlDate);
            assertTrue(srtString.endsWith(",080"));
        }
    }

    @Test
    void aboveHundred() throws Exception {
        try (TimedTextMarkupLanguageParser parser = new EbuTimedTextMarkupLanguageParser()) {
            // gain access to private fields
            var ttmlFormatterField = TimedTextMarkupLanguageParser.class.getDeclaredField("ttmlFormat");
            ttmlFormatterField.setAccessible(true);
            var srtFormatterField = SubripTextExporter.class.getDeclaredField("srtFormat");
            srtFormatterField.setAccessible(true);

            // get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormatterField.get(parser);
            SimpleDateFormat srtFormatter = (SimpleDateFormat) srtFormatterField.get(new SubripTextExporter());

            var ttmlDate = ttmlFormatter.parse("00:03:04.400");
            var srtString = srtFormatter.format(ttmlDate);
            assertTrue(srtString.endsWith(",400"));
        }
    }
}