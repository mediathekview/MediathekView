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

import mediathek.tool.ttml.TimedTextMarkupLanguageParser;
import mediathek.tool.ttml.parsers.EbuTimedTextMarkupLanguageParser;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertTrue;

class AdvancedSubstationAlphaExporterTest {
    @Test
    void convert_ttml_to_ass() {
        Path tempAss = null;
        try (EbuTimedTextMarkupLanguageParser parser = new EbuTimedTextMarkupLanguageParser()) {
            var file = new File("src/test/resources/ttml/testcase1.ttml");
            var ttmlPath = file.toPath();
            var res = parser.parse(ttmlPath);
            assertTrue(res);

            tempAss = Files.createTempFile("converted_ass_test_case", ".ass");
            AdvancedSubstationAlphaExporter exporter = new AdvancedSubstationAlphaExporter();
            exporter.write(parser, tempAss);

            file = new File("src/test/resources/ttml/testcase1.ass");
            var expectedAssResultPath = file.toPath();

            try (var fr1 = new FileReader(expectedAssResultPath.toFile());
                 var fr2 = new FileReader(tempAss.toFile());
                 var r1 = new BufferedReader(fr1); var r2 = new BufferedReader(fr2)) {
                res = IOUtils.contentEqualsIgnoreEOL(r1, r2);
                assertTrue(res);
            }
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
        finally {
            if (tempAss != null) {
                try {
                    Files.deleteIfExists(tempAss);
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
            var assFormatterMethod =
                    AdvancedSubstationAlphaExporter.class.getDeclaredMethod("getAssTime", Date.class);
            assFormatterMethod.setAccessible(true);

            // get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormatterField.get(ebuParser);

            var ttmlDate = ttmlFormatter.parse("00:03:00.080");
            var assString = (String) assFormatterMethod.invoke(new AdvancedSubstationAlphaExporter(), ttmlDate);
            assertTrue(assString.endsWith(".08"));
        }
    }

    @Test
    void aboveHundred() throws Exception {
        try (TimedTextMarkupLanguageParser parser = new EbuTimedTextMarkupLanguageParser()) {
            // gain access to private fields
            var ttmlFormatterField = TimedTextMarkupLanguageParser.class.getDeclaredField("ttmlFormat");
            ttmlFormatterField.setAccessible(true);
            var assFormatterMethod =
                    AdvancedSubstationAlphaExporter.class.getDeclaredMethod("getAssTime", Date.class);
            assFormatterMethod.setAccessible(true);

            // get the formatters
            SimpleDateFormat ttmlFormatter = (SimpleDateFormat) ttmlFormatterField.get(parser);

            var ttmlDate = ttmlFormatter.parse("00:03:04.400");
            var assString = (String) assFormatterMethod.invoke(new AdvancedSubstationAlphaExporter(), ttmlDate);
            assertTrue(assString.endsWith(".40"));
        }
    }

}