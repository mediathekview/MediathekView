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
import mediathek.tool.ttml.Subtitle;
import mediathek.tool.ttml.TimedTextMarkupLanguageParser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.text.SimpleDateFormat;

public class SubripTextExporter implements ITtmlExporter {
    private static final Logger logger = LogManager.getLogger();
    private final SimpleDateFormat srtFormat = new SimpleDateFormat("HH:mm:ss,SSS");

    @Override
    public void write(TimedTextMarkupLanguageParser parser, Path srtFile) {
        try (FileOutputStream fos = new FileOutputStream(srtFile.toFile());
             OutputStreamWriter osw = new OutputStreamWriter(fos, StandardCharsets.UTF_8);
             PrintWriter writer = new PrintWriter(osw)) {
            long counter = 1;
            for (Subtitle title : parser.getSubtitleList()) {
                writer.println(counter);
                writer.println(srtFormat.format(title.getBegin()) + " --> " + srtFormat.format(title.getEnd()));
                for (var entry : title.getListOfStrings()) {
                    final var entryColor = entry.getColor();
                    if (!entryColor.isEmpty()) {
                        writer.print("<font color=\"" + entryColor + "\">");
                    }
                    writer.print(entry.getText());
                    if (!entryColor.isEmpty()) {
                        writer.print("</font>");
                    }
                    writer.println();
                }
                writer.println("");
                counter++;
            }
            logger.trace("SRT export was succesful.");
        }
        catch (Exception ex) {
            logger.error("File: {}", srtFile, ex);
        }
    }
}
