/*
 * Copyright (c) 2026 derreisende77.
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

package mediathek.tool.subtitles.detector;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

/**
 * Detects timed-text file formats:
 * - WebVTT (header-based detection; strict validation provided in result)
 * - TTML1 vs TTML2 (TTML2 detected via vocabulary markers; otherwise TTML1)
 */
public final class TimedTextFormatDetector {

    private static final String TTML_NS = "http://www.w3.org/ns/ttml";
    private static final Set<String> TTML2_ELEMENTS = Set.of(
            "animate",
            "animation",
            "audio",
            "chunk",
            "data",
            "font",
            "image",
            "resources",
            "source",
            "initial"
    );
    private static final Set<String> TTML2_ATTRIBUTES = Set.of(
            "condition",
            "xml:base",
            "tts:backgroundImage",
            "tts:backgroundPosition",
            "tts:ruby",
            "tts:textShadow",
            "tts:textEmphasis"
    );

    private TimedTextFormatDetector() {
    }

    /**
     * @param requireAtLeastOneVttCue if true, WebVTT must contain at least one cue.
     */
    public static Result detect(Path path, boolean requireAtLeastOneVttCue) throws Exception {
        // 1) WebVTT: header-based detection + strict validation result
        var vtt = WebVttStrictValidator.validate(path, requireAtLeastOneVttCue);
        if (vtt.headerPresent()) {
            if (vtt.valid()) {
                return Result.ok(Format.WEBVTT, "Valid WebVTT, cues=" + vtt.cueCount());
            }
            else {
                return Result.fail(Format.WEBVTT, "Invalid WebVTT: " + String.join(" | ", vtt.errors()));
            }
        }

        // 2) TTML sniff (XML/StAX)
        var ttml = detectTtmlVersion(path);
        return switch (ttml) {
            case TTML1 -> Result.ok(Format.TTML1, "TTML root detected; no TTML2 markers found.");
            case TTML2 -> Result.ok(Format.TTML2, "TTML root detected; TTML2 marker(s) found.");
            case NOT_TTML -> Result.fail(Format.UNKNOWN, "Not WebVTT, and XML root is not TTML <tt>.");
        };
    }

    private static TtmlVersion detectTtmlVersion(Path path) {
        try (InputStream in = Files.newInputStream(path)) {
            XMLInputFactory factory = XMLInputFactory.newFactory();
            factory.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE, true);

            var reader = factory.createXMLStreamReader(in);

            boolean rootChecked = false;

            while (reader.hasNext()) {
                int event = reader.next();

                if (event == XMLStreamConstants.START_ELEMENT) {
                    String localName = reader.getLocalName();
                    String namespace = reader.getNamespaceURI();

                    if (!rootChecked) {
                        rootChecked = true;
                        if (!"tt".equals(localName) || !TTML_NS.equals(namespace)) {
                            return TtmlVersion.NOT_TTML;
                        }
                    }

                    if (TTML2_ELEMENTS.contains(localName)) {
                        return TtmlVersion.TTML2;
                    }

                    for (int i = 0; i < reader.getAttributeCount(); i++) {
                        String attrLocal = reader.getAttributeLocalName(i);
                        String attrPrefix = reader.getAttributePrefix(i);
                        String qualified = (attrPrefix == null || attrPrefix.isEmpty())
                                ? attrLocal
                                : attrPrefix + ":" + attrLocal;

                        if (TTML2_ATTRIBUTES.contains(qualified) || TTML2_ATTRIBUTES.contains(attrLocal)) {
                            return TtmlVersion.TTML2;
                        }
                    }
                }
            }

            return rootChecked ? TtmlVersion.TTML1 : TtmlVersion.NOT_TTML;
        }
        catch (Exception e) {
            return TtmlVersion.NOT_TTML;
        }
    }

    public enum Format {
        WEBVTT,
        TTML1,
        TTML2,
        UNKNOWN
    }

    private enum TtmlVersion {TTML1, TTML2, NOT_TTML}

    public record Result(Format format, boolean valid, String details) {
        public static Result ok(Format format, String details) {
            return new Result(format, true, details);
        }

        public static Result fail(Format format, String details) {
            return new Result(format, false, details);
        }
    }
}
