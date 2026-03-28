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

package mediathek.tool.subtitles.vtt;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Reader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.*;

/**
 * WebVTT -> TTML2 converter:
 * - Parses cue settings on timing line and maps:
 *   - align:start|middle|center|end|left|right -> tts:textAlign on <p>
 * - Preserves WebVTT <c.class> ... </c> spans by mapping known classes to TTML <span style="...">
 *   - Known: textWhite, textYellow, textCyan (common broadcaster palettes)
 * Intentionally not implemented:
 * - REGION blocks and mapping of line/position/size to TTML regions
 * - STYLE blocks, full CSS resolution
 * - Ruby (<ruby>/<rt>), voice (<v>), lang (<lang>), etc. (stripped, content kept)
 */
public final class WebVttToTtml2Converter {

    private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;

    private static final Map<String, String> VTT_CLASS_TO_TTML_STYLE = Map.of(
            "textWhite", "cTextWhite",
            "textYellow", "cTextYellow",
            "textCyan", "cTextCyan"
    );

    public void convert(Path webVttPath, Path ttml2OutPath) throws IOException {
        Objects.requireNonNull(webVttPath, "webVttPath");
        Objects.requireNonNull(ttml2OutPath, "ttml2OutPath");

        try (BufferedReader br = Files.newBufferedReader(webVttPath, DEFAULT_CHARSET);
             BufferedWriter bw = Files.newBufferedWriter(ttml2OutPath, DEFAULT_CHARSET)) {

            List<Cue> cues = parseWebVtt(br);
            String xml = toTtml2(cues);
            bw.write(xml);
            bw.flush();
        }
    }

    private List<Cue> parseWebVtt(Reader reader) throws IOException {
        BufferedReader br = (reader instanceof BufferedReader) ? (BufferedReader) reader : new BufferedReader(reader);

        String first = br.readLine();
        if (first == null) throw new VttParseException("Empty file: missing WEBVTT header.");
        if (!first.startsWith("WEBVTT")) throw new VttParseException("Invalid header. First line must start with 'WEBVTT'. Found: " + first);

        String line;
        boolean sawBlankAfterHeader = false;

        while ((line = br.readLine()) != null) {
            if (line.isEmpty()) {
                sawBlankAfterHeader = true;
                break;
            }
            if (line.startsWith("NOTE")) {
                consumeUntilBlank(br);
                sawBlankAfterHeader = true;
                break;
            }
            if (line.equals("STYLE") || line.equals("REGION")) {
                consumeUntilBlank(br);
            }
            // other metadata lines ignored
        }

        if (!sawBlankAfterHeader) {
            throw new VttParseException("Missing blank line after WEBVTT header/metadata.");
        }

        List<Cue> cues = new ArrayList<>();
        String pending = null;

        while (true) {
            String l = (pending != null) ? pending : br.readLine();
            pending = null;
            if (l == null) break;

            if (l.isEmpty()) continue;

            if (l.startsWith("NOTE")) {
                consumeUntilBlank(br);
                continue;
            }
            if (l.equals("STYLE") || l.equals("REGION")) {
                consumeUntilBlank(br);
                continue;
            }

            String id = null;
            String timingLine = l;
            if (!containsArrow(timingLine)) {
                String next = br.readLine();
                if (next == null) throw new VttParseException("Unexpected EOF after cue identifier: " + timingLine);
                if (!containsArrow(next)) throw new VttParseException("Expected timing line after identifier '" + timingLine + "', found: " + next);
                id = timingLine;
                timingLine = next;
            }

            Timing timing = parseTimingLine(timingLine);

            List<String> payloadLines = new ArrayList<>();
            while ((line = br.readLine()) != null) {
                if (line.isEmpty()) break;
                payloadLines.add(line);
            }

            cues.add(new Cue(id, timing.begin, timing.end, timing.settings, payloadLines));
        }

        return cues;
    }

    private static void consumeUntilBlank(BufferedReader br) throws IOException {
        String l;
        while ((l = br.readLine()) != null) {
            if (l.isEmpty()) return;
        }
    }

    private static boolean containsArrow(String line) {
        return line.contains("-->");
    }

    private Timing parseTimingLine(String line) {
        String trimmed = line.trim();
        int arrow = trimmed.indexOf("-->");
        if (arrow < 0) throw new VttParseException("Invalid timing line (missing -->): " + line);

        String left = trimmed.substring(0, arrow).trim();
        String rightAndSettings = trimmed.substring(arrow + 3).trim();

        String endTs;
        String settingsPart = "";
        int ws = indexOfWhitespace(rightAndSettings);
        if (ws < 0) {
            endTs = rightAndSettings;
        } else {
            endTs = rightAndSettings.substring(0, ws).trim();
            settingsPart = rightAndSettings.substring(ws).trim();
        }

        Duration begin = parseVttTimestamp(left);
        Duration end = parseVttTimestamp(endTs);

        if (!(end.compareTo(begin) > 0)) {
            throw new VttParseException("Invalid cue timing: end <= begin in line: " + line);
        }

        Map<String, String> settings = parseCueSettings(settingsPart);
        return new Timing(begin, end, settings);
    }

    private static Map<String, String> parseCueSettings(String settingsPart) {
        Map<String, String> m = new HashMap<>();
        if (settingsPart == null || settingsPart.isBlank()) return m;

        // tokens like: "align:middle position:50% line:90% size:80%"
        String[] tokens = settingsPart.trim().split("\\s+");
        for (String tok : tokens) {
            int colon = tok.indexOf(':');
            if (colon <= 0 || colon == tok.length() - 1) continue;
            String k = tok.substring(0, colon).trim();
            String v = tok.substring(colon + 1).trim();
            if (!k.isEmpty() && !v.isEmpty()) m.put(k, v);
        }
        return m;
    }

    private static int indexOfWhitespace(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (Character.isWhitespace(s.charAt(i))) return i;
        }
        return -1;
    }

    private static Duration parseVttTimestamp(String ts) {
        String t = ts.trim();
        int dot = t.indexOf('.');
        if (dot < 0) throw new VttParseException("Invalid timestamp (missing .mmm): " + ts);

        String hms = t.substring(0, dot);
        String msStr = t.substring(dot + 1);

        if (msStr.length() != 3 || !isAllDigits(msStr)) throw new VttParseException("Invalid milliseconds in timestamp: " + ts);
        int ms = Integer.parseInt(msStr);

        String[] parts = hms.split(":");
        int hours = 0, minutes, seconds;

        try {
            if (parts.length == 3) {
                hours = parseHours(parts[0], ts);
                minutes = parse2Digits(parts[1], "minutes", ts);
                seconds = parse2Digits(parts[2], "seconds", ts);
            } else if (parts.length == 2) {
                minutes = parse2Digits(parts[0], "minutes", ts);
                seconds = parse2Digits(parts[1], "seconds", ts);
            } else {
                throw new VttParseException("Invalid timestamp format (expected MM:SS.mmm or HH:MM:SS.mmm): " + ts);
            }
        } catch (NumberFormatException ex) {
            throw new VttParseException("Invalid numeric timestamp: " + ts, ex);
        }

        if (minutes < 0 || minutes > 59) throw new VttParseException("Minutes out of range in timestamp: " + ts);
        if (seconds < 0 || seconds > 59) throw new VttParseException("Seconds out of range in timestamp: " + ts);

        return Duration.ofHours(hours).plusMinutes(minutes).plusSeconds(seconds).plusMillis(ms);
    }

    private static int parseHours(String s, String fullTs) {
        if (s.length() < 2 || !isAllDigits(s)) throw new VttParseException("Invalid hours in timestamp: " + fullTs);
        return Integer.parseInt(s);
    }

    private static int parse2Digits(String s, String field, String fullTs) {
        if (s.length() != 2 || !isAllDigits(s)) throw new VttParseException("Invalid " + field + " in timestamp: " + fullTs);
        return Integer.parseInt(s);
    }

    private static boolean isAllDigits(String s) {
        for (int i = 0; i < s.length(); i++) if (!Character.isDigit(s.charAt(i))) return false;
        return true;
    }

    private String toTtml2(List<Cue> cues) {
        StringBuilder sb = new StringBuilder(32 * 1024);

        sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        sb.append("<tt\n");
        sb.append("  xmlns=\"http://www.w3.org/ns/ttml\"\n");
        sb.append("  xmlns:ttm=\"http://www.w3.org/ns/ttml#metadata\"\n");
        sb.append("  xmlns:tts=\"http://www.w3.org/ns/ttml#styling\"\n");
        sb.append("  xmlns:ttp=\"http://www.w3.org/ns/ttml#parameter\"\n");
        sb.append("  xml:lang=\"en\"\n");
        sb.append("  ttp:timeBase=\"media\"\n");
        sb.append("  ttp:frameRate=\"30\">\n");

        sb.append("  <head>\n");
        sb.append("    <styling>\n");
        sb.append("      <style xml:id=\"s0\" tts:fontFamily=\"sansSerif\" tts:fontSize=\"1c\"/>\n");

        // Emit known class styles
        sb.append("      <style xml:id=\"cTextWhite\" tts:color=\"#FFFFFF\"/>\n");
        sb.append("      <style xml:id=\"cTextYellow\" tts:color=\"#FFFF00\"/>\n");
        sb.append("      <style xml:id=\"cTextCyan\" tts:color=\"#00FFFF\"/>\n");

        sb.append("    </styling>\n");
        sb.append("    <layout>\n");
        sb.append("      <region xml:id=\"r0\"/>\n");
        sb.append("    </layout>\n");
        sb.append("  </head>\n");

        sb.append("  <body style=\"s0\" region=\"r0\">\n");
        sb.append("    <div>\n");

        for (Cue cue : cues) {
            String begin = formatTtmlTime(cue.begin);
            String end = formatTtmlTime(cue.end);

            sb.append("      <p");
            sb.append(" begin=\"").append(begin).append("\"");
            sb.append(" end=\"").append(end).append("\"");

            if (cue.id != null && !cue.id.isBlank()) {
                sb.append(" xml:id=\"").append(xmlIdSafe(cue.id)).append("\"");
            }

            // Map align to tts:textAlign
            String textAlign = mapAlignToTtml(cue.settings.get("align"));
            if (textAlign != null) {
                sb.append(" tts:textAlign=\"").append(textAlign).append("\"");
            }

            sb.append(">");

            String joined = String.join("\n", cue.payloadLines);
            sb.append(convertInlineMarkup(joined));

            sb.append("</p>\n");
        }

        sb.append("    </div>\n");
        sb.append("  </body>\n");
        sb.append("</tt>\n");

        return sb.toString();
    }

    private static String mapAlignToTtml(String vttAlign) {
        if (vttAlign == null) return null;
        return switch (vttAlign) {
            case "start", "left" -> "start";
            case "middle", "center" -> "center";
            case "end", "right" -> "end";
            default -> null;
        };
    }

    private static String formatTtmlTime(Duration d) {
        long totalMillis = d.toMillis();
        long hours = totalMillis / 3_600_000;
        long rem = totalMillis % 3_600_000;
        long minutes = rem / 60_000;
        rem %= 60_000;
        long seconds = rem / 1_000;
        long millis = rem % 1_000;
        return String.format("%02d:%02d:%02d.%03d", hours, minutes, seconds, millis);
    }

    private static String xmlIdSafe(String s) {
        String trimmed = s.trim();
        if (trimmed.isEmpty()) return "cue";
        StringBuilder out = new StringBuilder(trimmed.length() + 1);
        char first = trimmed.charAt(0);
        if (!isNameStart(first)) out.append('c');
        for (int i = 0; i < trimmed.length(); i++) {
            char ch = trimmed.charAt(i);
            out.append(isNameChar(ch) ? ch : '_');
        }
        return out.toString();
    }

    private static boolean isNameStart(char ch) {
        return Character.isLetter(ch) || ch == '_' || ch == ':';
    }

    private static boolean isNameChar(char ch) {
        return isNameStart(ch) || Character.isDigit(ch) || ch == '-' || ch == '.';
    }

    /**
     * Converts basic WebVTT inline tags into TTML spans.
     * - \n -> <br/>
     * - <b>/<i>/<u> -> TTML <span> with tts:* styling
     * - <c.class> -> TTML <span style="..."> for known classes (textWhite/textYellow/textCyan)
     *   Unknown classes are stripped (content kept).
     * - Timestamp tags in cue text like <00:00:01.000> are stripped.
     * - Other tags stripped.
     */
    private static String convertInlineMarkup(String webVttText) {
        String s = webVttText == null ? "" : webVttText;

        StringBuilder out = new StringBuilder(s.length() + 64);
        Deque<OpenSpan> stack = new ArrayDeque<>();

        int i = 0;
        while (i < s.length()) {
            char ch = s.charAt(i);

            if (ch == '\n') {
                out.append("<br/>");
                i++;
                continue;
            }

            if (ch == '<') {
                int gt = s.indexOf('>', i + 1);
                if (gt < 0) {
                    out.append("&lt;");
                    i++;
                    continue;
                }

                String raw = s.substring(i + 1, gt).trim();

                if (looksLikeVttTimestampTag(raw)) {
                    i = gt + 1;
                    continue;
                }

                boolean closing = raw.startsWith("/");
                String inner = closing ? raw.substring(1).trim() : raw;

                TagInfo tag = parseTag(inner);

                switch (tag.name) {
                    case "b" -> {
                        if (!closing) {
                            stack.push(OpenSpan.kind(SpanKind.BOLD));
                            out.append("<span tts:fontWeight=\"bold\">");
                        } else {
                            closeSpan(stack, out, OpenSpan.kind(SpanKind.BOLD));
                        }
                    }
                    case "i" -> {
                        if (!closing) {
                            stack.push(OpenSpan.kind(SpanKind.ITALIC));
                            out.append("<span tts:fontStyle=\"italic\">");
                        } else {
                            closeSpan(stack, out, OpenSpan.kind(SpanKind.ITALIC));
                        }
                    }
                    case "u" -> {
                        if (!closing) {
                            stack.push(OpenSpan.kind(SpanKind.UNDERLINE));
                            out.append("<span tts:textDecoration=\"underline\">");
                        } else {
                            closeSpan(stack, out, OpenSpan.kind(SpanKind.UNDERLINE));
                        }
                    }
                    case "br" -> out.append("<br/>");
                    case "c" -> {
                        // WebVTT class tag: <c.class1.class2> ... </c>
                        if (!closing) {
                            String styleId = mapClassesToStyleId(tag.classes);
                            if (styleId != null) {
                                OpenSpan os = OpenSpan.classStyle(styleId);
                                stack.push(os);
                                out.append("<span style=\"").append(styleId).append("\">");
                            } else {
                                // unknown class -> ignore tag (do not push)
                            }
                        } else {
                            // close most recent CLASS span (if any)
                            closeSpan(stack, out, OpenSpan.anyClass());
                        }
                    }
                    default -> {
                        // strip unknown tag (content kept)
                    }
                }

                i = gt + 1;
                continue;
            }

            switch (ch) {
                case '&' -> out.append("&amp;");
                case '<' -> out.append("&lt;");
                case '>' -> out.append("&gt;");
                case '"' -> out.append("&quot;");
                case '\'' -> out.append("&apos;");
                default -> out.append(ch);
            }

            i++;
        }

        while (!stack.isEmpty()) {
            stack.pop();
            out.append("</span>");
        }

        return out.toString();
    }

    private static String mapClassesToStyleId(List<String> classes) {
        if (classes == null || classes.isEmpty()) return null;
        // pick the first known class
        for (String c : classes) {
            String id = VTT_CLASS_TO_TTML_STYLE.get(c);
            if (id != null) return id;
        }
        return null;
    }

    private static void closeSpan(Deque<OpenSpan> stack, StringBuilder out, OpenSpan target) {
        // Best-effort close: if mis-nested, close until match then re-open.
        if (stack.isEmpty()) return;

        Deque<OpenSpan> tmp = new ArrayDeque<>();
        boolean found = false;

        while (!stack.isEmpty()) {
            OpenSpan top = stack.pop();
            out.append("</span>");
            if (target.matches(top)) {
                found = true;
                break;
            }
            tmp.push(top);
        }

        while (!tmp.isEmpty()) {
            OpenSpan st = tmp.pop();
            stack.push(st);
            out.append(st.openTag());
        }

        @SuppressWarnings("unused")
        boolean ignored = found;
    }

    private static boolean looksLikeVttTimestampTag(String tagContent) {
        String t = tagContent.trim();
        if (t.isEmpty() || t.indexOf(' ') >= 0 || t.startsWith("/")) return false;
        int dot = t.indexOf('.');
        if (dot < 0) return false;
        String left = t.substring(0, dot);
        String right = t.substring(dot + 1);
        if (right.length() != 3 || !isAllDigits(right)) return false;
        String[] parts = left.split(":");
        if (parts.length != 2 && parts.length != 3) return false;
        for (String p : parts) if (!isAllDigits(p)) return false;
        return true;
    }

    private static TagInfo parseTag(String inner) {
        // inner examples:
        // "b"
        // "c.textWhite"
        // "c.textWhite.textBold"
        // "v Fred" (we'll parse name=v, classes empty)
        // "lang en" (name=lang)
        String s = inner.trim();
        int sp = s.indexOf(' ');
        String head = (sp >= 0) ? s.substring(0, sp).trim() : s;

        String name;
        List<String> classes = List.of();

        int dot = head.indexOf('.');
        if (dot >= 0) {
            name = head.substring(0, dot).trim();
            String rest = head.substring(dot + 1).trim();
            if (!rest.isEmpty()) {
                String[] cls = rest.split("\\.");
                List<String> out = new ArrayList<>(cls.length);
                for (String c : cls) {
                    String cc = c.trim();
                    if (!cc.isEmpty()) out.add(cc);
                }
                classes = out;
            }
        } else {
            name = head;
        }

        return new TagInfo(name, classes);
    }

    private record TagInfo(String name, List<String> classes) {}

    private enum SpanKind { BOLD, ITALIC, UNDERLINE }

    private static final class OpenSpan {
        private final SpanKind kind;       // for B/I/U
        private final String classStyleId; // for <c.*> mapping
        private final boolean anyClassCloseMarker;

        private OpenSpan(SpanKind kind, String classStyleId, boolean anyClassCloseMarker) {
            this.kind = kind;
            this.classStyleId = classStyleId;
            this.anyClassCloseMarker = anyClassCloseMarker;
        }

        static OpenSpan kind(SpanKind kind) { return new OpenSpan(Objects.requireNonNull(kind), null, false); }

        static OpenSpan classStyle(String styleId) { return new OpenSpan(null, Objects.requireNonNull(styleId), false); }

        static OpenSpan anyClass() { return new OpenSpan(null, null, true); }

        boolean matches(OpenSpan other) {
            if (this.anyClassCloseMarker) {
                return other.classStyleId != null;
            }
            if (this.kind != null) {
                return other.kind == this.kind;
            }
            if (this.classStyleId != null) {
                return Objects.equals(other.classStyleId, this.classStyleId);
            }
            return false;
        }

        String openTag() {
            if (kind != null) {
                return switch (kind) {
                    case BOLD -> "<span tts:fontWeight=\"bold\">";
                    case ITALIC -> "<span tts:fontStyle=\"italic\">";
                    case UNDERLINE -> "<span tts:textDecoration=\"underline\">";
                };
            }
            if (classStyleId != null) {
                return "<span style=\"" + classStyleId + "\">";
            }
            return "<span>";
        }
    }

    private record Cue(String id, Duration begin, Duration end, Map<String, String> settings, List<String> payloadLines) {}
    private record Timing(Duration begin, Duration end, Map<String, String> settings) {}

    public static final class VttParseException extends IllegalArgumentException {
        public VttParseException(String message) { super(message); }
        public VttParseException(String message, Throwable cause) { super(message, cause); }
    }
}