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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Strict-ish WebVTT validator:
 * <ul>
 *   <li>Requires a WEBVTT header line (after optional UTF-8 BOM)</li>
 *   <li>Allows header metadata lines until the first blank line</li>
 *   <li>Parses NOTE/STYLE/REGION blocks (REGION lightly validated)</li>
 *   <li>Validates cue structure and timestamps</li>
 *   <li>Validates cue settings; unknown keys are errors</li>
 * </ul>
 * <p>
 * Interop note: the validator tolerates {@code align:middle} as a common non-standard alias for {@code align:center}.
 */
public final class WebVttStrictValidator {

    // Timing line: start --> end [settings...]
    private static final Pattern TIMING_LINE =
            Pattern.compile("^\\s*(\\S+)\\s+-->\\s+(\\S+)(?:\\s+(.*))?\\s*$");
    // WebVTT timestamps:
    //   mm:ss.mmm
    //   hh:mm:ss.mmm
    private static final Pattern TS_MM = Pattern.compile("^(\\d{1,2}):(\\d{2})\\.(\\d{3})$");
    private static final Pattern TS_HH = Pattern.compile("^(\\d{1,2}):(\\d{2}):(\\d{2})\\.(\\d{3})$");
    // Spec values plus tolerated alias "middle"
    private static final Set<String> ALIGN = Set.of("start", "center", "end", "left", "right", "middle");
    private static final Set<String> VERTICAL = Set.of("rl", "lr");

    private WebVttStrictValidator() {
    }

    public static ValidationResult validate(Path path, boolean requireAtLeastOneCue) throws IOException {
        List<String> errors = new ArrayList<>();
        int cues = 0;

        try (BufferedReader br = new BufferedReader(
                new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8))) {

            int lineNo = 0;
            String line = br.readLine();
            lineNo++;

            if (line == null) {
                return ValidationResult.fail(false, List.of("Empty file."), 0);
            }

            // Strip UTF-8 BOM on first line if present
            if (!line.isEmpty() && line.charAt(0) == '\uFEFF') {
                line = line.substring(1);
            }

            boolean headerPresent = startsWithWebVttMagic(line);
            if (!isValidHeaderLine(line)) {
                errors.add("Line 1: Missing or invalid WEBVTT header.");
                return ValidationResult.fail(headerPresent, errors, 0);
            }

            // Allow header metadata lines until the first blank line
            boolean inHeaderMetadata = true;

            State state = State.EXPECT_BLOCK_OR_CUE;
            CueBuilder cue = null;

            while ((line = br.readLine()) != null) {
                lineNo++;
                boolean blank = line.isBlank();

                if (inHeaderMetadata) {
                    if (blank) {
                        inHeaderMetadata = false;
                        state = State.EXPECT_BLOCK_OR_CUE;
                    }
                    continue;
                }

                switch (state) {
                    case EXPECT_BLOCK_OR_CUE -> {
                        if (blank)
                            continue;

                        if (line.startsWith("NOTE")) {
                            state = State.IN_NOTE;
                            continue;
                        }
                        if (line.equals("STYLE")) {
                            state = State.IN_STYLE;
                            continue;
                        }
                        if (line.equals("REGION")) {
                            state = State.IN_REGION;
                            continue;
                        }

                        // Cue: identifier or timing line
                        cue = new CueBuilder();
                        if (line.contains("-->")) {
                            if (!parseTimingLine(line, lineNo, cue, errors)) {
                                state = State.SKIP_UNTIL_BLANK;
                            }
                            else {
                                state = State.IN_CUE_PAYLOAD;
                                cues++;
                            }
                        }
                        else {
                            cue.identifier = line;
                            state = State.EXPECT_CUE_TIMING;
                        }
                    }

                    case EXPECT_CUE_TIMING -> {
                        if (blank) {
                            errors.add("Line " + lineNo + ": Unexpected blank line after cue identifier; expected timing line.");
                            state = State.EXPECT_BLOCK_OR_CUE;
                            cue = null;
                            continue;
                        }
                        if (!line.contains("-->")) {
                            errors.add("Line " + lineNo + ": Expected cue timing line containing '-->'.");
                            state = State.SKIP_UNTIL_BLANK;
                            continue;
                        }
                        if (!parseTimingLine(line, lineNo, cue, errors)) {
                            state = State.SKIP_UNTIL_BLANK;
                        }
                        else {
                            state = State.IN_CUE_PAYLOAD;
                            cues++;
                        }
                    }

                    case IN_CUE_PAYLOAD -> {
                        // Cue payload ends at blank line; EOF also ends cue (valid)
                        if (blank) {
                            state = State.EXPECT_BLOCK_OR_CUE;
                            cue = null;
                        }
                    }

                    case IN_NOTE -> {
                        // NOTE runs until blank line
                        if (blank)
                            state = State.EXPECT_BLOCK_OR_CUE;
                    }

                    case IN_STYLE -> {
                        // STYLE runs until blank line (CSS not parsed here)
                        if (blank)
                            state = State.EXPECT_BLOCK_OR_CUE;
                    }

                    case IN_REGION -> {
                        // REGION runs until blank line; validate key:value lines lightly
                        if (blank) {
                            state = State.EXPECT_BLOCK_OR_CUE;
                        }
                        else if (!line.contains(":")) {
                            errors.add("Line " + lineNo + ": REGION block line should contain ':' (key:value).");
                        }
                    }

                    case SKIP_UNTIL_BLANK -> {
                        if (blank) {
                            state = State.EXPECT_BLOCK_OR_CUE;
                            cue = null;
                        }
                    }
                }
            }

            if (state == State.EXPECT_CUE_TIMING) {
                errors.add("EOF: Cue identifier present but no timing line.");
            }

            if (requireAtLeastOneCue && cues == 0) {
                errors.add("No cues found (no timing line with '-->').");
            }

            return errors.isEmpty() ? ValidationResult.ok(cues) : ValidationResult.fail(true, errors, cues);
        }
    }

    /**
     * Returns true if line begins with the WebVTT magic token "WEBVTT" (no leading whitespace).
     */
    private static boolean startsWithWebVttMagic(String line) {
        return line != null && line.startsWith("WEBVTT");
    }

    private static boolean isValidHeaderLine(String line) {
        if (!startsWithWebVttMagic(line))
            return false;
        if (line.length() == 6)
            return true;
        char c = line.charAt(6);
        return c == ' ' || c == '\t';
    }

    private static boolean parseTimingLine(String line, int lineNo, CueBuilder cue, List<String> errors) {
        Matcher m = TIMING_LINE.matcher(line);
        if (!m.matches()) {
            errors.add("Line " + lineNo + ": Invalid timing line syntax.");
            return false;
        }

        String startS = m.group(1);
        String endS = m.group(2);
        String settings = m.group(3);

        Long start = parseTimestampMillis(startS);
        Long end = parseTimestampMillis(endS);

        if (start == null) {
            errors.add("Line " + lineNo + ": Invalid start timestamp: " + startS);
            return false;
        }
        if (end == null) {
            errors.add("Line " + lineNo + ": Invalid end timestamp: " + endS);
            return false;
        }
        if (end <= start) {
            errors.add("Line " + lineNo + ": Cue end must be > start.");
            return false;
        }

        cue.startMs = start;
        cue.endMs = end;

        if (settings != null && !settings.isBlank()) {
            validateSettings(settings, lineNo, errors);
        }

        return true;
    }

    private static Long parseTimestampMillis(String ts) {
        Matcher hh = TS_HH.matcher(ts);
        if (hh.matches()) {
            int h = Integer.parseInt(hh.group(1));
            int m = Integer.parseInt(hh.group(2));
            int s = Integer.parseInt(hh.group(3));
            int ms = Integer.parseInt(hh.group(4));
            if (!validMS(s, ms))
                return null;
            return toMillis(h, m, s, ms);
        }
        Matcher mm = TS_MM.matcher(ts);
        if (mm.matches()) {
            int m = Integer.parseInt(mm.group(1));
            int s = Integer.parseInt(mm.group(2));
            int ms = Integer.parseInt(mm.group(3));
            if (!validMS(s, ms))
                return null;
            return toMillis(0, m, s, ms);
        }
        return null;
    }

    private static boolean validMS(int s, int ms) {
        return s >= 0 && s <= 59 && ms >= 0 && ms <= 999;
    }

    private static long toMillis(int h, int m, int s, int ms) {
        return (((long) h) * 3600L + ((long) m) * 60L + (long) s) * 1000L + (long) ms;
    }

    private static void validateSettings(String settings, int lineNo, List<String> errors) {
        String[] parts = settings.trim().split("\\s+");
        for (String p : parts) {
            int idx = p.indexOf(':');
            if (idx <= 0 || idx == p.length() - 1) {
                errors.add("Line " + lineNo + ": Invalid cue setting '" + p + "' (expected key:value).");
                continue;
            }
            String key = p.substring(0, idx);
            String val = p.substring(idx + 1);

            switch (key) {
                case "align" -> {
                    // tolerate align:middle as alias for center
                    if (!ALIGN.contains(val)) {
                        errors.add("Line " + lineNo + ": Invalid align value '" + val + "'.");
                    }
                }
                case "vertical" -> {
                    if (!VERTICAL.contains(val))
                        errors.add("Line " + lineNo + ": Invalid vertical value '" + val + "'.");
                }
                case "size", "position" -> {
                    String main = val.split(",", 2)[0];
                    if (!isPercent(main))
                        errors.add("Line " + lineNo + ": " + key + " must be a percentage, got '" + val + "'.");
                }
                case "line" -> {
                    String[] lp = val.split(",", 2);
                    String main = lp[0];
                    boolean ok = isPercent(main) || isInt(main);
                    if (!ok)
                        errors.add("Line " + lineNo + ": line must be an integer or percentage, got '" + val + "'.");
                    if (lp.length == 2 && !ALIGN.contains(lp[1])) {
                        errors.add("Line " + lineNo + ": line alignment must be start|center|end|left|right, got '" + lp[1] + "'.");
                    }
                }
                case "region" -> {
                    if (val.isBlank() || val.contains(" "))
                        errors.add("Line " + lineNo + ": region must be a non-empty token, got '" + val + "'.");
                }
                default -> errors.add("Line " + lineNo + ": Unknown cue setting key '" + key + "'.");
            }
        }
    }

    private static boolean isInt(String s) {
        return s.matches("^-?\\d+$");
    }

    private static boolean isPercent(String s) {
        if (!s.endsWith("%"))
            return false;
        String num = s.substring(0, s.length() - 1);
        if (!num.matches("^\\d{1,3}(?:\\.\\d+)?$"))
            return false;
        double v = Double.parseDouble(num);
        return v >= 0.0 && v <= 100.0;
    }

    private enum State {
        EXPECT_BLOCK_OR_CUE,
        IN_NOTE,
        IN_STYLE,
        IN_REGION,
        EXPECT_CUE_TIMING,
        IN_CUE_PAYLOAD,
        SKIP_UNTIL_BLANK
    }

    public record ValidationResult(boolean headerPresent, boolean valid, List<String> errors, int cueCount) {
        public static ValidationResult ok(int cueCount) {
            return new ValidationResult(true, true, List.of(), cueCount);
        }

        public static ValidationResult fail(boolean headerPresent, List<String> errors, int cueCount) {
            return new ValidationResult(headerPresent, false, List.copyOf(errors), cueCount);
        }
    }

    private static final class CueBuilder {
        String identifier;
        long startMs;
        long endMs;
    }
}
