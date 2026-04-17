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

package mediathek.tool.subtitles.detector

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.util.regex.Pattern

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
object WebVttStrictValidator {
    // Timing line: start --> end [settings...]
    private val TIMING_LINE: Pattern = Pattern.compile("^\\s*(\\S+)\\s+-->\\s+(\\S+)(?:\\s+(.*))?\\s*$")

    // WebVTT timestamps:
    //   mm:ss.mmm
    //   hh:mm:ss.mmm
    private val TS_MM: Pattern = Pattern.compile("^(\\d{1,2}):(\\d{2})\\.(\\d{3})$")
    private val TS_HH: Pattern = Pattern.compile("^(\\d{1,2}):(\\d{2}):(\\d{2})\\.(\\d{3})$")
    private val INTEGER = Regex("^-?\\d+$")
    private val PERCENT = Regex("^\\d{1,3}(?:\\.\\d+)?$")

    // Spec values plus tolerated alias "middle"
    private val ALIGN = setOf("start", "center", "end", "left", "right", "middle")
    private val VERTICAL = setOf("rl", "lr")

    @JvmStatic
    @Throws(IOException::class)
    fun validate(path: Path, requireAtLeastOneCue: Boolean): ValidationResult {
        val errors = mutableListOf<String>()
        var cues = 0

        BufferedReader(InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8)).use { br ->
            var lineNo = 1
            var line = br.readLine() ?: return ValidationResult.fail(false, listOf("Empty file."), 0)

            // Strip UTF-8 BOM on first line if present
            if (line.isNotEmpty() && line[0] == '\uFEFF') {
                line = line.substring(1)
            }

            val headerPresent = startsWithWebVttMagic(line)
            if (!isValidHeaderLine(line)) {
                errors.add("Line 1: Missing or invalid WEBVTT header.")
                return ValidationResult.fail(headerPresent, errors, 0)
            }

            // Allow header metadata lines until the first blank line
            var inHeaderMetadata = true

            var state = State.EXPECT_BLOCK_OR_CUE

            while (true) {
                line = br.readLine() ?: break
                lineNo++
                val blank = line.isBlank()

                if (inHeaderMetadata) {
                    if (blank) {
                        inHeaderMetadata = false
                        state = State.EXPECT_BLOCK_OR_CUE
                    }
                    continue
                }

                when (state) {
                    State.EXPECT_BLOCK_OR_CUE -> {
                        if (blank) {
                            continue
                        }

                        if (line.startsWith("NOTE")) {
                            state = State.IN_NOTE
                            continue
                        }
                        if (line == "STYLE") {
                            state = State.IN_STYLE
                            continue
                        }
                        if (line == "REGION") {
                            state = State.IN_REGION
                            continue
                        }

                        // Cue: identifier or timing line
                        if (line.contains("-->")) {
                            if (!parseTimingLine(line, lineNo, errors)) {
                                state = State.SKIP_UNTIL_BLANK
                            } else {
                                state = State.IN_CUE_PAYLOAD
                                cues++
                            }
                        } else {
                            state = State.EXPECT_CUE_TIMING
                        }
                    }

                    State.EXPECT_CUE_TIMING -> {
                        if (blank) {
                            errors.add("Line $lineNo: Unexpected blank line after cue identifier; expected timing line.")
                            state = State.EXPECT_BLOCK_OR_CUE
                            continue
                        }
                        if (!line.contains("-->")) {
                            errors.add("Line $lineNo: Expected cue timing line containing '-->'.")
                            state = State.SKIP_UNTIL_BLANK
                            continue
                        }
                        if (!parseTimingLine(line, lineNo, errors)) {
                            state = State.SKIP_UNTIL_BLANK
                        } else {
                            state = State.IN_CUE_PAYLOAD
                            cues++
                        }
                    }

                    State.IN_CUE_PAYLOAD,
                    State.IN_NOTE,
                    State.IN_STYLE,
                    State.SKIP_UNTIL_BLANK -> {
                        if (blank) {
                            state = State.EXPECT_BLOCK_OR_CUE
                        }
                    }

                    State.IN_REGION -> {
                        // REGION runs until blank line; validate key:value lines lightly
                        if (blank) {
                            state = State.EXPECT_BLOCK_OR_CUE
                        } else if (!line.contains(":")) {
                            errors.add("Line $lineNo: REGION block line should contain ':' (key:value).")
                        }
                    }
                }
            }

            if (state == State.EXPECT_CUE_TIMING) {
                errors.add("EOF: Cue identifier present but no timing line.")
            }

            if (requireAtLeastOneCue && cues == 0) {
                errors.add("No cues found (no timing line with '-->').")
            }

            return if (errors.isEmpty()) {
                ValidationResult.ok(cues)
            } else {
                ValidationResult.fail(true, errors, cues)
            }
        }
    }

    /**
     * Returns true if line begins with the WebVTT magic token "WEBVTT" (no leading whitespace).
     */
    private fun startsWithWebVttMagic(line: String): Boolean {
        return line.startsWith("WEBVTT")
    }

    private fun isValidHeaderLine(line: String): Boolean {
        if (!startsWithWebVttMagic(line)) {
            return false
        }
        if (line.length == 6) {
            return true
        }
        val c = line[6]
        return c == ' ' || c == '\t'
    }

    private fun parseTimingLine(line: String, lineNo: Int, errors: MutableList<String>): Boolean {
        val m = TIMING_LINE.matcher(line)
        if (!m.matches()) {
            errors.add("Line $lineNo: Invalid timing line syntax.")
            return false
        }

        val startS = m.group(1)
        val endS = m.group(2)
        val settings = m.group(3)

        val start = parseTimestampMillis(startS)
        val end = parseTimestampMillis(endS)

        if (start == null) {
            errors.add("Line $lineNo: Invalid start timestamp: $startS")
            return false
        }
        if (end == null) {
            errors.add("Line $lineNo: Invalid end timestamp: $endS")
            return false
        }
        if (end <= start) {
            errors.add("Line $lineNo: Cue end must be > start.")
            return false
        }

        if (settings != null && settings.isNotBlank()) {
            validateSettings(settings, lineNo, errors)
        }

        return true
    }

    private fun parseTimestampMillis(ts: String): Long? {
        val hh = TS_HH.matcher(ts)
        if (hh.matches()) {
            val h = hh.group(1).toInt()
            val m = hh.group(2).toInt()
            val s = hh.group(3).toInt()
            val ms = hh.group(4).toInt()
            if (!validMS(s, ms)) {
                return null
            }
            return toMillis(h, m, s, ms)
        }

        val mm = TS_MM.matcher(ts)
        if (mm.matches()) {
            val m = mm.group(1).toInt()
            val s = mm.group(2).toInt()
            val ms = mm.group(3).toInt()
            if (!validMS(s, ms)) {
                return null
            }
            return toMillis(0, m, s, ms)
        }

        return null
    }

    private fun validMS(s: Int, ms: Int): Boolean {
        return s in 0..59 && ms in 0..999
    }

    private fun toMillis(h: Int, m: Int, s: Int, ms: Int): Long {
        return (h * 3_600L + m * 60L + s) * 1_000L + ms
    }

    private fun validateSettings(settings: String, lineNo: Int, errors: MutableList<String>) {
        val parts = settings.trim().split("\\s+".toRegex())
        for (p in parts) {
            val idx = p.indexOf(':')
            if (idx <= 0 || idx == p.length - 1) {
                errors.add("Line $lineNo: Invalid cue setting '$p' (expected key:value).")
                continue
            }
            val key = p.substring(0, idx)
            val value = p.substring(idx + 1)

            when (key) {
                "align" -> {
                    // tolerate align:middle as alias for center
                    if (value !in ALIGN) {
                        errors.add("Line $lineNo: Invalid align value '$value'.")
                    }
                }

                "vertical" -> {
                    if (value !in VERTICAL) {
                        errors.add("Line $lineNo: Invalid vertical value '$value'.")
                    }
                }

                "size", "position" -> {
                    val main = value.split(",", limit = 2)[0]
                    if (!isPercent(main)) {
                        errors.add("Line $lineNo: $key must be a percentage, got '$value'.")
                    }
                }

                "line" -> {
                    val lp = value.split(",", limit = 2)
                    val main = lp[0]
                    val ok = isPercent(main) || isInt(main)
                    if (!ok) {
                        errors.add("Line $lineNo: line must be an integer or percentage, got '$value'.")
                    }
                    if (lp.size == 2 && lp[1] !in ALIGN) {
                        errors.add("Line $lineNo: line alignment must be start|center|end|left|right, got '${lp[1]}'.")
                    }
                }

                "region" -> {
                    if (value.isBlank() || value.contains(" ")) {
                        errors.add("Line $lineNo: region must be a non-empty token, got '$value'.")
                    }
                }

                else -> errors.add("Line $lineNo: Unknown cue setting key '$key'.")
            }
        }
    }

    private fun isInt(s: String): Boolean {
        return s.matches(INTEGER)
    }

    private fun isPercent(s: String): Boolean {
        if (!s.endsWith("%")) {
            return false
        }
        val num = s.substring(0, s.length - 1)
        if (!num.matches(PERCENT)) {
            return false
        }
        val v = num.toDouble()
        return v in 0.0..100.0
    }

    private enum class State {
        EXPECT_BLOCK_OR_CUE,
        IN_NOTE,
        IN_STYLE,
        IN_REGION,
        EXPECT_CUE_TIMING,
        IN_CUE_PAYLOAD,
        SKIP_UNTIL_BLANK
    }

    data class ValidationResult(
        val headerPresent: Boolean,
        val valid: Boolean,
        val errors: List<String>,
        val cueCount: Int
    ) {
        companion object {
            fun ok(cueCount: Int): ValidationResult {
                return ValidationResult(headerPresent = true, valid = true, errors = emptyList(), cueCount = cueCount)
            }

            fun fail(headerPresent: Boolean, errors: List<String>, cueCount: Int): ValidationResult {
                return ValidationResult(headerPresent, false, errors.toList(), cueCount)
            }
        }
    }
}
