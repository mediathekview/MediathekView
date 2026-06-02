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

package mediathek.tool.subtitles.vtt

import mediathek.tool.HtmlUtils
import java.io.BufferedReader
import java.io.Reader
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration
import java.util.ArrayDeque

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
class WebVttToTtml2Converter {
    fun convert(webVttPath: Path, ttml2OutPath: Path) {
        Files.writeString(ttml2OutPath, convertToString(webVttPath), DEFAULT_CHARSET)
    }

    fun convertToString(webVttPath: Path): String =
        Files.newBufferedReader(webVttPath, DEFAULT_CHARSET).use(::convertToString)

    fun convertToString(reader: Reader): String = toTtml2(parseWebVtt(reader))

    private fun parseWebVtt(reader: Reader): List<Cue> {
        val bufferedReader = reader as? BufferedReader ?: BufferedReader(reader)

        val first = bufferedReader.readLine() ?: throw VttParseException("Empty file: missing WEBVTT header.")
        if (!first.startsWith("WEBVTT")) {
            throw VttParseException("Invalid header. First line must start with 'WEBVTT'. Found: $first")
        }

        var sawBlankAfterHeader = false
        while (true) {
            val line = bufferedReader.readLine() ?: break
            when {
                line.isEmpty() -> {
                    sawBlankAfterHeader = true
                    break
                }

                line.startsWith("NOTE") -> {
                    consumeUntilBlank(bufferedReader)
                    sawBlankAfterHeader = true
                    break
                }

                line == "STYLE" || line == "REGION" -> consumeUntilBlank(bufferedReader)
            }
        }

        if (!sawBlankAfterHeader) {
            throw VttParseException("Missing blank line after WEBVTT header/metadata.")
        }

        val cues = ArrayList<Cue>()
        while (true) {
            val line = bufferedReader.readLine() ?: break
            if (line.isEmpty()) {
                continue
            }

            if (line.startsWith("NOTE")) {
                consumeUntilBlank(bufferedReader)
                continue
            }
            if (line == "STYLE" || line == "REGION") {
                consumeUntilBlank(bufferedReader)
                continue
            }

            var id: String? = null
            var timingLine = line
            if (!timingLine.containsArrow()) {
                val next = bufferedReader.readLine()
                    ?: throw VttParseException("Unexpected EOF after cue identifier: $timingLine")
                if (!next.containsArrow()) {
                    throw VttParseException("Expected timing line after identifier '$timingLine', found: $next")
                }
                id = timingLine
                timingLine = next
            }

            val timing = parseTimingLine(timingLine)
            val payloadLines = ArrayList<String>()
            while (true) {
                val payloadLine = bufferedReader.readLine() ?: break
                if (payloadLine.isEmpty()) {
                    break
                }
                payloadLines.add(payloadLine)
            }

            cues.add(Cue(id, timing.begin, timing.end, timing.settings, payloadLines))
        }

        return cues
    }

    private fun parseTimingLine(line: String): Timing {
        val trimmed = line.trim()
        val arrow = trimmed.indexOf("-->")
        if (arrow < 0) {
            throw VttParseException("Invalid timing line (missing -->): $line")
        }

        val left = trimmed.substring(0, arrow).trim()
        val rightAndSettings = trimmed.substring(arrow + 3).trim()

        val whitespace = rightAndSettings.indexOfWhitespace()
        val endTimestamp: String
        val settingsPart: String
        if (whitespace < 0) {
            endTimestamp = rightAndSettings
            settingsPart = ""
        } else {
            endTimestamp = rightAndSettings.substring(0, whitespace).trim()
            settingsPart = rightAndSettings.substring(whitespace).trim()
        }

        val begin = parseVttTimestamp(left)
        val end = parseVttTimestamp(endTimestamp)
        if (end <= begin) {
            throw VttParseException("Invalid cue timing: end <= begin in line: $line")
        }

        return Timing(begin, end, parseCueSettings(settingsPart))
    }

    private fun toTtml2(cues: List<Cue>): String = buildString(32 * 1024) {
        append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
        append("<tt\n")
        append("  xmlns=\"http://www.w3.org/ns/ttml\"\n")
        append("  xmlns:ttm=\"http://www.w3.org/ns/ttml#metadata\"\n")
        append("  xmlns:tts=\"http://www.w3.org/ns/ttml#styling\"\n")
        append("  xmlns:ttp=\"http://www.w3.org/ns/ttml#parameter\"\n")
        append("  xml:lang=\"en\"\n")
        append("  ttp:timeBase=\"media\"\n")
        append("  ttp:frameRate=\"30\">\n")

        append("  <head>\n")
        append("    <styling>\n")
        append("      <style xml:id=\"s0\" tts:fontFamily=\"sansSerif\" tts:fontSize=\"1c\"/>\n")
        append("      <style xml:id=\"cTextWhite\" tts:color=\"#FFFFFF\"/>\n")
        append("      <style xml:id=\"cTextYellow\" tts:color=\"#FFFF00\"/>\n")
        append("      <style xml:id=\"cTextCyan\" tts:color=\"#00FFFF\"/>\n")
        append("    </styling>\n")
        append("    <layout>\n")
        append("      <region xml:id=\"r0\"/>\n")
        append("    </layout>\n")
        append("  </head>\n")

        append("  <body style=\"s0\" region=\"r0\">\n")
        append("    <div>\n")

        for (cue in cues) {
            append("      <p")
            append(" begin=\"").append(formatTtmlTime(cue.begin)).append("\"")
            append(" end=\"").append(formatTtmlTime(cue.end)).append("\"")

            cue.id?.takeIf { it.isNotBlank() }?.let { id ->
                append(" xml:id=\"").append(xmlIdSafe(id)).append("\"")
            }

            mapAlignToTtml(cue.settings["align"])?.let { textAlign ->
                append(" tts:textAlign=\"").append(textAlign).append("\"")
            }

            append(">")
            append(convertInlineMarkup(cue.payloadLines.joinToString("\n")))
            append("</p>\n")
        }

        append("    </div>\n")
        append("  </body>\n")
        append("</tt>\n")
    }

    private fun convertInlineMarkup(webVttText: String?): String {
        val source = webVttText ?: ""
        val out = StringBuilder(source.length + 64)
        val stack = ArrayDeque<OpenSpan>()

        var index = 0
        while (index < source.length) {
            val character = source[index]
            when {
                character == '\n' -> {
                    out.append("<br/>")
                    index++
                }

                character == '<' -> {
                    val closingIndex = source.indexOf('>', index + 1)
                    if (closingIndex < 0) {
                        out.append("&lt;")
                        index++
                        continue
                    }

                    val raw = source.substring(index + 1, closingIndex).trim()
                    if (looksLikeVttTimestampTag(raw)) {
                        index = closingIndex + 1
                        continue
                    }

                    val closing = raw.startsWith("/")
                    val inner = if (closing) raw.substring(1).trim() else raw
                    val tag = parseTag(inner)

                    when (tag.name) {
                        "b" -> {
                            if (closing) {
                                closeSpan(stack, out, OpenSpan.kind(SpanKind.BOLD))
                            } else {
                                stack.push(OpenSpan.kind(SpanKind.BOLD))
                                out.append("<span tts:fontWeight=\"bold\">")
                            }
                        }

                        "i" -> {
                            if (closing) {
                                closeSpan(stack, out, OpenSpan.kind(SpanKind.ITALIC))
                            } else {
                                stack.push(OpenSpan.kind(SpanKind.ITALIC))
                                out.append("<span tts:fontStyle=\"italic\">")
                            }
                        }

                        "u" -> {
                            if (closing) {
                                closeSpan(stack, out, OpenSpan.kind(SpanKind.UNDERLINE))
                            } else {
                                stack.push(OpenSpan.kind(SpanKind.UNDERLINE))
                                out.append("<span tts:textDecoration=\"underline\">")
                            }
                        }

                        "br" -> out.append("<br/>")
                        "c" -> {
                            if (closing) {
                                closeSpan(stack, out, OpenSpan.anyClass())
                            } else {
                                mapClassesToStyleId(tag.classes)?.let { styleId ->
                                    val span = OpenSpan.classStyle(styleId)
                                    stack.push(span)
                                    out.append("<span style=\"").append(styleId).append("\">")
                                }
                            }
                        }
                    }

                    index = closingIndex + 1
                }

                else -> {
                    out.append(HtmlUtils.escapeHtmlCharacter(character) ?: character)
                    index++
                }
            }
        }

        while (!stack.isEmpty()) {
            stack.pop()
            out.append("</span>")
        }

        return out.toString()
    }

    class VttParseException : IllegalArgumentException {
        constructor(message: String) : super(message)
        constructor(message: String, cause: Throwable) : super(message, cause)
    }

    private data class Cue(
        val id: String?,
        val begin: Duration,
        val end: Duration,
        val settings: Map<String, String>,
        val payloadLines: List<String>,
    )

    private data class Timing(
        val begin: Duration,
        val end: Duration,
        val settings: Map<String, String>,
    )

    private data class TagInfo(val name: String, val classes: List<String>)

    private enum class SpanKind {
        BOLD,
        ITALIC,
        UNDERLINE,
    }

    private data class OpenSpan(
        private val kind: SpanKind?,
        private val classStyleId: String?,
        private val anyClassCloseMarker: Boolean,
    ) {
        fun matches(other: OpenSpan): Boolean =
            when {
                anyClassCloseMarker -> other.classStyleId != null
                kind != null -> other.kind == kind
                classStyleId != null -> other.classStyleId == classStyleId
                else -> false
            }

        fun openTag(): String =
            when {
                kind == SpanKind.BOLD -> "<span tts:fontWeight=\"bold\">"
                kind == SpanKind.ITALIC -> "<span tts:fontStyle=\"italic\">"
                kind == SpanKind.UNDERLINE -> "<span tts:textDecoration=\"underline\">"
                classStyleId != null -> "<span style=\"$classStyleId\">"
                else -> "<span>"
            }

        companion object {
            fun kind(kind: SpanKind): OpenSpan = OpenSpan(kind, null, false)
            fun classStyle(styleId: String): OpenSpan = OpenSpan(null, styleId, false)
            fun anyClass(): OpenSpan = OpenSpan(null, null, true)
        }
    }

    private companion object {
        private val DEFAULT_CHARSET = StandardCharsets.UTF_8
        private val VTT_CLASS_TO_TTML_STYLE = mapOf(
            "textWhite" to "cTextWhite",
            "textYellow" to "cTextYellow",
            "textCyan" to "cTextCyan",
        )

        private fun consumeUntilBlank(reader: BufferedReader) {
            while (true) {
                val line = reader.readLine() ?: return
                if (line.isEmpty()) {
                    return
                }
            }
        }

        private fun String.containsArrow(): Boolean = contains("-->")

        private fun String.indexOfWhitespace(): Int = indexOfFirst(Char::isWhitespace)

        private fun parseCueSettings(settingsPart: String?): Map<String, String> {
            if (settingsPart.isNullOrBlank()) {
                return emptyMap()
            }

            val settings = HashMap<String, String>()
            for (token in settingsPart.trim().split(Regex("\\s+"))) {
                val colon = token.indexOf(':')
                if (colon <= 0 || colon == token.length - 1) {
                    continue
                }
                val key = token.substring(0, colon).trim()
                val value = token.substring(colon + 1).trim()
                if (key.isNotEmpty() && value.isNotEmpty()) {
                    settings[key] = value
                }
            }
            return settings
        }

        private fun parseVttTimestamp(timestamp: String): Duration {
            val trimmed = timestamp.trim()
            val dot = trimmed.indexOf('.')
            if (dot < 0) {
                throw VttParseException("Invalid timestamp (missing .mmm): $timestamp")
            }

            val hms = trimmed.substring(0, dot)
            val millisecondsText = trimmed.substring(dot + 1)
            if (millisecondsText.length != 3 || !millisecondsText.isAllDigits()) {
                throw VttParseException("Invalid milliseconds in timestamp: $timestamp")
            }

            val milliseconds = millisecondsText.toInt()
            val parts = hms.split(":")
            val hours: Int
            val minutes: Int
            val seconds: Int

            try {
                when (parts.size) {
                    3 -> {
                        hours = parseHours(parts[0], timestamp)
                        minutes = parse2Digits(parts[1], "minutes", timestamp)
                        seconds = parse2Digits(parts[2], "seconds", timestamp)
                    }

                    2 -> {
                        hours = 0
                        minutes = parse2Digits(parts[0], "minutes", timestamp)
                        seconds = parse2Digits(parts[1], "seconds", timestamp)
                    }

                    else -> throw VttParseException(
                        "Invalid timestamp format (expected MM:SS.mmm or HH:MM:SS.mmm): $timestamp"
                    )
                }
            } catch (ex: NumberFormatException) {
                throw VttParseException("Invalid numeric timestamp: $timestamp", ex)
            }

            if (minutes !in 0..59) {
                throw VttParseException("Minutes out of range in timestamp: $timestamp")
            }
            if (seconds !in 0..59) {
                throw VttParseException("Seconds out of range in timestamp: $timestamp")
            }

            return Duration.ofHours(hours.toLong())
                .plusMinutes(minutes.toLong())
                .plusSeconds(seconds.toLong())
                .plusMillis(milliseconds.toLong())
        }

        private fun parseHours(text: String, fullTimestamp: String): Int {
            if (text.length < 2 || !text.isAllDigits()) {
                throw VttParseException("Invalid hours in timestamp: $fullTimestamp")
            }
            return text.toInt()
        }

        private fun parse2Digits(text: String, field: String, fullTimestamp: String): Int {
            if (text.length != 2 || !text.isAllDigits()) {
                throw VttParseException("Invalid $field in timestamp: $fullTimestamp")
            }
            return text.toInt()
        }

        private fun String.isAllDigits(): Boolean = all(Char::isDigit)

        private fun mapAlignToTtml(vttAlign: String?): String? =
            when (vttAlign) {
                "start", "left" -> "start"
                "middle", "center" -> "center"
                "end", "right" -> "end"
                else -> null
            }

        private fun formatTtmlTime(duration: Duration): String {
            var remaining = duration.toMillis()
            val hours = remaining / 3_600_000
            remaining %= 3_600_000
            val minutes = remaining / 60_000
            remaining %= 60_000
            val seconds = remaining / 1_000
            val milliseconds = remaining % 1_000
            return "%02d:%02d:%02d.%03d".format(hours, minutes, seconds, milliseconds)
        }

        private fun xmlIdSafe(value: String): String {
            val trimmed = value.trim()
            if (trimmed.isEmpty()) {
                return "cue"
            }

            return buildString(trimmed.length + 1) {
                if (!trimmed.first().isNameStart()) {
                    append('c')
                }
                for (character in trimmed) {
                    append(if (character.isNameChar()) character else '_')
                }
            }
        }

        private fun Char.isNameStart(): Boolean = isLetter() || this == '_' || this == ':'

        private fun Char.isNameChar(): Boolean = isNameStart() || isDigit() || this == '-' || this == '.'

        private fun mapClassesToStyleId(classes: List<String>?): String? =
            classes?.firstNotNullOfOrNull(VTT_CLASS_TO_TTML_STYLE::get)

        private fun closeSpan(stack: ArrayDeque<OpenSpan>, out: StringBuilder, target: OpenSpan) {
            if (stack.isEmpty()) {
                return
            }

            val temporary = ArrayDeque<OpenSpan>()
            while (!stack.isEmpty()) {
                val top = stack.pop()
                out.append("</span>")
                if (target.matches(top)) {
                    break
                }
                temporary.push(top)
            }

            while (!temporary.isEmpty()) {
                val span = temporary.pop()
                stack.push(span)
                out.append(span.openTag())
            }
        }

        private fun looksLikeVttTimestampTag(tagContent: String): Boolean {
            val trimmed = tagContent.trim()
            if (trimmed.isEmpty() || trimmed.indexOf(' ') >= 0 || trimmed.startsWith("/")) {
                return false
            }
            val dot = trimmed.indexOf('.')
            if (dot < 0) {
                return false
            }
            val left = trimmed.substring(0, dot)
            val right = trimmed.substring(dot + 1)
            if (right.length != 3 || !right.isAllDigits()) {
                return false
            }
            val parts = left.split(":")
            if (parts.size != 2 && parts.size != 3) {
                return false
            }
            return parts.all { it.isAllDigits() }
        }

        private fun parseTag(inner: String): TagInfo {
            val text = inner.trim()
            val space = text.indexOf(' ')
            val head = if (space >= 0) text.substring(0, space).trim() else text

            val dot = head.indexOf('.')
            if (dot < 0) {
                return TagInfo(head, emptyList())
            }

            val name = head.substring(0, dot).trim()
            val rest = head.substring(dot + 1).trim()
            if (rest.isEmpty()) {
                return TagInfo(name, emptyList())
            }

            val classes = rest.split(".")
                .map { it.trim() }
                .filter { it.isNotEmpty() }
            return TagInfo(name, classes)
        }
    }
}
