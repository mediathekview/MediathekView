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

package mediathek.tool.subtitles.ttml2

import mediathek.tool.subtitles.SubtitleDocument
import mediathek.tool.subtitles.SubtitleDocument.TextStyle
import java.time.Duration

/**
 * SubRip exporter with HTML-like style tags:
 * - <b>, <i>, <u>
 * - <font color="#RRGGBB"> (alpha ignored; SRT has no standard alpha support)
 */
class SubRipHtmlExporter {
    fun export(doc: SubtitleDocument): String = buildString {
        var index = 1

        for (cue in doc.cues) {
            append(index++)
            append(LINE_SEPARATOR)
            append(formatTimestamp(cue.start))
            append(" --> ")
            append(formatTimestamp(cue.end))
            append(LINE_SEPARATOR)

            var previousStyle: TextStyle? = null
            for (run in cue.runs) {
                val currentStyle = run.style

                if (previousStyle != null && previousStyle != currentStyle) {
                    append(closeTags(previousStyle))
                }
                if (previousStyle == null || previousStyle != currentStyle) {
                    append(openTags(currentStyle))
                }

                append(escapeText(run.text))
                previousStyle = currentStyle
            }
            if (previousStyle != null) {
                append(closeTags(previousStyle))
            }

            append(LINE_SEPARATOR)
            append(LINE_SEPARATOR)
        }
    }

    private fun openTags(style: TextStyle?): String {
        if (style == null) {
            return ""
        }
        return buildString {
            if (style.bold) {
                append("<b>")
            }
            if (style.italic) {
                append("<i>")
            }
            if (style.underline) {
                append("<u>")
            }
            val color = style.color
            if (color != null && !color.isTransparent()) {
                append("<font color=\"")
                append("#%02X%02X%02X".format(color.r, color.g, color.b))
                append("\">")
            }
        }
    }

    private fun closeTags(style: TextStyle?): String {
        if (style == null) {
            return ""
        }
        return buildString {
            if (style.color != null && !style.color.isTransparent()) {
                append("</font>")
            }
            if (style.underline) {
                append("</u>")
            }
            if (style.italic) {
                append("</i>")
            }
            if (style.bold) {
                append("</b>")
            }
        }
    }

    private fun formatTimestamp(duration: Duration): String {
        var milliseconds = duration.toMillis().coerceAtLeast(0)
        val hours = milliseconds / 3_600_000
        milliseconds %= 3_600_000
        val minutes = milliseconds / 60_000
        milliseconds %= 60_000
        val seconds = milliseconds / 1_000
        val remainingMilliseconds = milliseconds % 1_000
        return "%02d:%02d:%02d,%03d".format(hours, minutes, seconds, remainingMilliseconds)
    }

    private fun escapeText(text: String): String =
        text
            .replace("\\n", LINE_SEPARATOR)
            .replace("<", "&lt;")
            .replace(">", "&gt;")

    private companion object {
        private const val LINE_SEPARATOR = "\r\n"
    }
}
