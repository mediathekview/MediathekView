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

import mediathek.config.Konstanten
import mediathek.tool.subtitles.SubtitleDocument
import mediathek.tool.subtitles.SubtitleDocument.Cue
import mediathek.tool.subtitles.SubtitleDocument.Region
import mediathek.tool.subtitles.SubtitleDocument.TextStyle
import java.time.Duration
import java.util.Locale
import kotlin.math.roundToInt

/**
 * Advanced SubStation Alpha (ASS) exporter with:
 * - mixed-span styling (\b, \i, \u, \1c/\1a for color+alpha)
 * - region-based placement via \pos(x,y) and \an
 * Background color has no perfect ASS equivalent; optional approximation via outline.
 */
class AssExporter(
    options: Options?,
) {
    data class Options(
        val playResX: Int,
        val playResY: Int,
        val approximateBackground: Boolean,
    ) {
        init {
            require(playResX > 0 && playResY > 0) { "playRes must be > 0" }
        }

        companion object {
            fun defaults(): Options = Options(384, 288, false)
        }
    }

    private val options: Options = options ?: Options.defaults()

    fun export(doc: SubtitleDocument): String = buildString {
        append(header(options.playResX, options.playResY))
        for (cue in doc.cues) {
            append(dialogue(doc, cue))
            append('\n')
        }
    }

    private fun dialogue(doc: SubtitleDocument, cue: Cue): String {
        val start = formatAssTimestamp(cue.start)
        val end = formatAssTimestamp(cue.end)

        val region = cue.regionId?.let { regionId -> doc.regions[regionId] }
        val rect = region?.let(::resolveRect) ?: Rect(0, 0, options.playResX, options.playResY)

        val textAlign = cue.cueStyle.textAlign ?: region?.textAlign
        val displayAlign = cue.cueStyle.displayAlign ?: region?.displayAlign
        val anchor = anchor(rect, textAlign, displayAlign)

        val text = buildString {
            append("{\\pos(${anchor.x},${anchor.y})\\an${anchor.an}}")

            var previousStyle = TextStyle.EMPTY
            for (run in cue.runs) {
                val currentStyle = run.style
                if (currentStyle != previousStyle) {
                    append("{")
                    append(styleDelta(previousStyle, currentStyle))
                    append("}")
                    previousStyle = currentStyle
                }
                append(escapeAss(run.text).replace("\\n", "\\N"))
            }
        }

        return "Dialogue: 0,$start,$end,Default,,0,0,0,,$text"
    }

    private fun styleDelta(previousStyle: TextStyle, currentStyle: TextStyle): String = buildString {
        if (previousStyle.bold != currentStyle.bold) {
            append(if (currentStyle.bold) "\\b1" else "\\b0")
        }
        if (previousStyle.italic != currentStyle.italic) {
            append(if (currentStyle.italic) "\\i1" else "\\i0")
        }
        if (previousStyle.underline != currentStyle.underline) {
            append(if (currentStyle.underline) "\\u1" else "\\u0")
        }

        if (previousStyle.color != currentStyle.color) {
            val color = currentStyle.color
            if (color != null) {
                append("\\1c&H%02X%02X%02X&".format(color.b, color.g, color.r))
                append("\\1a&H%02X&".format(255 - color.a))
            } else {
                append("\\1a&H00&")
            }
        }

        if (options.approximateBackground && previousStyle.backgroundColor != currentStyle.backgroundColor) {
            val backgroundColor = currentStyle.backgroundColor
            if (backgroundColor != null) {
                append("\\3c&H%02X%02X%02X&".format(backgroundColor.b, backgroundColor.g, backgroundColor.r))
                append("\\3a&H%02X&".format(255 - backgroundColor.a))
                append("\\bord3\\shad0")
            } else {
                append("\\bord2")
            }
        }
    }

    private fun resolveRect(region: Region): Rect {
        var x = 0
        var y = 0
        var width = options.playResX
        var height = options.playResY
        region.origin?.let { origin ->
            x = origin.x.resolve(options.playResX.toDouble()).roundToInt()
            y = origin.y.resolve(options.playResY.toDouble()).roundToInt()
        }
        region.extent?.let { extent ->
            width = extent.x.resolve(options.playResX.toDouble()).roundToInt()
            height = extent.y.resolve(options.playResY.toDouble()).roundToInt()
        }
        return Rect(x, y, width, height)
    }

    private fun anchor(rect: Rect, textAlign: String?, displayAlign: String?): Anchor {
        val textAlignment = textAlign?.lowercase(Locale.ROOT) ?: "center"
        val (x, column) =
            when {
                textAlignment.contains("end") || textAlignment.contains("right") -> rect.x + rect.width to 3
                textAlignment.contains("start") || textAlignment.contains("left") -> rect.x to 1
                else -> rect.x + rect.width / 2 to 2
            }

        val displayAlignment = displayAlign?.lowercase(Locale.ROOT) ?: "after"
        val (y, rowBase) =
            when {
                displayAlignment.contains("before") || displayAlignment.contains("top") -> rect.y to 6
                displayAlignment.contains("center") || displayAlignment.contains("middle") -> rect.y + rect.height / 2 to 3
                else -> rect.y + rect.height to 0
            }

        return Anchor(x, y, rowBase + column)
    }

    private fun header(x: Int, y: Int): String =
        "[Script Info]\n" +
            "; Script generated by MediathekView ${Konstanten.MVVERSION}\n" +
            "ScriptType: v4.00+\n" +
            "Collisions: Normal\n" +
            "PlayResX: $x\n" +
            "PlayResY: $y\n" +
            "WrapStyle: 0\n" +
            "ScaledBorderAndShadow: yes\n\n" +
            "[V4+ Styles]\n" +
            "Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\n" +
            "Style: Default,Arial,24,&H00FFFFFF,&H000000FF,&H00000000,&H64000000,0,0,0,0,100,100,0,0,1,2,1,2,20,20,20,1\n\n" +
            "[Events]\n" +
            "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n"

    private fun formatAssTimestamp(duration: Duration): String {
        var milliseconds = duration.toMillis().coerceAtLeast(0)
        val hours = milliseconds / 3_600_000
        milliseconds %= 3_600_000
        val minutes = milliseconds / 60_000
        milliseconds %= 60_000
        val seconds = milliseconds / 1_000
        val centiseconds = (milliseconds % 1_000) / 10
        return "%d:%02d:%02d.%02d".format(hours, minutes, seconds, centiseconds)
    }

    private fun escapeAss(text: String): String = text.replace("{", "｛").replace("}", "｝")

    private data class Rect(val x: Int, val y: Int, val width: Int, val height: Int)

    private data class Anchor(val x: Int, val y: Int, val an: Int)
}
