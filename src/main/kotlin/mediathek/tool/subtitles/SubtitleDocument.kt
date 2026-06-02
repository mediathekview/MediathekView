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

package mediathek.tool.subtitles

import java.time.Duration

/**
 * Intermediate subtitle model with region layout and per-run styling.
 */
data class SubtitleDocument(
    val regions: Map<String, Region>,
    val cues: List<Cue>,
) {
    data class Cue(
        val start: Duration,
        val end: Duration,
        val regionId: String?,
        val runs: List<StyledRun>,
        val cueStyle: CueStyle,
    ) {
        init {
            require(end >= start) { "Cue end < start: $start .. $end" }
        }

        fun plainText(): String = runs.joinToString(separator = "") { run -> run.text }
    }

    data class StyledRun(
        val text: String,
        val style: TextStyle,
    )

    /**
     * Region rectangle (origin/extent) plus align hints.
     * origin/extent may be null to indicate defaults.
     */
    data class Region(
        val id: String,
        val origin: Length2?,
        val extent: Length2?,
        val displayAlign: String?,
        val textAlign: String?,
    )

    /**
     * Cue-level layout hints (may be inherited from region).
     */
    data class CueStyle(
        val displayAlign: String?,
        val textAlign: String?,
    ) {
        fun merge(child: CueStyle?): CueStyle =
            if (child == null) {
                this
            } else {
                CueStyle(
                    displayAlign = child.displayAlign ?: displayAlign,
                    textAlign = child.textAlign ?: textAlign,
                )
            }

        companion object {
            val EMPTY = CueStyle(null, null)
        }
    }

    /**
     * Inline text styling; extend as needed (font family/size, outlines, etc).
     * color/backgroundColor are nullable and override via last-specified-wins in StyleIndex.
     */
    data class TextStyle(
        val bold: Boolean,
        val italic: Boolean,
        val underline: Boolean,
        val color: Rgba?,
        val backgroundColor: Rgba?,
    ) {
        fun merge(child: TextStyle?): TextStyle =
            if (child == null) {
                this
            } else {
                TextStyle(
                    bold = bold || child.bold,
                    italic = italic || child.italic,
                    underline = underline || child.underline,
                    color = child.color ?: color,
                    backgroundColor = child.backgroundColor ?: backgroundColor,
                )
            }

        companion object {
            val EMPTY = TextStyle(false, false, false, null, null)
        }
    }

    sealed interface Length {
        fun resolve(reference: Double): Double
    }

    data class Px(val value: Double) : Length {
        override fun resolve(reference: Double): Double = value
    }

    data class Percent(val value: Double) : Length {
        override fun resolve(reference: Double): Double = reference * (value / 100.0)
    }

    data class Length2(val x: Length, val y: Length)
}
