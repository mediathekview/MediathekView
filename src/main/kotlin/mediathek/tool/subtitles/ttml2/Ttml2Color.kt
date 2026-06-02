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

import mediathek.tool.subtitles.Rgba
import java.util.Locale
import java.util.regex.Pattern

/**
 * TTML2 <color> lexical parser.
 * Supports:
 * - #RRGGBB
 * - #RRGGBBAA
 * - rgb(r,g,b) where each component is [0,255]
 * - rgba(r,g,b,a) where each component is [0,255]
 * - named colors (case-insensitive): transparent, black, silver, gray, white, maroon, red,
 * purple, fuchsia, magenta, green, lime, olive, yellow, navy, blue, teal, aqua, cyan.
 */
object Ttml2Color {
    private val RGB = Pattern.compile(
        "^rgb\\(\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*\\)$",
        Pattern.CASE_INSENSITIVE,
    )
    private val RGBA = Pattern.compile(
        "^rgba\\(\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*\\)$",
        Pattern.CASE_INSENSITIVE,
    )

    private val NAMED = mapOf(
        "transparent" to Rgba(0, 0, 0, 0),
        "black" to Rgba(0, 0, 0, 255),
        "silver" to Rgba(192, 192, 192, 255),
        "gray" to Rgba(128, 128, 128, 255),
        "white" to Rgba(255, 255, 255, 255),
        "maroon" to Rgba(128, 0, 0, 255),
        "red" to Rgba(255, 0, 0, 255),
        "purple" to Rgba(128, 0, 128, 255),
        "fuchsia" to Rgba(255, 0, 255, 255),
        "magenta" to Rgba(255, 0, 255, 255),
        "green" to Rgba(0, 128, 0, 255),
        "lime" to Rgba(0, 255, 0, 255),
        "olive" to Rgba(128, 128, 0, 255),
        "yellow" to Rgba(255, 255, 0, 255),
        "navy" to Rgba(0, 0, 128, 255),
        "blue" to Rgba(0, 0, 255, 255),
        "teal" to Rgba(0, 128, 128, 255),
        "aqua" to Rgba(0, 255, 255, 255),
        "cyan" to Rgba(0, 255, 255, 255),
    )

    fun parse(raw: String?): Rgba? {
        val color = raw?.trim()?.takeIf { it.isNotEmpty() } ?: return null

        if (color.startsWith("#")) {
            return parseHexColor(color, raw)
        }

        RGB.matcher(color).takeIf { matcher -> matcher.matches() }?.let { matcher ->
            return Rgba(comp(matcher.group(1), raw), comp(matcher.group(2), raw), comp(matcher.group(3), raw), 255)
        }
        RGBA.matcher(color).takeIf { matcher -> matcher.matches() }?.let { matcher ->
            return Rgba(
                comp(matcher.group(1), raw),
                comp(matcher.group(2), raw),
                comp(matcher.group(3), raw),
                comp(matcher.group(4), raw),
            )
        }

        NAMED[color.lowercase(Locale.ROOT)]?.let { return it }

        throw IllegalArgumentException("Invalid TTML2 <color>: $raw")
    }

    private fun parseHexColor(color: String, raw: String): Rgba {
        val hex = color.substring(1)
        if (hex.length == 6) {
            return Rgba(
                hex.substring(0, 2).toInt(16),
                hex.substring(2, 4).toInt(16),
                hex.substring(4, 6).toInt(16),
                255,
            )
        }
        if (hex.length == 8) {
            return Rgba(
                hex.substring(0, 2).toInt(16),
                hex.substring(2, 4).toInt(16),
                hex.substring(4, 6).toInt(16),
                hex.substring(6, 8).toInt(16),
            )
        }
        throw IllegalArgumentException("Invalid TTML2 color hex: $raw")
    }

    private fun comp(dec: String, raw: String): Int {
        val value = dec.toIntOrNull()
            ?: throw IllegalArgumentException("Invalid TTML2 color component in: $raw")
        require(value in 0..255) { "TTML2 color component out of range [0,255]: $raw" }
        return value
    }
}
