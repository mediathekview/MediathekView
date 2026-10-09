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

data class Rgba(
    val r: Int,
    val g: Int,
    val b: Int,
    val a: Int,
) {
    init {
        require(r in RGBA_RANGE && g in RGBA_RANGE && b in RGBA_RANGE && a in RGBA_RANGE) {
            "RGBA components must be in [0,255]"
        }
    }

    fun isOpaque(): Boolean = a == 255

    fun isTransparent(): Boolean = a == 0

    companion object {
        private val RGBA_RANGE = 0..255
    }
}
