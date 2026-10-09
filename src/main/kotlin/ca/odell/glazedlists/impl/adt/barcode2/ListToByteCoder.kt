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
package ca.odell.glazedlists.impl.adt.barcode2

import java.util.*

/**
 * Makes conversions and color operations efficient by using bytes rather than
 * full-size objects. At most seven values are supported, one for each non-sign
 * bit of a byte.
 *
 * @author [Jesse Wilson](mailto:jesse@swank.ca)
 */
internal class ListToByteCoder<C>(allColors: List<C>) {
    val colors: List<C> = Collections.unmodifiableList(ArrayList(allColors))

    init {
        require(colors.size <= MAX_COLORS) { "Max 7 colors!" }
        require(colors.indices.all { colors.indexOf(colors[it]) == it }) { "Colors must be unique!" }
    }

    fun allColorsToByte(): Byte = colorsToByte(colors)

    /** Encodes the specified list of colors into a byte. */
    fun colorsToByte(colors: List<C>): Byte {
        var result = 0
        for (color in colors) {
            result = result or (1 shl indexOfColor(color))
        }
        return result.toByte()
    }

    /** Encodes the specified color into a byte. */
    fun colorToByte(color: C): Byte = (1 shl indexOfColor(color)).toByte()

    private fun indexOfColor(color: C): Int {
        val index = colors.indexOf(color)
        require(index >= 0) { "Unknown color: $color" }
        return index
    }

    companion object {
        private const val MAX_COLORS = 7

        /** Converts a single encoded color bit into its zero-based index. */
        @JvmName("colorAsIndex")
        internal fun colorAsIndex(color: Byte): Int =
            when (color.toInt()) {
                1 -> 0
                2 -> 1
                4 -> 2
                8 -> 3
                16 -> 4
                32 -> 5
                64 -> 6
                else -> throw IllegalArgumentException()
            }
    }
}
