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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.impl.adt.barcode2

internal class FourColorNode<T> private constructor(
    initialColor: Byte,
    initialSize: Int,
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var t0: T?,
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var parent: FourColorNode<T>?,
) : Element<T> {
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var count1: Int = 0
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var count2: Int = 0
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var count4: Int = 0
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var count8: Int = 0

    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    override var color: Byte = initialColor

    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var size: Int = initialSize

    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var height: Byte = 1
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var left: FourColorNode<T>? = null
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var right: FourColorNode<T>? = null

    override var sorted: Int = Element.SORTED

    init {
        assert(ListToByteCoder.colorAsIndex(color) in 0..<7)
        when (color.toInt()) {
            1 -> count1 += size
            2 -> count2 += size
            4 -> count4 += size
            8 -> count8 += size
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun get(): T = t0 as T

    override fun set(value: T) {
        t0 = value
    }

    @JvmSynthetic
    internal fun size(colors: Byte): Int {
        var result = 0
        if (colors.toInt() and 1 != 0) result += count1
        if (colors.toInt() and 2 != 0) result += count2
        if (colors.toInt() and 4 != 0) result += count4
        if (colors.toInt() and 8 != 0) result += count8
        return result
    }

    @JvmSynthetic
    internal fun nodeSize(colors: Byte): Int = if (colors.toInt() and color.toInt() > 0) size else 0

    @JvmSynthetic
    internal fun refreshCounts() {
        count1 = 0
        count2 = 0
        count4 = 0
        count8 = 0

        if (left != null) {
            count1 += left!!.count1
            count2 += left!!.count2
            count4 += left!!.count4
            count8 += left!!.count8
        }

        if (right != null) {
            count1 += right!!.count1
            count2 += right!!.count2
            count4 += right!!.count4
            count8 += right!!.count8
        }

        when (color.toInt()) {
            1 -> count1 += size
            2 -> count2 += size
            4 -> count4 += size
            8 -> count8 += size
        }
    }

    override fun toString(): String = toString(listOf("A", "B", "C", "D", "E", "F", "G", "H"))

    @JvmSynthetic
    internal fun toString(colors: List<*>): String {
        val result = StringBuilder()
        asTree(0, result, colors)
        return result.toString()
    }

    @JvmSynthetic
    internal fun asTree(indentation: Int, out: StringBuilder, colors: List<*>) {
        left?.asTree(indentation + 1, out, colors)

        repeat(indentation.coerceAtLeast(0)) {
            out.append("   ")
        }
        out.append(colors[ListToByteCoder.colorAsIndex(color)])
        out.append(" [").append(size).append("]")
        val value = t0
        if (value != null) {
            out.append(": ")
            if (value is FourColorNode<*>) {
                out.append("<Node>")
            } else {
                out.append(value)
            }
        }
        out.append('\n')

        right?.asTree(indentation + 1, out, colors)
    }

    override fun next(): Element<T>? = FourColorTree.next(this)

    override fun previous(): Element<T>? = FourColorTree.previous(this)

    companion object {
        @JvmSynthetic
        internal fun <T> create(color: Byte, size: Int, value: T?, parent: FourColorNode<T>?): FourColorNode<T> =
            FourColorNode(color, size, value, parent)
    }
}
