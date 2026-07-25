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

internal class SimpleNode<T> private constructor(
    size: Int,
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var t0: T?,
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var parent: SimpleNode<T>?,
) : Element<T> {
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var count1: Int = 0
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var height: Byte = 1
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var left: SimpleNode<T>? = null
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var right: SimpleNode<T>? = null

    override var sorted: Int = Element.SORTED

    init {
        assert(size == 1)
        count1 += size
    }

    @Suppress("UNCHECKED_CAST")
    override fun get(): T = t0 as T

    override fun set(value: T) {
        t0 = value
    }

    override val color: Byte
        get() = 1

    @JvmSynthetic
    internal fun size(colors: Byte): Int {
        var result = 0
        if (colors.toInt() and 1 != 0) result += count1
        return result
    }

    @JvmSynthetic
    internal fun refreshCounts(countSelf: Boolean) {
        count1 = 0
        if (left != null) count1 += left!!.count1
        if (right != null) count1 += right!!.count1
        count1 += if (countSelf) 1 else 0
    }

    override fun toString(): String = toString(listOf("A", "B", "C", "D", "E", "F", "G", "H"))

    @JvmSynthetic
    internal fun toString(@Suppress("UNUSED_PARAMETER") colors: List<*>): String {
        val result = StringBuilder()
        asTree(0, result, colors)
        return result.toString()
    }

    @JvmSynthetic
    internal fun asTree(
        indentation: Int,
        out: StringBuilder,
        @Suppress("UNUSED_PARAMETER") colors: List<*>,
    ) {
        left?.asTree(indentation + 1, out, colors)

        repeat(indentation.coerceAtLeast(0)) {
            out.append("   ")
        }

        val value = t0
        if (value != null) {
            out.append(": ")
            if (value is SimpleNode<*>) {
                out.append("<Node>")
            } else {
                out.append(value)
            }
        }
        out.append('\n')

        right?.asTree(indentation + 1, out, colors)
    }

    override fun next(): Element<T>? = SimpleTree.next(this)

    override fun previous(): Element<T>? = SimpleTree.previous(this)

    companion object {
        @JvmSynthetic
        internal fun <T> create(size: Int, value: T?, parent: SimpleNode<T>?): SimpleNode<T> = SimpleNode(size, value, parent)
    }
}
