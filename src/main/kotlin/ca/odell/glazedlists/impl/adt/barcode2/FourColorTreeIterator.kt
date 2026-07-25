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

internal class FourColorTreeIterator<T> {
    internal var count1: Int = 0
    internal var count2: Int = 0
    internal var count4: Int = 0
    internal var count8: Int = 0

    private val tree: FourColorTree<T>
    private var node: FourColorNode<T>? = null
    private var index: Int = 0

    constructor(tree: FourColorTree<T>) : this(tree, 0, 0)

    constructor(tree: FourColorTree<T>, nextIndex: Int, nextIndexColors: Byte) {
        this.tree = tree

        if (nextIndex != 0) {
            val currentIndex = nextIndex - 1
            node = tree[currentIndex, nextIndexColors] as FourColorNode<T>

            count1 = tree.convertIndexColor(currentIndex, nextIndexColors, 1) + if (node!!.color.toInt() == 1) 0 else 1
            count2 = tree.convertIndexColor(currentIndex, nextIndexColors, 2) + if (node!!.color.toInt() == 2) 0 else 1
            count4 = tree.convertIndexColor(currentIndex, nextIndexColors, 4) + if (node!!.color.toInt() == 4) 0 else 1
            count8 = tree.convertIndexColor(currentIndex, nextIndexColors, 8) + if (node!!.color.toInt() == 8) 0 else 1

            when (node!!.color.toInt()) {
                1 -> index = count1 - tree.indexOfNode(node!!, 1)
                2 -> index = count2 - tree.indexOfNode(node!!, 2)
                4 -> index = count4 - tree.indexOfNode(node!!, 4)
                8 -> index = count8 - tree.indexOfNode(node!!, 8)
            }
        }
    }

    fun copy(): FourColorTreeIterator<T> =
        FourColorTreeIterator(tree).also { copy ->
            copy.count1 = count1
            copy.count2 = count2
            copy.count4 = count4
            copy.count8 = count8
            copy.node = node
            copy.index = index
        }

    fun hasNext(colors: Byte): Boolean =
        if (node == null) {
            tree.size(colors) > 0
        } else if (colors.toInt() and node!!.color.toInt() != 0) {
            index(colors) < tree.size(colors) - 1
        } else {
            index(colors) < tree.size(colors)
        }

    fun hasNextNode(colors: Byte): Boolean =
        if (node == null) {
            tree.size(colors) > 0
        } else {
            nodeEndIndex(colors) < tree.size(colors)
        }

    fun next(colors: Byte) {
        if (!hasNext(colors)) throw NoSuchElementException()

        if (node == null) {
            node = tree.firstNode()
            index = 0
            if (node!!.color.toInt() and colors.toInt() != 0) return
        } else if (node!!.color.toInt() and colors.toInt() != 0 && index < node!!.size - 1) {
            when (node!!.color.toInt()) {
                1 -> count1++
                2 -> count2++
                4 -> count4++
                8 -> count8++
            }
            index++
            return
        }

        while (true) {
            when (node!!.color.toInt()) {
                1 -> count1 += node!!.size - index
                2 -> count2 += node!!.size - index
                4 -> count4 += node!!.size - index
                8 -> count8 += node!!.size - index
            }

            node = FourColorTree.next(node!!)
            index = 0
            if (node!!.color.toInt() and colors.toInt() != 0) break
        }
    }

    fun nextNode(colors: Byte) {
        if (!hasNextNode(colors)) throw NoSuchElementException()

        if (node == null) {
            node = tree.firstNode()
            index = 0
            if (node!!.color.toInt() and colors.toInt() != 0) return
        }

        while (true) {
            when (node!!.color.toInt()) {
                1 -> count1 += node!!.size - index
                2 -> count2 += node!!.size - index
                4 -> count4 += node!!.size - index
                8 -> count8 += node!!.size - index
            }

            node = FourColorTree.next(node!!)
            index = 0
            if (node!!.color.toInt() and colors.toInt() != 0) break
        }
    }

    fun nodeSize(colors: Byte): Int {
        val currentNode = node!!
        return if (currentNode.color.toInt() and colors.toInt() != 0) currentNode.size else 0
    }

    fun color(): Byte = node?.color ?: throw IllegalStateException()

    fun index(colors: Byte): Int {
        if (node == null) throw NoSuchElementException()

        var result = 0
        if (colors.toInt() and 1 != 0) result += count1
        if (colors.toInt() and 2 != 0) result += count2
        if (colors.toInt() and 4 != 0) result += count4
        if (colors.toInt() and 8 != 0) result += count8
        return result
    }

    fun nodeStartIndex(colors: Byte): Int {
        val currentNode = node ?: throw NoSuchElementException()

        var result = 0
        if (colors.toInt() and 1 != 0) result += count1
        if (colors.toInt() and 2 != 0) result += count2
        if (colors.toInt() and 4 != 0) result += count4
        if (colors.toInt() and 8 != 0) result += count8
        if (currentNode.color.toInt() and colors.toInt() != 0) result -= index
        return result
    }

    fun nodeEndIndex(colors: Byte): Int {
        if (node == null) throw NoSuchElementException()
        return nodeStartIndex(colors) + nodeSize(colors)
    }

    fun value(): T = (node ?: throw IllegalStateException()).get()

    fun node(): Element<T> = node ?: throw IllegalStateException()
}
