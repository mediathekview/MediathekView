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

internal class SimpleTreeIterator<T> {
    internal var count1: Int = 0

    private val tree: SimpleTree<T>
    private var node: SimpleNode<T>? = null
    private var index: Int = 0

    constructor(tree: SimpleTree<T>) : this(tree, 0, 0)

    constructor(tree: SimpleTree<T>, nextIndex: Int, @Suppress("UNUSED_PARAMETER") nextIndexColors: Byte) {
        this.tree = tree

        if (nextIndex != 0) {
            val currentIndex = nextIndex - 1
            this.node = tree[currentIndex] as SimpleNode<T>
            count1 = currentIndex
            this.index = count1 - tree.indexOfNode(this.node!!, 1)
        }
    }

    fun copy(): SimpleTreeIterator<T> =
        SimpleTreeIterator(tree).also { copy ->
            copy.count1 = count1
            copy.node = node
            copy.index = index
        }

    fun hasNext(): Boolean =
        if (node == null) {
            tree.size() > 0
        } else {
            index() < tree.size() - 1
        }

    fun hasNextNode(): Boolean =
        if (node == null) {
            tree.size() > 0
        } else {
            nodeEndIndex() < tree.size()
        }

    fun next() {
        if (!hasNext()) throw NoSuchElementException()

        if (node == null) {
            node = tree.firstNode()
            index = 0
            return
        } else if (index < 1 - 1) {
            count1++
            index++
            return
        }

        while (true) {
            count1 += 1 - index
            node = SimpleTree.next(node!!)
            index = 0
            break
        }
    }

    fun nextNode() {
        if (!hasNextNode()) throw NoSuchElementException()

        if (node == null) {
            node = tree.firstNode()
            index = 0
            return
        }

        while (true) {
            count1 += 1 - index
            node = SimpleTree.next(node!!)
            index = 0
            break
        }
    }

    fun nodeSize(): Int = 1

    fun index(): Int {
        if (node == null) throw NoSuchElementException()

        var result = 0
        result += count1
        return result
    }

    fun nodeStartIndex(): Int {
        if (node == null) throw NoSuchElementException()

        var result = 0
        result += count1
        result -= index
        return result
    }

    fun nodeEndIndex(): Int {
        if (node == null) throw NoSuchElementException()
        return nodeStartIndex() + nodeSize()
    }

    fun value(): T {
        val currentNode = node ?: throw IllegalStateException()
        return currentNode.get()
    }

    fun node(): Element<T> = node ?: throw IllegalStateException()
}
