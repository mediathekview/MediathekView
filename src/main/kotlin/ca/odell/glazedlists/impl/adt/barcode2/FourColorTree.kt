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

import kotlin.math.abs

internal class FourColorTree<T>(private val coder: ListToByteCoder<*>) {
    private var root: FourColorNode<T>? = null
    private val zeroQueue = ArrayList<FourColorNode<T>>()

    operator fun get(index: Int, indexColors: Byte): Element<T> {
        if (root == null) throw IndexOutOfBoundsException()

        var currentNode = root
        var currentIndex = index
        while (true) {
            assert(currentNode != null)
            assert(currentIndex >= 0)

            val node = currentNode!!
            val nodeLeft = node.left
            val leftSize = nodeLeft?.size(indexColors) ?: 0
            if (currentIndex < leftSize) {
                currentNode = nodeLeft
                continue
            } else {
                currentIndex -= leftSize
            }

            val size = node.nodeSize(indexColors)
            if (currentIndex < size) {
                return node
            } else {
                currentIndex -= size
            }

            currentNode = node.right
        }
    }

    fun add(index: Int, indexColors: Byte, color: Byte, value: T?, size: Int): Element<T> {
        assert(index >= 0)
        assert(index <= size(indexColors))
        assert(size >= 0)

        if (root == null) {
            if (index != 0) throw IndexOutOfBoundsException()

            root = FourColorNode.create(color, size, value, null)
            validateInvariantsIfAssertionsEnabled()
            return root!!
        }

        val inserted = insertIntoSubtree(root!!, index, indexColors, color, value, size)
        validateInvariantsIfAssertionsEnabled()
        return inserted
    }

    private fun insertIntoSubtree(
        parent: FourColorNode<T>,
        index: Int,
        indexColors: Byte,
        color: Byte,
        value: T?,
        size: Int,
    ): FourColorNode<T> {
        var currentParent = parent
        var currentIndex = index
        while (true) {
            assert(currentIndex >= 0)

            val parentLeft = currentParent.left
            val parentLeftSize = parentLeft?.size(indexColors) ?: 0
            var parentRightStartIndex = parentLeftSize + currentParent.nodeSize(indexColors)

            if (color == currentParent.color && value === currentParent.t0 && value != null) {
                if (currentIndex in parentLeftSize..parentRightStartIndex) {
                    currentParent.size += size
                    fixCountsThruRoot(currentParent, color, size)
                    return currentParent
                }
            }

            if (currentIndex <= parentLeftSize) {
                if (parentLeft == null) {
                    val inserted = FourColorNode.create(color, size, value, currentParent)
                    currentParent.left = inserted
                    fixCountsThruRoot(currentParent, color, size)
                    fixHeightPostChange(currentParent, false)
                    return inserted
                }

                currentParent = parentLeft
                continue
            }

            if (currentIndex < parentRightStartIndex) {
                val parentRightHalfSize = parentRightStartIndex - currentIndex
                currentParent.size -= parentRightHalfSize
                fixCountsThruRoot(currentParent, currentParent.color, -parentRightHalfSize)
                val inserted = insertIntoSubtree(currentParent, currentIndex, indexColors, currentParent.color, null, parentRightHalfSize)
                inserted.t0 = currentParent.t0
                parentRightStartIndex = parentLeftSize + currentParent.nodeSize(indexColors)
            }

            val parentSize = currentParent.size(indexColors)
            assert(currentIndex <= parentSize)
            val parentRight = currentParent.right
            if (parentRight == null) {
                val inserted = FourColorNode.create(color, size, value, currentParent)
                currentParent.right = inserted
                fixCountsThruRoot(currentParent, color, size)
                fixHeightPostChange(currentParent, false)
                return inserted
            }

            currentParent = parentRight
            currentIndex -= parentRightStartIndex
        }
    }


    private fun fixCountsThruRoot(node: FourColorNode<T>?, color: Byte, delta: Int) {
        when (color.toInt()) {
            1 -> {
                var currentNode = node
                while (currentNode != null) {
                    currentNode.count1 += delta
                    currentNode = currentNode.parent
                }
            }

            2 -> {
                var currentNode = node
                while (currentNode != null) {
                    currentNode.count2 += delta
                    currentNode = currentNode.parent
                }
            }

            4 -> {
                var currentNode = node
                while (currentNode != null) {
                    currentNode.count4 += delta
                    currentNode = currentNode.parent
                }
            }

            8 -> {
                var currentNode = node
                while (currentNode != null) {
                    currentNode.count8 += delta
                    currentNode = currentNode.parent
                }
            }
        }
    }

    fun setColor(element: Element<T>, color: Byte) {
        val node = element as FourColorNode<T>
        val oldColor = node.color
        if (oldColor == color) return

        fixCountsThruRoot(node, oldColor, -node.size)
        node.color = color
        fixCountsThruRoot(node, color, node.size)
    }

    private fun fixHeightPostChange(node: FourColorNode<T>?, allTheWayToRoot: Boolean) {
        var currentNode = node
        while (currentNode != null) {
            var leftHeight = currentNode.left?.height?.toInt() ?: 0
            var rightHeight = currentNode.right?.height?.toInt() ?: 0

            if (leftHeight > rightHeight && leftHeight - rightHeight == 2) {
                val leftLeftHeight = currentNode.left?.left?.height?.toInt() ?: 0
                val leftRightHeight = currentNode.left?.right?.height?.toInt() ?: 0
                if (leftRightHeight > leftLeftHeight) {
                    rotateRight(currentNode.left!!)
                }
                currentNode = rotateLeft(currentNode)
            } else if (rightHeight > leftHeight && rightHeight - leftHeight == 2) {
                val rightLeftHeight = currentNode.right?.left?.height?.toInt() ?: 0
                val rightRightHeight = currentNode.right?.right?.height?.toInt() ?: 0
                if (rightLeftHeight > rightRightHeight) {
                    rotateLeft(currentNode.right!!)
                }
                currentNode = rotateRight(currentNode)
            }

            leftHeight = currentNode.left?.height?.toInt() ?: 0
            rightHeight = currentNode.right?.height?.toInt() ?: 0
            val newNodeHeight = (maxOf(leftHeight, rightHeight) + 1).toByte()
            if (!allTheWayToRoot && currentNode.height == newNodeHeight) return
            currentNode.height = newNodeHeight
            currentNode = currentNode.parent
        }
    }

    private fun rotateLeft(subtreeRoot: FourColorNode<T>): FourColorNode<T> {
        assert(subtreeRoot.left != null)
        val newSubtreeRoot = subtreeRoot.left!!

        subtreeRoot.left = newSubtreeRoot.right
        if (newSubtreeRoot.right != null) newSubtreeRoot.right!!.parent = subtreeRoot
        newSubtreeRoot.parent = subtreeRoot.parent
        if (newSubtreeRoot.parent != null) {
            if (newSubtreeRoot.parent!!.left == subtreeRoot) {
                newSubtreeRoot.parent!!.left = newSubtreeRoot
            } else if (newSubtreeRoot.parent!!.right == subtreeRoot) {
                newSubtreeRoot.parent!!.right = newSubtreeRoot
            } else {
                throw IllegalStateException()
            }
        } else {
            root = newSubtreeRoot
        }
        newSubtreeRoot.right = subtreeRoot
        subtreeRoot.parent = newSubtreeRoot

        val subtreeRootLeftHeight = subtreeRoot.left?.height?.toInt() ?: 0
        val subtreeRootRightHeight = subtreeRoot.right?.height?.toInt() ?: 0
        subtreeRoot.height = (maxOf(subtreeRootLeftHeight, subtreeRootRightHeight) + 1).toByte()
        subtreeRoot.refreshCounts()

        val newSubtreeRootLeftHeight = newSubtreeRoot.left?.height?.toInt() ?: 0
        val newSubtreeRootRightHeight = newSubtreeRoot.right?.height?.toInt() ?: 0
        newSubtreeRoot.height = (maxOf(newSubtreeRootLeftHeight, newSubtreeRootRightHeight) + 1).toByte()
        newSubtreeRoot.refreshCounts()

        return newSubtreeRoot
    }

    private fun rotateRight(subtreeRoot: FourColorNode<T>): FourColorNode<T> {
        assert(subtreeRoot.right != null)
        val newSubtreeRoot = subtreeRoot.right!!

        subtreeRoot.right = newSubtreeRoot.left
        if (newSubtreeRoot.left != null) newSubtreeRoot.left!!.parent = subtreeRoot
        newSubtreeRoot.parent = subtreeRoot.parent
        if (newSubtreeRoot.parent != null) {
            if (newSubtreeRoot.parent!!.left == subtreeRoot) {
                newSubtreeRoot.parent!!.left = newSubtreeRoot
            } else if (newSubtreeRoot.parent!!.right == subtreeRoot) {
                newSubtreeRoot.parent!!.right = newSubtreeRoot
            } else {
                throw IllegalStateException()
            }
        } else {
            root = newSubtreeRoot
        }
        newSubtreeRoot.left = subtreeRoot
        subtreeRoot.parent = newSubtreeRoot

        val subtreeRootLeftHeight = subtreeRoot.left?.height?.toInt() ?: 0
        val subtreeRootRightHeight = subtreeRoot.right?.height?.toInt() ?: 0
        subtreeRoot.height = (maxOf(subtreeRootLeftHeight, subtreeRootRightHeight) + 1).toByte()
        subtreeRoot.refreshCounts()

        val newSubtreeRootLeftHeight = newSubtreeRoot.left?.height?.toInt() ?: 0
        val newSubtreeRootRightHeight = newSubtreeRoot.right?.height?.toInt() ?: 0
        newSubtreeRoot.height = (maxOf(newSubtreeRootLeftHeight, newSubtreeRootRightHeight) + 1).toByte()
        newSubtreeRoot.refreshCounts()

        return newSubtreeRoot
    }

    fun remove(element: Element<T>) {
        val node = element as FourColorNode<T>
        assert(node.size > 0)
        assert(root != null)

        fixCountsThruRoot(node, node.color, -node.size)
        node.size = 0
        zeroQueue.add(node)
        drainZeroQueue()

        validateInvariantsIfAssertionsEnabled()
    }

    fun remove(index: Int, indexColors: Byte, size: Int) {
        if (size == 0) return
        assert(index >= 0)
        assert(index + size <= size(indexColors))
        assert(root != null)

        removeFromSubtree(root!!, index, indexColors, size)
        drainZeroQueue()

        validateInvariantsIfAssertionsEnabled()
    }

    private fun drainZeroQueue() {
        for (queuedNode in zeroQueue) {
            assert(queuedNode.size == 0)

            if (queuedNode.right == null) {
                replaceChild(queuedNode, queuedNode.left)
            } else if (queuedNode.left == null) {
                replaceChild(queuedNode, queuedNode.right)
            } else {
                replaceEmptyNodeWithChild(queuedNode)
            }
        }
        zeroQueue.clear()
    }

    private fun removeFromSubtree(node: FourColorNode<T>, index: Int, indexColors: Byte, size: Int) {
        var currentNode = node
        var currentIndex = index
        var remainingSize = size
        while (remainingSize > 0) {
            assert(currentIndex >= 0)

            val nodeLeft = currentNode.left
            var leftSize = nodeLeft?.size(indexColors) ?: 0
            if (currentIndex < leftSize) {
                if (currentIndex + remainingSize > leftSize) {
                    val toRemove = leftSize - currentIndex
                    removeFromSubtree(nodeLeft!!, currentIndex, indexColors, toRemove)
                    remainingSize -= toRemove
                    leftSize -= toRemove
                } else {
                    currentNode = nodeLeft!!
                    continue
                }
            }
            assert(currentIndex >= leftSize)

            var rightStartIndex = leftSize + currentNode.nodeSize(indexColors)
            if (currentIndex < rightStartIndex) {
                val toRemove = minOf(rightStartIndex - currentIndex, remainingSize)
                currentNode.size -= toRemove
                remainingSize -= toRemove
                rightStartIndex -= toRemove
                fixCountsThruRoot(currentNode, currentNode.color, -toRemove)
                if (currentNode.size == 0) {
                    zeroQueue.add(currentNode)
                }
                if (remainingSize == 0) return
            }
            assert(currentIndex >= rightStartIndex)

            currentIndex -= rightStartIndex
            currentNode = currentNode.right!!
        }
    }

    private fun replaceChild(node: FourColorNode<T>, replacement: FourColorNode<T>?) {
        val nodeParent = node.parent
        if (nodeParent == null) {
            assert(node == root)
            root = replacement
        } else if (nodeParent.left == node) {
            nodeParent.left = replacement
        } else if (nodeParent.right == node) {
            nodeParent.right = replacement
        }

        if (replacement != null) {
            replacement.parent = nodeParent
        }

        fixHeightPostChange(nodeParent, true)
    }

    private fun replaceEmptyNodeWithChild(toReplace: FourColorNode<T>): FourColorNode<T> {
        assert(toReplace.size == 0)
        assert(toReplace.left != null)
        assert(toReplace.right != null)

        var replacement = toReplace.left!!
        while (replacement.right != null) {
            replacement = replacement.right!!
        }
        assert(replacement.right == null)

        fixCountsThruRoot(replacement, replacement.color, -replacement.size)
        replaceChild(replacement, replacement.left)

        replacement.left = toReplace.left
        if (replacement.left != null) replacement.left!!.parent = replacement
        replacement.right = toReplace.right
        if (replacement.right != null) replacement.right!!.parent = replacement
        replacement.height = toReplace.height
        replacement.refreshCounts()
        replaceChild(toReplace, replacement)
        fixCountsThruRoot(replacement.parent, replacement.color, replacement.size)

        return replacement
    }

    fun set(index: Int, indexColors: Byte, color: Byte, value: T?, size: Int): Element<T> {
        remove(index, indexColors, size)
        return add(index, indexColors, color, value, size)
    }

    fun clear() {
        root = null
    }

    fun indexOfNode(element: Element<T>, colorsOut: Byte): Int {
        var node = element as FourColorNode<T>

        var index = node.left?.size(colorsOut) ?: 0
        while (node.parent != null) {
            if (node.parent!!.right == node) {
                index += node.parent!!.left?.size(colorsOut) ?: 0
                index += node.parent!!.nodeSize(colorsOut)
            }
            node = node.parent!!
        }

        return index
    }


    fun convertIndexColor(index: Int, indexColors: Byte, colorsOut: Byte): Int {
        if (root == null) {
            if (index == 0) return 0
            throw IndexOutOfBoundsException()
        }

        var currentIndex = index
        var result = 0
        var currentNode = root
        while (true) {
            assert(currentNode != null)
            assert(currentIndex >= 0)

            val node = currentNode!!
            val nodeLeft = node.left
            val leftSize = nodeLeft?.size(indexColors) ?: 0
            if (currentIndex < leftSize) {
                currentNode = nodeLeft
                continue
            } else {
                if (nodeLeft != null) result += nodeLeft.size(colorsOut)
                currentIndex -= leftSize
            }

            val size = node.nodeSize(indexColors)
            if (currentIndex < size) {
                if (colorsOut.toInt() and node.color.toInt() > 0) {
                    result += currentIndex
                } else {
                    result -= 1
                }
                return result
            } else {
                result += node.nodeSize(colorsOut)
                currentIndex -= size
            }

            currentNode = node.right
        }
    }

    fun size(colors: Byte): Int = root?.size(colors) ?: 0

    override fun toString(): String = root?.toString(coder.colors) ?: ""

    fun asSequenceOfColors(): String {
        if (root == null) return ""

        val result = StringBuilder()
        var node = firstNode()
        while (node != null) {
            val color = coder.colors[ListToByteCoder.colorAsIndex(node.color)]
            repeat(node.size.coerceAtLeast(0)) {
                result.append(color)
            }
            node = next(node)
        }
        return result.toString()
    }

    @JvmSynthetic
    internal fun firstNode(): FourColorNode<T>? {
        var result = root ?: return null
        while (result.left != null) {
            result = result.left!!
        }
        return result
    }

    private fun validateInvariantsIfAssertionsEnabled() {
        if (!ASSERTIONS_ENABLED) return
        validateInvariants()
    }

    private fun validateInvariants() {
        var node = firstNode()
        while (node != null) {
            val originalCount1 = node.count1
            val originalCount2 = node.count2
            val originalCount4 = node.count4
            val originalCount8 = node.count8

            node.refreshCounts()

            assert(originalCount1 == node.count1) {
                "Incorrect count 0 on node: \n$node\n Expected ${node.count1} but was $originalCount1"
            }
            assert(originalCount2 == node.count2) {
                "Incorrect count 1 on node: \n$node\n Expected ${node.count2} but was $originalCount2"
            }
            assert(originalCount4 == node.count4) {
                "Incorrect count 2 on node: \n$node\n Expected ${node.count4} but was $originalCount4"
            }
            assert(originalCount8 == node.count8) {
                "Incorrect count 3 on node: \n$node\n Expected ${node.count8} but was $originalCount8"
            }

            val leftHeight = node.left?.height?.toInt() ?: 0
            val rightHeight = node.right?.height?.toInt() ?: 0
            assert(maxOf(leftHeight, rightHeight) + 1 == node.height.toInt())
            assert(node.left == null || node.left!!.parent == node)
            assert(node.right == null || node.right!!.parent == node)
            assert(abs(leftHeight - rightHeight) < 2) { "Subtree is not AVL: \n$node" }

            node = next(node)
        }
    }

    companion object {
        private val ASSERTIONS_ENABLED = FourColorTree::class.java.desiredAssertionStatus()

        internal fun <T> next(node: FourColorNode<T>): FourColorNode<T>? {
            if (node.right != null) {
                var child = node.right
                while (child?.left != null) {
                    child = child.left
                }
                return child
            }

            var ancestor: FourColorNode<T>? = node
            while (ancestor?.parent != null && ancestor.parent!!.right == ancestor) {
                ancestor = ancestor.parent
            }
            return ancestor?.parent
        }

        internal fun <T> previous(node: FourColorNode<T>): FourColorNode<T>? {
            if (node.left != null) {
                var child = node.left
                while (child?.right != null) {
                    child = child.right
                }
                return child
            }

            var ancestor: FourColorNode<T>? = node
            while (ancestor?.parent != null && ancestor.parent!!.left == ancestor) {
                ancestor = ancestor.parent
            }
            return ancestor?.parent
        }
    }
}
