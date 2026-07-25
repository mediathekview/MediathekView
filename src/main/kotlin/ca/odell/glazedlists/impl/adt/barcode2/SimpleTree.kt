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

import ca.odell.glazedlists.GlazedLists

internal class SimpleTree<T>(val comparator: Comparator<in T>) {
    private var root: SimpleNode<T>? = null
    private val zeroQueue = ArrayList<SimpleNode<T>>()

    constructor() : this(comparableComparator())

    operator fun get(index: Int): Element<T> {
        if (root == null) throw IndexOutOfBoundsException()

        var currentNode = root
        var currentIndex = index
        while (true) {
            assert(currentNode != null)
            assert(currentIndex >= 0)

            val node = currentNode!!
            val nodeLeft = node.left
            val leftSize = nodeLeft?.count1 ?: 0
            if (currentIndex < leftSize) {
                currentNode = nodeLeft
                continue
            } else {
                currentIndex -= leftSize
            }

            val size = 1
            if (currentIndex < size) {
                return node
            } else {
                currentIndex -= size
            }

            currentNode = node.right
        }
    }

    fun add(index: Int, value: T, size: Int): Element<T> {
        assert(index >= 0)
        assert(index <= size())
        assert(size >= 0)

        if (root == null) {
            if (index != 0) throw IndexOutOfBoundsException()

            root = SimpleNode.create(size, value, null)
            validateInvariantsIfAssertionsEnabled()
            return root!!
        }

        val inserted = insertIntoSubtree(root!!, index, value, size)
        validateInvariantsIfAssertionsEnabled()
        return inserted
    }

    private fun insertIntoSubtree(parent: SimpleNode<T>, index: Int, value: T, size: Int): SimpleNode<T> {
        var currentParent = parent
        var currentIndex = index
        while (true) {
            assert(currentIndex >= 0)

            val parentLeft = currentParent.left
            val parentLeftSize = parentLeft?.count1 ?: 0
            val parentRightStartIndex = parentLeftSize + 1

            if (currentIndex <= parentLeftSize) {
                if (parentLeft == null) {
                    val inserted = SimpleNode.create(size, value, currentParent)
                    currentParent.left = inserted
                    fixCountsThruRoot(currentParent, size)
                    fixHeightPostChange(currentParent, false)
                    return inserted
                }

                currentParent = parentLeft
                continue
            }

            val parentSize = currentParent.count1
            assert(currentIndex <= parentSize)
            val parentRight = currentParent.right

            if (parentRight == null) {
                val inserted = SimpleNode.create(size, value, currentParent)
                currentParent.right = inserted
                fixCountsThruRoot(currentParent, size)
                fixHeightPostChange(currentParent, false)
                return inserted
            }

            currentParent = parentRight
            currentIndex -= parentRightStartIndex
        }
    }

    fun addInSortedOrder(@Suppress("UNUSED_PARAMETER") color: Byte, value: T, size: Int): Element<T> {
        assert(size >= 0)

        if (root == null) {
            root = SimpleNode.create(size, value, null)
            validateInvariantsIfAssertionsEnabled()
            return root!!
        }

        val inserted = insertIntoSubtreeInSortedOrder(root!!, value, size)
        validateInvariantsIfAssertionsEnabled()
        return inserted
    }

    private fun insertIntoSubtreeInSortedOrder(parent: SimpleNode<T>, value: T, size: Int): SimpleNode<T> {
        var currentParent = parent
        while (true) {
            val sortSide: Int
            var currentFollower: SimpleNode<T>? = currentParent
            while (true) {
                if (currentFollower == null) {
                    sortSide = -1
                    break
                } else if (currentFollower.sorted == Element.SORTED) {
                    sortSide = comparator.compare(value, currentFollower.get())
                    break
                }
                currentFollower = next(currentFollower)
            }

            var insertOnLeft = false
            insertOnLeft = insertOnLeft || sortSide < 0
            insertOnLeft = insertOnLeft || sortSide == 0 && currentParent.left == null
            insertOnLeft =
                insertOnLeft ||
                        sortSide == 0 &&
                        currentParent.right != null &&
                        currentParent.left!!.height < currentParent.right!!.height
            if (insertOnLeft) {
                val parentLeft = currentParent.left
                if (parentLeft == null) {
                    val inserted = SimpleNode.create(size, value, currentParent)
                    currentParent.left = inserted
                    fixCountsThruRoot(currentParent, size)
                    fixHeightPostChange(currentParent, false)
                    return inserted
                }

                currentParent = parentLeft
                continue
            }

            val parentRight = currentParent.right
            if (parentRight == null) {
                val inserted = SimpleNode.create(size, value, currentParent)
                currentParent.right = inserted
                fixCountsThruRoot(currentParent, size)
                fixHeightPostChange(currentParent, false)
                return inserted
            }

            currentParent = parentRight
        }
    }

    private fun fixCountsThruRoot(node: SimpleNode<T>?, delta: Int) {
        var currentNode = node
        while (currentNode != null) {
            currentNode.count1 += delta
            currentNode = currentNode.parent
        }
    }

    private fun fixHeightPostChange(node: SimpleNode<T>?, allTheWayToRoot: Boolean) {
        var currentNode = node
        while (currentNode != null) {
            var leftHeight: Int = currentNode.left?.height?.toInt() ?: 0
            var rightHeight: Int = currentNode.right?.height?.toInt() ?: 0

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

    private fun rotateLeft(subtreeRoot: SimpleNode<T>): SimpleNode<T> {
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
        subtreeRoot.refreshCounts(!zeroQueue.contains(subtreeRoot))

        val newSubtreeRootLeftHeight = newSubtreeRoot.left?.height?.toInt() ?: 0
        val newSubtreeRootRightHeight = newSubtreeRoot.right?.height?.toInt() ?: 0
        newSubtreeRoot.height = (maxOf(newSubtreeRootLeftHeight, newSubtreeRootRightHeight) + 1).toByte()
        newSubtreeRoot.refreshCounts(!zeroQueue.contains(newSubtreeRoot))

        return newSubtreeRoot
    }

    private fun rotateRight(subtreeRoot: SimpleNode<T>): SimpleNode<T> {
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
        subtreeRoot.refreshCounts(!zeroQueue.contains(subtreeRoot))

        val newSubtreeRootLeftHeight = newSubtreeRoot.left?.height?.toInt() ?: 0
        val newSubtreeRootRightHeight = newSubtreeRoot.right?.height?.toInt() ?: 0
        newSubtreeRoot.height = (maxOf(newSubtreeRootLeftHeight, newSubtreeRootRightHeight) + 1).toByte()
        newSubtreeRoot.refreshCounts(!zeroQueue.contains(newSubtreeRoot))

        return newSubtreeRoot
    }

    fun remove(element: Element<T>) {
        val node = element as SimpleNode<T>

        assert(root != null)

        fixCountsThruRoot(node, -1)
        zeroQueue.add(node)
        drainZeroQueue()

        validateInvariantsIfAssertionsEnabled()
    }

    fun remove(index: Int, size: Int) {
        if (size == 0) return
        assert(index >= 0)
        assert(index + size <= size())
        assert(root != null)

        removeFromSubtree(root!!, index, size)
        drainZeroQueue()

        validateInvariantsIfAssertionsEnabled()
    }

    private fun drainZeroQueue() {
        for (node in zeroQueue) {
            if (node.right == null) {
                replaceChild(node, node.left)
            } else if (node.left == null) {
                replaceChild(node, node.right)
            } else {
                replaceEmptyNodeWithChild(node)
            }
        }
        zeroQueue.clear()
    }

    private fun removeFromSubtree(node: SimpleNode<T>, index: Int, size: Int) {
        var currentNode = node
        var currentIndex = index
        var remainingSize = size
        while (remainingSize > 0) {
            assert(currentIndex >= 0)

            val nodeLeft = currentNode.left
            var leftSize = nodeLeft?.count1 ?: 0

            if (currentIndex < leftSize) {
                if (currentIndex + remainingSize > leftSize) {
                    val toRemove = leftSize - currentIndex
                    removeFromSubtree(nodeLeft!!, currentIndex, toRemove)
                    remainingSize -= toRemove
                    leftSize -= toRemove
                } else {
                    currentNode = nodeLeft!!
                    continue
                }
            }
            assert(currentIndex >= leftSize)

            var rightStartIndex = leftSize + 1
            if (currentIndex < rightStartIndex) {
                val toRemove = minOf(rightStartIndex - currentIndex, remainingSize)

                remainingSize -= toRemove
                rightStartIndex -= toRemove
                fixCountsThruRoot(currentNode, -toRemove)
                zeroQueue.add(currentNode)
                if (remainingSize == 0) return
            }
            assert(currentIndex >= rightStartIndex)

            currentIndex -= rightStartIndex
            currentNode = currentNode.right!!
        }
    }

    private fun replaceChild(node: SimpleNode<T>, replacement: SimpleNode<T>?) {
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

    private fun replaceEmptyNodeWithChild(toReplace: SimpleNode<T>): SimpleNode<T> {
        assert(toReplace.left != null)
        assert(toReplace.right != null)

        var replacement = toReplace.left!!
        while (replacement.right != null) {
            replacement = replacement.right!!
        }
        assert(replacement.right == null)

        fixCountsThruRoot(replacement, -1)
        replaceChild(replacement, replacement.left)

        replacement.left = toReplace.left
        if (replacement.left != null) replacement.left!!.parent = replacement
        replacement.right = toReplace.right
        if (replacement.right != null) replacement.right!!.parent = replacement
        replacement.height = toReplace.height
        replacement.refreshCounts(!zeroQueue.contains(replacement))
        replaceChild(toReplace, replacement)
        fixCountsThruRoot(replacement.parent, 1)

        return replacement
    }

    fun set(index: Int, value: T, size: Int): Element<T> {
        remove(index, size)
        return add(index, value, size)
    }

    fun clear() {
        root = null
    }

    fun indexOfNode(element: Element<T>, @Suppress("UNUSED_PARAMETER") colorsOut: Byte): Int {
        var node = element as SimpleNode<T>

        var index = node.left?.count1 ?: 0
        while (node.parent != null) {
            if (node.parent!!.right == node) {
                index += node.parent!!.left?.count1 ?: 0
                index += 1
            }
            node = node.parent!!
        }

        return index
    }

    fun indexOfValue(
        element: T,
        firstIndex: Boolean,
        simulated: Boolean,
        @Suppress("UNUSED_PARAMETER") colorsOut: Byte,
    ): Int {
        var result = 0
        var found = false

        var node = root
        while (true) {
            if (node == null) {
                if (found && !firstIndex) result--
                return if (found || simulated) result else -1
            }

            val comparison = comparator.compare(element, node.get())
            if (comparison < 0) {
                node = node.left
                continue
            }
            val nodeLeft = node.left

            if (comparison == 0) {
                found = true
                if (firstIndex) {
                    node = nodeLeft
                    continue
                }
            }

            result += nodeLeft?.count1 ?: 0
            result += 1
            node = node.right
        }
    }

    fun convertIndexColor(
        index: Int,
        @Suppress("UNUSED_PARAMETER") indexColors: Byte,
        @Suppress("UNUSED_PARAMETER") colorsOut: Byte,
    ): Int {
        if (root == null) {
            if (index == 0) return 0
            throw IndexOutOfBoundsException()
        }

        var result = 0
        var currentNode = root
        var currentIndex = index
        while (true) {
            assert(currentNode != null)
            assert(currentIndex >= 0)

            val node = currentNode!!
            val nodeLeft = node.left
            val leftSize = nodeLeft?.count1 ?: 0

            if (currentIndex < leftSize) {
                currentNode = nodeLeft
                continue
            } else {
                if (nodeLeft != null) result += nodeLeft.count1
                currentIndex -= leftSize
            }

            val size = 1
            if (currentIndex < size) {
                result += currentIndex
                return result
            } else {
                result += 1
                currentIndex -= size
            }

            currentNode = node.right
        }
    }

    fun size(): Int = root?.count1 ?: 0

    override fun toString(): String = root?.toString() ?: ""

    internal fun firstNode(): SimpleNode<T>? {
        if (root == null) return null

        var result = root
        while (result?.left != null) {
            result = result.left
        }
        return result
    }

    private fun validateInvariantsIfAssertionsEnabled() {
        if (ASSERTIONS_ENABLED) {
            valid()
        }
    }

    private fun valid(): Boolean {
        var node = firstNode()
        while (node != null) {
            val originalCount1 = node.count1

            node.refreshCounts(!zeroQueue.contains(node))

            assert(originalCount1 == node.count1) {
                "Incorrect count 0 on node: \n$node\n Expected ${node.count1} but was $originalCount1"
            }

            val leftHeight = node.left?.height?.toInt() ?: 0
            val rightHeight = node.right?.height?.toInt() ?: 0
            assert(maxOf(leftHeight, rightHeight) + 1 == node.height.toInt())
            assert(node.left == null || node.left!!.parent == node)
            assert(node.right == null || node.right!!.parent == node)
            assert(kotlin.math.abs(leftHeight - rightHeight) < 2) { "Subtree is not AVL: \n$node" }

            node = next(node)
        }

        return true
    }

    companion object {
        private val ASSERTIONS_ENABLED = SimpleTree::class.java.desiredAssertionStatus()

        @Suppress("UNCHECKED_CAST")
        private fun <T> comparableComparator(): Comparator<in T> =
            GlazedLists.comparableComparator<Comparable<Any?>>() as Comparator<in T>

        internal fun <T> next(node: SimpleNode<T>): SimpleNode<T>? {
            if (node.right != null) {
                var child = node.right
                while (child?.left != null) {
                    child = child.left
                }
                return child
            }

            var ancestor: SimpleNode<T>? = node
            while (ancestor?.parent != null && ancestor.parent!!.right == ancestor) {
                ancestor = ancestor.parent
            }
            return ancestor?.parent
        }

        internal fun <T> previous(node: SimpleNode<T>): SimpleNode<T>? {
            if (node.left != null) {
                var child = node.left
                while (child?.right != null) {
                    child = child.right
                }
                return child
            }

            var ancestor: SimpleNode<T>? = node
            while (ancestor?.parent != null && ancestor.parent!!.left == ancestor) {
                ancestor = ancestor.parent
            }
            return ancestor?.parent
        }
    }
}
