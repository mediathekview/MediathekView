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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.impl.adt

internal class SparseListNode private constructor(
    host: SparseList,
    parent: SparseListNode?,
    value: Any?,
) {
    private var parent: SparseListNode? = parent
    private var host: SparseList? = host
    private var left: SparseListNode? = null
    private var right: SparseListNode? = null
    private var totalRightSize = 0
    private var totalLeftSize = 0
    private var emptySpace = 0
    private var height = 1

    var value: Any? = value
        private set

    val index: Int
        get() = if (parent != null) parent!!.getIndex(this) + totalLeftSize + emptySpace else totalLeftSize + emptySpace

    companion object {
        @JvmSynthetic
        internal fun create(host: SparseList, parent: SparseListNode?, value: Any?, emptySpace: Int = 0): SparseListNode =
            SparseListNode(host, parent, value).apply {
                this.emptySpace = emptySpace
            }

        @JvmSynthetic
        internal fun createIterator(sparseList: SparseList, root: SparseListNode?): MutableIterator<Any?> =
            SparseListIterator(sparseList, root)
    }

    @JvmSynthetic
    internal fun size(): Int = totalLeftSize + emptySpace + totalRightSize + 1

    @JvmSynthetic
    internal fun insert(index: Int, value: Any?) {
        val localizedIndex = index - totalLeftSize
        when {
            localizedIndex < 0 -> {
                totalLeftSize++
                left!!.insert(index, value)
            }

            localizedIndex > emptySpace -> {
                totalRightSize++
                right!!.insert(localizedIndex - emptySpace - 1, value)
            }

            localizedIndex < emptySpace -> {
                emptySpace -= localizedIndex
                totalLeftSize += localizedIndex + 1
                if (left == null) {
                    left = create(host!!, this, value, localizedIndex)
                    ensureAVL()
                } else {
                    left!!.insertAtEnd(value, localizedIndex)
                }
            }

            else -> insertAtThisNode(value)
        }
    }

    private fun insertAtThisNode(value: Any?) {
        val replacement = create(host!!, parent, value, emptySpace)
        emptySpace = 0
        replacement.height = height
        height = 1
        replacement.totalRightSize = totalRightSize + 1
        replacement.left = left
        if (left != null) {
            replacement.left!!.parent = replacement
            replacement.totalLeftSize = totalLeftSize
            totalLeftSize = 0
            left = null
        }

        if (parent == null) host!!.setRootNode(replacement) else parent!!.replace(this, replacement)

        if (right == null) {
            parent = replacement
            replacement.right = this
            replacement.ensureAVL()
        } else {
            replacement.right = right
            replacement.right!!.parent = replacement
            totalRightSize = 0
            right = null
            replacement.right!!.moveToSmallest(this)
        }
    }

    @JvmSynthetic
    internal fun insertAtEnd(value: Any?, leadingNulls: Int) {
        totalRightSize += leadingNulls + 1
        if (right != null) {
            right!!.insertAtEnd(value, leadingNulls)
        } else {
            right = create(host!!, this, value, leadingNulls)
            ensureAVL()
        }
    }

    @JvmSynthetic
    internal fun insertEmptySpace(index: Int, length: Int) {
        val localizedIndex = index - totalLeftSize
        when {
            localizedIndex < 0 -> {
                totalLeftSize += length
                left!!.insertEmptySpace(index, length)
            }

            localizedIndex > emptySpace -> {
                totalRightSize += length
                right!!.insertEmptySpace(localizedIndex - emptySpace - 1, length)
            }

            else -> emptySpace += length
        }
    }

    private fun moveToSmallest(movingNode: SparseListNode) {
        totalLeftSize += movingNode.emptySpace + 1
        if (left != null) {
            left!!.moveToSmallest(movingNode)
        } else {
            movingNode.parent = this
            left = movingNode
            ensureAVL()
        }
    }

    private fun getIndex(child: SparseListNode): Int =
        if (child === left) {
            if (parent != null) parent!!.getIndex(this) else 0
        } else {
            if (parent != null) parent!!.getIndex(this) + totalLeftSize + emptySpace + 1 else totalLeftSize + emptySpace + 1
        }

    @JvmSynthetic
    internal fun getNode(index: Int): SparseListNode? {
        val localizedIndex = index - totalLeftSize
        return when {
            localizedIndex < 0 -> left!!.getNode(index)
            localizedIndex > emptySpace -> right!!.getNode(localizedIndex - emptySpace - 1)
            localizedIndex < emptySpace -> null
            else -> this
        }
    }

    private fun replaceValue(value: Any?): Any? =
        if (value != null) {
            val oldValue = this.value
            this.value = value
            oldValue
        } else {
            emptySpace++
            unlink()
        }

    @JvmSynthetic
    internal fun set(index: Int, value: Any?): Any? {
        val localizedIndex = index - totalLeftSize
        return when {
            localizedIndex < 0 -> left!!.set(index, value)
            localizedIndex > emptySpace -> right!!.set(localizedIndex - emptySpace - 1, value)
            localizedIndex < emptySpace -> {
                if (value == null) {
                    null
                } else {
                    emptySpace--
                    insert(index, value)
                    null
                }
            }

            else -> replaceValue(value)
        }
    }

    @JvmSynthetic
    internal fun remove(index: Int): Any? {
        val localizedIndex = index - totalLeftSize
        return when {
            localizedIndex < 0 -> {
                totalLeftSize--
                left!!.remove(index)
            }

            localizedIndex > emptySpace -> {
                totalRightSize--
                right!!.remove(localizedIndex - emptySpace - 1)
            }

            localizedIndex < emptySpace -> {
                emptySpace--
                null
            }

            else -> unlink()
        }
    }

    private fun unlink(): Any? {
        var index = -1
        val replacement: SparseListNode?

        if (right != null && left != null) {
            return unlinkFromTwoChildren()
        } else if (right != null) {
            replacement = right
            replacement!!.parent = parent
            replacement.emptySpace += emptySpace
        } else {
            replacement = left
            replacement?.parent = parent

            if (parent == null) {
                index = if (emptySpace == 0) -1 else host!!.size
            } else if (parent!!.left === this) {
                parent!!.emptySpace += emptySpace
                parent!!.totalLeftSize -= emptySpace
            } else if (emptySpace != 0) {
                index = this.index - emptySpace
            }
        }

        if (parent != null) {
            parent!!.replace(this, replacement)
            parent!!.ensureAVL()
        } else {
            host!!.setRootNode(replacement)
        }

        if (index != -1) {
            if (parent != null) parent!!.prepareForReinsert(false, emptySpace)
            host!!.addNulls(index, emptySpace)
        }
        return clear()
    }

    private fun unlinkFromTwoChildren(): Any? {
        val replacement = right!!.pruneSmallestChild()
        val repParent = replacement.parent
        replacement.emptySpace += emptySpace
        replacement.height = height
        replacement.left = left
        replacement.left!!.parent = replacement
        replacement.totalLeftSize = totalLeftSize
        replacement.parent = parent

        if (parent == null) host!!.setRootNode(replacement) else parent!!.replace(this, replacement)

        if (repParent === this) {
            replacement.ensureAVL()
        } else {
            repParent!!.left = replacement.right
            if (repParent.left != null) repParent.left!!.parent = repParent
            repParent.totalLeftSize = replacement.totalRightSize
            replacement.right = right
            replacement.right!!.parent = replacement
            replacement.totalRightSize = replacement.right!!.size()
            repParent.ensureAVL()
        }
        return clear()
    }

    private fun pruneSmallestChild(): SparseListNode =
        if (left != null) {
            val prunedNode = left!!.pruneSmallestChild()
            totalLeftSize -= prunedNode.emptySpace + 1
            prunedNode
        } else {
            this
        }

    private fun prepareForReinsert(leftChild: Boolean, length: Int) {
        if (leftChild) {
            totalLeftSize -= length
        } else {
            totalRightSize -= length
        }

        if (parent != null) parent!!.prepareForReinsert(parent!!.left === this, length) else host!!.treeSizeChanged()
    }

    private fun clear(): Any? {
        left = null
        totalLeftSize = 0
        right = null
        totalRightSize = 0
        host = null
        parent = null
        emptySpace = 0
        height = -1
        val thisValue = value
        value = null
        return thisValue
    }

    private fun ensureAVL() {
        val oldHeight = height
        recalculateHeight()
        avlRotate()
        if (height != oldHeight && parent != null) parent!!.ensureAVL()
    }

    private fun replace(child: SparseListNode, replacement: SparseListNode?) {
        if (child === left) left = replacement else right = replacement
    }

    private fun recalculateHeight() {
        val leftHeight = left?.height ?: 0
        val rightHeight = right?.height ?: 0
        height = 1 + maxOf(leftHeight, rightHeight)
    }

    private fun avlRotate() {
        val leftHeight = left?.height ?: 0
        val rightHeight = right?.height ?: 0

        if (leftHeight - rightHeight >= 2) {
            val leftLeftHeight = left?.left?.height ?: 0
            val leftRightHeight = left?.right?.height ?: 0
            if (leftRightHeight > leftLeftHeight) left!!.rotateRight()
            rotateLeft()
        } else if (rightHeight - leftHeight >= 2) {
            val rightLeftHeight = right?.left?.height ?: 0
            val rightRightHeight = right?.right?.height ?: 0
            if (rightLeftHeight > rightRightHeight) right!!.rotateLeft()
            rotateRight()
        }
    }

    private fun rotateLeft() {
        val replacement = left!!
        left = replacement.right
        totalLeftSize = replacement.totalRightSize
        if (replacement.right != null) replacement.right!!.parent = this
        replacement.right = this
        replacement.totalRightSize = size()
        if (parent != null) parent!!.replace(this, replacement) else host!!.setRootNode(replacement)
        replacement.parent = parent
        parent = replacement
        recalculateHeight()
        replacement.height = 0
    }

    private fun rotateRight() {
        val replacement = right!!
        right = replacement.left
        totalRightSize = replacement.totalLeftSize
        if (replacement.left != null) replacement.left!!.parent = this
        replacement.left = this
        replacement.totalLeftSize = size()
        if (parent != null) parent!!.replace(this, replacement) else host!!.setRootNode(replacement)
        replacement.parent = parent
        parent = replacement
        recalculateHeight()
        replacement.height = 0
    }

    override fun toString(): String = "[ $left <$emptySpace> $value <$height> $right ]"

    private fun correctSizes(sizeChange: Int) {
        if (parent != null) {
            if (parent!!.left === this) totalLeftSize += sizeChange else totalRightSize += sizeChange
            parent!!.correctSizes(sizeChange)
        } else {
            host!!.treeSizeChanged()
        }
    }

    private class SparseListIterator(
        private val sparseList: SparseList,
        root: SparseListNode?,
    ) : MutableIterator<Any?> {
        private var currentNode: SparseListNode? = null
        private var timesRequested = -1
        private var treeSize = 0
        private val size = sparseList.size
        private var index = -1

        init {
            if (root != null) {
                treeSize = root.size()
                currentNode = root
                while (currentNode!!.left != null) {
                    currentNode = currentNode!!.left
                }
            }
        }

        override fun hasNext(): Boolean = index < treeSize - 1 || index != size - 1

        override fun next(): Any? {
            timesRequested++
            index++

            if (currentNode == null) {
                if (index < size) return null
                throw NoSuchElementException()
            } else if (timesRequested > currentNode!!.emptySpace) {
                if (index < treeSize) {
                    findNextNode()
                    timesRequested = 0
                } else {
                    if (index < size) return null
                    throw NoSuchElementException()
                }
            }

            return when {
                timesRequested < currentNode!!.emptySpace -> null
                timesRequested == currentNode!!.emptySpace -> currentNode!!.value
                else -> throw IllegalStateException()
            }
        }

        override fun remove() {
            when {
                timesRequested == -1 -> throw IllegalStateException("Cannot remove() without a prior call to next()")
                currentNode == null || index >= treeSize -> sparseList.removeAt(index)
                timesRequested < currentNode!!.emptySpace -> {
                    currentNode!!.correctSizes(-1)
                    currentNode!!.emptySpace--
                }

                timesRequested == currentNode!!.emptySpace -> {
                    currentNode!!.correctSizes(-1)
                    val nodeToRemove = currentNode!!
                    findNextNode()
                    timesRequested = -1
                    nodeToRemove.unlink()
                }

                else -> throw IllegalStateException()
            }
        }

        private fun findNextNode() {
            if (currentNode!!.right != null) {
                currentNode = currentNode!!.right
                while (currentNode!!.left != null) {
                    currentNode = currentNode!!.left
                }
            } else if (currentNode!!.parent!!.left === currentNode) {
                currentNode = currentNode!!.parent
            } else if (currentNode!!.parent!!.right === currentNode) {
                while (currentNode!!.parent!!.right === currentNode) {
                    currentNode = currentNode!!.parent
                }
                currentNode = currentNode!!.parent
            } else {
                throw IllegalStateException()
            }
        }

        override fun toString(): String = "Accessing $currentNode for the $timesRequested time."
    }
}
