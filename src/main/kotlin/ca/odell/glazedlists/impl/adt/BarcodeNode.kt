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
package ca.odell.glazedlists.impl.adt

internal class BarcodeNode private constructor(
    private var host: Barcode?,
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var parent: BarcodeNode?,
) {
    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var left: BarcodeNode? = null

    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var right: BarcodeNode? = null

    private var blackLeftSize = 0
    private var blackRightSize = 0
    private var treeLeftSize = 0
    private var treeRightSize = 0

    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var whiteSpace = 0

    @field:JvmSynthetic
    @get:JvmSynthetic
    @set:JvmSynthetic
    internal var rootSize = 1

    private var height = 1

    companion object {
        @JvmSynthetic
        internal fun create(host: Barcode, parent: BarcodeNode?, values: Int, whiteSpace: Int): BarcodeNode =
            BarcodeNode(host, parent).apply {
                this.whiteSpace = whiteSpace
                rootSize = values
            }
    }

    @JvmSynthetic
    internal fun size(): Int = treeLeftSize + whiteSpace + rootSize + treeRightSize

    @JvmSynthetic
    internal fun blackSize(): Int = blackLeftSize + rootSize + blackRightSize

    @JvmSynthetic
    internal fun whiteSize(): Int = (treeLeftSize - blackLeftSize) + whiteSpace + (treeRightSize - blackRightSize)

    @JvmSynthetic
    internal fun insertBlack(index: Int, length: Int) {
        val localIndex = index - treeLeftSize
        when {
            localIndex < 0 -> {
                blackLeftSize += length
                treeLeftSize += length
                left!!.insertBlack(index, length)
            }

            localIndex > whiteSpace + rootSize -> {
                blackRightSize += length
                treeRightSize += length
                right!!.insertBlack(localIndex - whiteSpace - rootSize, length)
            }

            localIndex == whiteSpace + rootSize -> {
                rootSize += length
            }

            localIndex < whiteSpace -> {
                whiteSpace -= localIndex
                blackLeftSize += length
                treeLeftSize += localIndex + length
                if (left == null) {
                    left = create(host!!, this, length, localIndex)
                    ensureAVL()
                } else {
                    left!!.insertBlackAtEnd(length, localIndex)
                }
            }

            else -> {
                rootSize += length
            }
        }
    }

    @JvmSynthetic
    internal fun insertBlackAtEnd(values: Int, leadingWhite: Int) {
        if (right != null) {
            blackRightSize += values
            treeRightSize += values + leadingWhite
            right!!.insertBlackAtEnd(values, leadingWhite)
        } else if (leadingWhite == 0) {
            rootSize += values
        } else {
            blackRightSize += values
            treeRightSize += values + leadingWhite
            right = create(host!!, this, values, leadingWhite)
            ensureAVL()
        }
    }

    @JvmSynthetic
    internal fun insertWhite(index: Int, length: Int) {
        var localIndex = index - treeLeftSize
        when {
            localIndex < 0 -> {
                treeLeftSize += length
                left!!.insertWhite(index, length)
            }

            localIndex > whiteSpace + rootSize - 1 -> {
                treeRightSize += length
                right!!.insertWhite(localIndex - whiteSpace - rootSize, length)
            }

            localIndex <= whiteSpace -> {
                whiteSpace += length
            }

            else -> {
                localIndex -= whiteSpace
                val movingRoot = rootSize - localIndex
                rootSize = localIndex
                blackRightSize += movingRoot
                treeRightSize += movingRoot + length

                if (right == null) {
                    right = create(host!!, this, movingRoot, length)
                    ensureAVL()
                } else {
                    val node = create(host!!, null, movingRoot, length)
                    right!!.moveToSmallest(node)
                }
            }
        }
    }

    private fun moveToSmallest(movingNode: BarcodeNode) {
        if (left != null) {
            blackLeftSize += movingNode.rootSize
            treeLeftSize += movingNode.whiteSpace + movingNode.rootSize
            left!!.moveToSmallest(movingNode)
        } else if (whiteSpace == 0) {
            rootSize += movingNode.rootSize
            whiteSpace += movingNode.whiteSpace
            movingNode.clear()
        } else {
            blackLeftSize += movingNode.rootSize
            treeLeftSize += movingNode.whiteSpace + movingNode.rootSize
            movingNode.parent = this
            left = movingNode
            ensureAVL()
        }
    }

    @JvmSynthetic
    internal fun getWhiteIndex(index: Int): Int = getWhiteIndex(index, 0)

    private fun getWhiteIndex(index: Int, accumulation: Int): Int {
        val localIndex = index - treeLeftSize
        return when {
            localIndex < 0 -> left!!.getWhiteIndex(index, accumulation)
            localIndex > whiteSpace + rootSize - 1 -> {
                val nextAccumulation = accumulation + (treeLeftSize - blackLeftSize) + whiteSpace
                right!!.getWhiteIndex(localIndex - whiteSpace - rootSize, nextAccumulation)
            }

            localIndex < whiteSpace -> accumulation + (treeLeftSize - blackLeftSize) + localIndex
            else -> -1
        }
    }

    @JvmSynthetic
    internal fun getBlackIndex(index: Int): Int = getBlackIndex(index, 0)

    private fun getBlackIndex(index: Int, accumulation: Int): Int {
        val localIndex = index - treeLeftSize
        return when {
            localIndex < 0 -> left!!.getBlackIndex(index, accumulation)
            localIndex > whiteSpace + rootSize - 1 ->
                right!!.getBlackIndex(localIndex - whiteSpace - rootSize, accumulation + blackLeftSize + rootSize)

            localIndex < whiteSpace -> -1
            else -> accumulation + blackLeftSize + localIndex - whiteSpace
        }
    }

    @JvmSynthetic
    internal fun getWhiteIndex(index: Int, lead: Boolean): Int {
        val localIndex = index - treeLeftSize
        return when {
            localIndex < 0 -> left!!.getWhiteIndex(index, lead)
            localIndex > whiteSpace + rootSize - 1 ->
                right!!.getWhiteIndex(localIndex - whiteSpace - rootSize, lead) + treeLeftSize - blackLeftSize + whiteSpace

            localIndex < whiteSpace -> treeLeftSize - blackLeftSize + localIndex
            lead -> treeLeftSize - blackLeftSize + whiteSpace - 1
            else -> treeLeftSize - blackLeftSize + whiteSpace
        }
    }

    @JvmSynthetic
    internal fun getBlackIndex(index: Int, lead: Boolean): Int {
        val localIndex = index - treeLeftSize
        return when {
            localIndex < 0 -> left!!.getBlackIndex(index, lead)
            localIndex > whiteSpace + rootSize - 1 ->
                right!!.getBlackIndex(localIndex - whiteSpace - rootSize, lead) + blackLeftSize + rootSize

            localIndex < whiteSpace -> if (lead) blackLeftSize - 1 else blackLeftSize
            else -> blackLeftSize + localIndex - whiteSpace
        }
    }

    @JvmSynthetic
    internal fun getIndexByWhiteIndex(whiteIndex: Int): Int {
        val localIndex = whiteIndex - (treeLeftSize - blackLeftSize)
        return when {
            localIndex < 0 -> left!!.getIndexByWhiteIndex(whiteIndex)
            localIndex >= whiteSpace -> right!!.getIndexByWhiteIndex(localIndex - whiteSpace) + treeLeftSize + whiteSpace + rootSize
            else -> treeLeftSize + localIndex
        }
    }

    @JvmSynthetic
    internal fun getIndexByBlackIndex(blackIndex: Int): Int {
        val localIndex = blackIndex - blackLeftSize
        return when {
            localIndex < 0 -> left!!.getIndexByBlackIndex(blackIndex)
            localIndex >= rootSize -> right!!.getIndexByBlackIndex(localIndex - rootSize) + treeLeftSize + whiteSpace + rootSize
            else -> treeLeftSize + whiteSpace + localIndex
        }
    }

    @JvmSynthetic
    internal fun getWhiteSequenceIndex(whiteIndex: Int): Int {
        val localIndex = whiteIndex - (treeLeftSize - blackLeftSize)
        return when {
            localIndex < 0 -> left!!.getWhiteSequenceIndex(whiteIndex)
            localIndex >= whiteSpace -> right!!.getWhiteSequenceIndex(localIndex - whiteSpace)
            else -> localIndex
        }
    }

    @JvmSynthetic
    internal fun getBlackBeforeWhite(whiteIndex: Int): Int {
        val localIndex = whiteIndex - (treeLeftSize - blackLeftSize)
        return when {
            localIndex < 0 -> left!!.getBlackBeforeWhite(whiteIndex)
            localIndex >= whiteSpace -> right!!.getBlackBeforeWhite(localIndex - whiteSpace) + blackLeftSize + rootSize
            else -> blackLeftSize - 1
        }
    }

    @JvmSynthetic
    internal fun findSequenceOfMinimumSize(size: Int, colour: Any?): Int = findFirstFitSequence(size, colour, 0)

    private fun findFirstFitSequence(size: Int, colour: Any?, accumulation: Int): Int {
        var result = -1
        if (left != null) {
            result = left!!.findFirstFitSequence(size, colour, accumulation)
        }
        if (result == -1) {
            if (colour === Barcode.WHITE && size <= whiteSpace) {
                return accumulation + treeLeftSize
            } else if (colour === Barcode.BLACK && size <= rootSize) {
                return accumulation + treeLeftSize + whiteSpace
            }
        }
        if (result == -1 && right != null) {
            result = right!!.findFirstFitSequence(size, colour, accumulation + treeLeftSize + whiteSpace + rootSize)
        }
        return result
    }

    @JvmSynthetic
    internal fun set(index: Int, value: Any?, length: Int) {
        if (length == 1) {
            setBaseCase(index, index, value)
        } else {
            set(index, index, value, length)
        }
    }

    private fun set(absoluteIndex: Int, localIndex: Int, value: Any?, length: Int) {
        val localizedIndex = localIndex - treeLeftSize
        when {
            localizedIndex < 0 -> left!!.set(absoluteIndex, localIndex, value, length)
            localizedIndex > whiteSpace + rootSize - 1 -> right!!.set(absoluteIndex, localizedIndex - whiteSpace - rootSize, value, length)
            value === Barcode.WHITE -> setWhite(absoluteIndex, localizedIndex, length)
            else -> setBlack(absoluteIndex, localizedIndex, length)
        }
    }

    @JvmSynthetic
    internal fun setWhite(absoluteIndex: Int, localIndex: Int, length: Int) {
        val endIndex = localIndex + length - 1
        if (endIndex < whiteSpace) {
            return
        } else if (localIndex > whiteSpace - 1) {
            val rootChange = minOf(length, whiteSpace + rootSize - localIndex)
            if (rootSize == rootChange) {
                whiteSpace += rootChange
                rootSize = 0
                correctSizes(-rootChange, 0)
                unlink(absoluteIndex - localIndex)
            } else {
                rootSize -= rootChange
                if (localIndex < whiteSpace + rootSize) {
                    correctSizes(-rootChange, 0)
                    insertWhite(localIndex + treeLeftSize, rootChange)
                } else {
                    correctSizes(-rootChange, -rootChange)
                    host!!.addWhite(absoluteIndex, rootChange)
                }
            }
            if (rootChange != length) {
                host!!.remove(absoluteIndex + rootChange, length - rootChange)
                host!!.addWhite(absoluteIndex + rootChange, length - rootChange)
            }
        } else if (localIndex < whiteSpace + 1 && endIndex < whiteSpace + rootSize) {
            val rootChange = minOf(length, whiteSpace + rootSize - localIndex) + (localIndex - whiteSpace)
            rootSize -= rootChange
            whiteSpace += rootChange
            correctSizes(-rootChange, 0)
        } else {
            whiteSpace += rootSize
            val localLength = whiteSpace - localIndex
            unlink(absoluteIndex - localIndex)
            if (localLength != length) {
                host!!.remove(absoluteIndex + localLength, length - localLength)
                host!!.addWhite(absoluteIndex + localLength, length - localLength)
            }
        }
    }

    @JvmSynthetic
    internal fun setBlack(absoluteIndex: Int, localIndex: Int, length: Int) {
        val endIndex = localIndex + length - 1
        val localLength = minOf(length, whiteSpace + rootSize - localIndex)
        if (localIndex > whiteSpace - 1) {
            return
        } else if (endIndex > whiteSpace - 1) {
            val whiteChange = whiteSpace - localIndex
            rootSize += whiteChange
            whiteSpace -= whiteChange
            correctSizes(whiteChange, 0)
            compressNode(absoluteIndex - localIndex)
        } else {
            whiteSpace -= length
            correctSizes(0, -length)
            host!!.addBlack(absoluteIndex, length)
            compressNode(absoluteIndex - localIndex)
        }

        if (localLength != length) {
            host!!.remove(absoluteIndex + localLength, length - localLength)
            host!!.addBlack(absoluteIndex + localLength, length - localLength)
        }
    }

    private fun setBaseCase(absoluteIndex: Int, index: Int, value: Any?) {
        val localIndex = index - treeLeftSize
        when {
            localIndex < 0 -> left!!.setBaseCase(absoluteIndex, index, value)
            localIndex > whiteSpace + rootSize -> right!!.setBaseCase(absoluteIndex, localIndex - whiteSpace - rootSize, value)
            localIndex == whiteSpace + rootSize -> {
                if (value !== Barcode.WHITE) {
                    rootSize++
                    treeRightSize--
                    correctSizes(1, 0)
                    right!!.setFirstNullToTrue(absoluteIndex, localIndex - whiteSpace - rootSize + 1)
                }
            }

            localIndex < whiteSpace -> {
                if (value === Barcode.WHITE) return
                whiteSpace--
                correctSizes(1, 0)
                insertBlack(index, 1)
                compressNode(absoluteIndex)
            }

            localIndex == whiteSpace -> {
                if (value === Barcode.WHITE) {
                    whiteSpace++
                    rootSize--
                    correctSizes(-1, 0)
                    if (rootSize == 0) unlink(absoluteIndex - localIndex)
                }
            }

            localIndex == whiteSpace + rootSize - 1 -> {
                if (value === Barcode.WHITE) {
                    rootSize--
                    if (right != null) {
                        treeRightSize++
                        right!!.insertWhite(localIndex - whiteSpace - rootSize, 1)
                        correctSizes(-1, 0)
                    } else if (parent != null && parent!!.left === this) {
                        parent!!.whiteSpace++
                        parent!!.treeLeftSize--
                        parent!!.correctSizes(true, -1, 0)
                    } else {
                        correctSizes(-1, -1)
                        host!!.addWhite(absoluteIndex, 1)
                    }
                }
            }

            else -> {
                if (value === Barcode.WHITE) {
                    rootSize--
                    correctSizes(-1, 0)
                    insertWhite(index, 1)
                }
            }
        }
    }

    private fun setFirstNullToTrue(absoluteIndex: Int, index: Int) {
        val localIndex = index - treeLeftSize
        when {
            localIndex < 0 -> {
                treeLeftSize--
                left!!.setFirstNullToTrue(absoluteIndex, index)
            }

            localIndex > whiteSpace + rootSize - 1 -> {
                treeRightSize--
                right!!.setFirstNullToTrue(absoluteIndex, localIndex - whiteSpace - rootSize)
            }

            else -> {
                whiteSpace--
                compressNode(absoluteIndex)
            }
        }
    }

    @JvmSynthetic
    internal fun remove(index: Int, length: Int) {
        if (length == 1) {
            removeBaseCase(index, index)
        } else {
            remove(index, index, length)
        }
    }

    private fun remove(absoluteIndex: Int, index: Int, length: Int) {
        val localIndex = index - treeLeftSize
        when {
            localIndex < 0 -> left!!.remove(absoluteIndex, index, length)
            localIndex > whiteSpace + rootSize - 1 -> right!!.remove(absoluteIndex, localIndex - whiteSpace - rootSize, length)
            else -> {
                val adjustedLength = minOf(localIndex + length, whiteSpace + rootSize) - localIndex
                val endIndex = localIndex + adjustedLength - 1
                if (localIndex < whiteSpace && endIndex < whiteSpace + rootSize) {
                    val whiteChange = minOf(whiteSpace - localIndex, adjustedLength)
                    val blackChange = maxOf(endIndex - whiteSpace + 1, 0)
                    whiteSpace -= whiteChange
                    rootSize -= blackChange
                    correctSizes(-blackChange, -(whiteChange + blackChange))
                    compressNode(absoluteIndex - localIndex)
                } else if (localIndex > whiteSpace - 1) {
                    if (adjustedLength == rootSize) {
                        unlink(absoluteIndex - localIndex)
                    } else {
                        rootSize -= adjustedLength
                        correctSizes(-adjustedLength, -adjustedLength)
                    }
                } else {
                    val whiteChange = whiteSpace
                    val blackChange = rootSize
                    whiteSpace = 0
                    rootSize = 0
                    correctSizes(-blackChange, -(whiteChange + blackChange))
                    unlink(absoluteIndex - localIndex)
                }
            }
        }
    }

    @JvmSynthetic
    internal fun removeBaseCase(absoluteIndex: Int, index: Int) {
        val localIndex = index - treeLeftSize
        when {
            localIndex < 0 -> {
                treeLeftSize--
                left!!.removeBaseCase(absoluteIndex, index)
            }

            localIndex > whiteSpace + rootSize - 1 -> {
                treeRightSize--
                right!!.removeBaseCase(absoluteIndex, localIndex - whiteSpace - rootSize)
            }

            localIndex < whiteSpace -> {
                whiteSpace--
                compressNode(absoluteIndex)
            }

            else -> {
                rootSize--
                if (rootSize == 0) {
                    rootSize = 1
                    unlink(absoluteIndex - localIndex, false)
                } else {
                    correctSizes(-1, 0)
                }
            }
        }
    }

    private fun unlink(absoluteIndex: Int) {
        unlink(absoluteIndex, true)
    }

    private fun unlink(absoluteIndex: Int, consistent: Boolean) {
        if (right != null && left != null) {
            if (rootSize != 0) correctSizes(-rootSize, -rootSize, consistent)
            unlinkWithTwoChildren()
        } else if (right != null) {
            unlinkWithRightChild(consistent)
        } else {
            val replacement =
                if (left != null) {
                    left!!.also { it.parent = parent }
                } else {
                    null
                }

            if (parent == null) {
                host!!.setRootNode(replacement)
                if (whiteSpace != 0) host!!.addWhite(host!!.size() + 1, whiteSpace)
            } else if (parent!!.left === this) {
                parent!!.whiteSpace += whiteSpace
                parent!!.treeLeftSize -= whiteSpace
                parent!!.left = replacement
                parent!!.ensureAVL()
                if (rootSize != 0) parent!!.correctSizes(true, -rootSize, -rootSize, consistent)
                clear()
            } else {
                parent!!.right = replacement
                parent!!.ensureAVL()
                if (whiteSpace != 0) {
                    parent!!.correctSizes(false, -rootSize, -(whiteSpace + rootSize), consistent)
                    host!!.addWhite(absoluteIndex, whiteSpace)
                } else if (rootSize != 0) {
                    parent!!.correctSizes(false, -rootSize, -rootSize, consistent)
                }
                clear()
            }
        }
    }

    private fun unlinkWithTwoChildren() {
        val replacement = right!!.pruneSmallestChild()
        val repParent = replacement.parent
        whiteSpace += replacement.whiteSpace
        rootSize = replacement.rootSize
        treeRightSize -= replacement.whiteSpace + replacement.rootSize
        blackRightSize -= replacement.rootSize

        if (repParent === this) {
            right = replacement.right
            if (right != null) right!!.parent = this
            ensureAVL()
        } else {
            repParent!!.left = replacement.right
            if (repParent.left != null) repParent.left!!.parent = repParent
            repParent.ensureAVL()
        }
        replacement.clear()
    }

    private fun unlinkWithRightChild(consistent: Boolean) {
        whiteSpace += right!!.whiteSpace
        val oldSize = rootSize
        rootSize = right!!.rootSize
        right!!.clear()
        right = null
        blackRightSize = 0
        treeRightSize = 0
        height = 1
        if (parent != null) {
            if (oldSize != 0) parent!!.correctSizes(parent!!.left === this, -oldSize, -oldSize, consistent)
            parent!!.ensureAVL()
        }
    }

    private fun pruneSmallestChild(): BarcodeNode =
        if (left != null) {
            val prunedNode = left!!.pruneSmallestChild()
            blackLeftSize -= prunedNode.rootSize
            treeLeftSize -= prunedNode.whiteSpace + prunedNode.rootSize
            prunedNode
        } else {
            this
        }

    private fun correctSizes(blackOffset: Int, totalOffset: Int, consistent: Boolean) {
        if (consistent) {
            correctSizes(blackOffset, totalOffset)
        } else {
            correctSizes(-1, totalOffset - blackOffset)
        }
    }

    private fun correctSizes(leftChild: Boolean, blackOffset: Int, totalOffset: Int, consistent: Boolean) {
        if (consistent) {
            correctSizes(leftChild, blackOffset, totalOffset)
        } else {
            correctSizes(leftChild, -1, totalOffset - blackOffset)
        }
    }

    private fun correctSizes(blackOffset: Int, totalOffset: Int) {
        if (parent != null) {
            parent!!.correctSizes(parent!!.left === this, blackOffset, totalOffset)
        } else {
            host!!.treeSizeChanged()
        }
    }

    private fun correctSizes(leftChild: Boolean, blackOffset: Int, totalOffset: Int) {
        if (leftChild) {
            blackLeftSize += blackOffset
            treeLeftSize += totalOffset
        } else {
            blackRightSize += blackOffset
            treeRightSize += totalOffset
        }

        if (parent != null) {
            parent!!.correctSizes(parent!!.left === this, blackOffset, totalOffset)
        } else {
            host!!.treeSizeChanged()
        }
    }

    private fun clear() {
        left = null
        blackLeftSize = 0
        treeLeftSize = 0
        right = null
        blackRightSize = 0
        treeRightSize = 0
        host = null
        parent = null
        whiteSpace = 0
        rootSize = 0
        height = -1
    }

    private fun replace(child: BarcodeNode, replacement: BarcodeNode?) {
        if (child === left) {
            left = replacement
        } else {
            right = replacement
        }
    }

    private fun compressNode(absoluteIndex: Int) {
        if (whiteSpace != 0) return
        if (parent == null) {
            compressRoot(absoluteIndex)
        } else if (parent!!.left === this) {
            compressLeftChild(absoluteIndex)
        } else {
            compressRightChild(absoluteIndex)
        }
    }

    private fun compressRoot(absoluteIndex: Int) {
        if (left != null) {
            if (right == null) {
                left!!.rootSize += rootSize
                left!!.parent = null
                host!!.setRootNode(left)
                clear()
            } else {
                left!!.compressToTheRight(rootSize)
                blackLeftSize += rootSize
                treeLeftSize += rootSize
                rootSize = 0
                unlink(absoluteIndex)
            }
        }
    }

    private fun compressLeftChild(absoluteIndex: Int) {
        if (left != null) {
            left!!.compressToTheRight(rootSize)
            blackLeftSize += rootSize
            treeLeftSize += rootSize
            rootSize = 0
            unlink(absoluteIndex)
        } else {
            if (absoluteIndex == 0) return
            parent!!.left = right
            if (right != null) parent!!.left!!.parent = parent
            parent!!.correctSizes(true, -rootSize, -rootSize)
            parent!!.ensureAVL()
            host!!.addBlack(absoluteIndex - 1, rootSize)
            clear()
        }
    }

    private fun compressRightChild(absoluteIndex: Int) {
        if (left == null) {
            parent!!.blackRightSize -= rootSize
            parent!!.treeRightSize -= rootSize
            parent!!.rootSize += rootSize
            rootSize = 0
            unlink(absoluteIndex)
        } else {
            left!!.compressToTheRight(rootSize)
            blackLeftSize += rootSize
            treeLeftSize += rootSize
            rootSize = 0
            unlink(absoluteIndex)
        }
    }

    private fun compressToTheRight(values: Int) {
        if (right != null) {
            blackRightSize += values
            treeRightSize += values
            right!!.compressToTheRight(values)
        } else {
            rootSize += values
        }
    }

    private fun ensureAVL() {
        val oldHeight = height
        recalculateHeight()
        avlRotate()
        if (height != oldHeight && parent != null) parent!!.ensureAVL()
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
            if (leftRightHeight > leftLeftHeight) {
                left!!.rotateRight()
            }
            rotateLeft()
        } else if (rightHeight - leftHeight >= 2) {
            val rightLeftHeight = right?.left?.height ?: 0
            val rightRightHeight = right?.right?.height ?: 0
            if (rightLeftHeight > rightRightHeight) {
                right!!.rotateLeft()
            }
            rotateRight()
        }
    }

    private fun rotateLeft() {
        val replacement = left!!
        left = replacement.right
        blackLeftSize = replacement.blackRightSize
        treeLeftSize = replacement.treeRightSize
        if (replacement.right != null) replacement.right!!.parent = this
        replacement.right = this
        replacement.blackRightSize = blackSize()
        replacement.treeRightSize = size()
        if (parent != null) {
            parent!!.replace(this, replacement)
        } else {
            host!!.setRootNode(replacement)
        }
        replacement.parent = parent
        parent = replacement
        recalculateHeight()
        replacement.height = 0
    }

    private fun rotateRight() {
        val replacement = right!!
        right = replacement.left
        blackRightSize = replacement.blackLeftSize
        treeRightSize = replacement.treeLeftSize
        if (replacement.left != null) replacement.left!!.parent = this
        replacement.left = this
        replacement.blackLeftSize = blackSize()
        replacement.treeLeftSize = size()
        if (parent != null) {
            parent!!.replace(this, replacement)
        } else {
            host!!.setRootNode(replacement)
        }
        replacement.parent = parent
        parent = replacement
        recalculateHeight()
        replacement.height = 0
    }

    override fun toString(): String =
        "[ $left ($blackLeftSize, $treeLeftSize) <$whiteSpace> $rootSize <$height> ($blackRightSize, $treeRightSize) $right ]"

    @JvmSynthetic
    internal fun validate() {
        validateLineage()
        validateHeight()
        validateTreeSize()
        validateBlackSize()
        validateCompression()
        validateRootSize()
    }

    private fun validateBlackSize(): Int {
        val leftTreeSize = left?.validateBlackSize() ?: 0
        val rightTreeSize = right?.validateBlackSize() ?: 0
        if (leftTreeSize != blackLeftSize) {
            throw IllegalStateException("Black Size Validation Failure in Left Subtree\nExpected: $leftTreeSize\nActual: $blackLeftSize\n$this")
        }
        if (rightTreeSize != blackRightSize) {
            throw IllegalStateException("Black Size Validation Failure in Right Subtree\nExpected: $rightTreeSize\nActual: $blackRightSize\n$this")
        }
        return leftTreeSize + rightTreeSize + rootSize
    }

    private fun validateHeight(): Int {
        val leftHeight = left?.validateHeight() ?: 0
        val rightHeight = right?.validateHeight() ?: 0
        val expectedHeight = 1 + maxOf(leftHeight, rightHeight)
        if (height != expectedHeight) {
            throw IllegalStateException("Height Validation Failure\nExpected: $expectedHeight\nActual: $height\n$this")
        }
        if (kotlin.math.abs(leftHeight - rightHeight) > 1) {
            throw IllegalStateException("AVL Property Validation Failure\n$this")
        }
        return expectedHeight
    }

    private fun validateLineage() {
        if (left != null) {
            if (left!!.parent !== this) throw IllegalStateException("Lineage Validation Failure\nLeft child is orphaned :\n$left")
            left!!.validateLineage()
        }
        if (right != null) {
            if (right!!.parent !== this) throw IllegalStateException("Lineage Validation Failure\nRight child is orphaned :\n$right")
            right!!.validateLineage()
        }
    }

    private fun validateCompression() {
        left?.validateCompression()
        right?.validateCompression()
        if (whiteSpace == 0 && getIndexForValidation() != 0) {
            throw IllegalStateException("Compression Validation Failure\nThe following node was found that could be compressed: \n$this")
        }
    }

    private fun validateTreeSize(): Int {
        val leftTreeSize = left?.validateTreeSize() ?: 0
        val rightTreeSize = right?.validateTreeSize() ?: 0
        if (treeLeftSize != leftTreeSize) {
            throw IllegalStateException("Tree Size Validation Failure\nThe following node was found that had a tree size failure on the left subtree: \n$this")
        }
        if (treeRightSize != rightTreeSize) {
            throw IllegalStateException("Tree Size Validation Failure\nThe following node was found that had a tree size failure on the right subtree: \n$this")
        }
        return treeLeftSize + whiteSpace + rootSize + treeRightSize
    }

    private fun getIndexForValidation(): Int = if (parent != null) parent!!.getIndexForValidation(this) + treeLeftSize else treeLeftSize

    private fun getIndexForValidation(child: BarcodeNode): Int =
        if (child === left) {
            if (parent != null) parent!!.getIndexForValidation(this) else 0
        } else {
            if (parent != null) parent!!.getIndexForValidation(this) + treeLeftSize + whiteSpace + rootSize
            else treeLeftSize + whiteSpace + rootSize
        }

    private fun validateRootSize() {
        left?.validateRootSize()
        right?.validateRootSize()
        if (rootSize == 0) throw IllegalStateException("Root Size Validation Failure\nA node was found with a root size of zero.")
    }
}
