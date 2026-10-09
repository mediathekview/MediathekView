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

/**
 * A Barcode is an ADT to replace the more general CompressableList ADT.
 * CompressableList provides list compression capabilites that allow a list to
 * be accessed by both the real index and a compressed index. The compressed
 * index corresponds to the index of the current value as though no nulls
 * existed in the list.
 *
 * This provides a huge performance boost over ArrayList on partially empty
 * lists. However, the CompressableList is one of the volatile implementation
 * classes for internal development and isn't the best structure for the
 * current usage. The GlazedLists use CompressableList to store only three
 * values: Boolean.TRUE, Boolean.FALSE, and null. As such, it was slower and
 * more memory intensive than it could be due to its general purpose design.
 *
 * The Barcode is designed such that a list of n elements of the same colour
 * will contain at most one node for BLACK and no nodes for WHITE. This will
 * improve the performance and scalability of the GlazedLists which currently
 * make use of CompressableList.
 *
 * Barcode does not support more than two values stored in the list. Three
 * different values are used by one of the GlazedLists at this time. Until
 * UniqueList is refactored to make use of only two values, Barcode cannot
 * completely replace CompressableList and they will exist in parallel.
 *
 * In an effort to maximize performance this ADT does NOT validate that
 * arguments passed to methods are valid in any way. While this adds inherent
 * risk to the use of this code, this is a volatile implementation class. As
 * such, it should only be used for internal GlazedList development. It is up
 * to the calling code to do any argument validation which may be necessary.
 *
 * Every effort has been made to squeeze the highest performance and smallest
 * footprint out of this data structure. These benefits hopefully don't come at
 * the cost of code clarity or maintainability. The memory usage of this ADT is
 * bound to the number of sequences of BLACK elements. WHITE elements have no
 * memory impact on the data structure.
 *
 * @author [Kevin Maltby](mailto:kevin@swank.ca)
 */
internal class Barcode {
    private var root: BarcodeNode? = null
    private var whiteSpace = 0
    private var treeSize = 0

    val isEmpty: Boolean
        get() = size() == 0

    fun validate() {
        root?.validate()
    }

    fun size(): Int = treeSize + whiteSpace

    fun whiteSize(): Int = if (root == null) whiteSpace else root!!.whiteSize() + whiteSpace

    fun blackSize(): Int = if (root == null) 0 else root!!.blackSize()

    fun colourSize(colour: Any?): Int = if (colour === WHITE) whiteSize() else blackSize()

    fun add(index: Int, colour: Any?, length: Int) {
        if (colour === WHITE) {
            addWhite(index, length)
        } else {
            addBlack(index, length)
        }
    }

    fun addWhite(index: Int, length: Int) {
        require(length >= 0) { "length must be non-negative: $length" }
        if (length == 0) return

        if (root == null || index >= treeSize) {
            whiteSpace += length
        } else {
            root!!.insertWhite(index, length)
            treeSizeChanged()
        }
    }

    fun addBlack(index: Int, length: Int) {
        require(length >= 0) { "length must be non-negative: $length" }
        if (length == 0) return

        if (root == null) {
            root = BarcodeNode.create(this, null, length, index)
            treeSize = index + length
            whiteSpace -= index
        } else if (index >= treeSize) {
            val movingWhitespace = index - treeSize
            whiteSpace -= movingWhitespace
            root!!.insertBlackAtEnd(length, movingWhitespace)
            treeSizeChanged()
        } else {
            root!!.insertBlack(index, length)
            treeSizeChanged()
        }
    }

    operator fun get(index: Int): Any = if (getBlackIndex(index) == -1) WHITE else BLACK

    fun set(index: Int, colour: Any?, length: Int) {
        require(length >= 1) { "length must be at least 1: $length" }

        var remainingLength = length
        val trailingChange = if (index > treeSize - 1) remainingLength else index + remainingLength - treeSize
        if (trailingChange > 0) {
            if (colour === BLACK) {
                whiteSpace -= trailingChange
                addBlack(index, trailingChange)
            }
            remainingLength -= trailingChange
            if (remainingLength == 0) return
        }

        if (root != null) {
            root!!.set(index, colour, remainingLength)
            if (root != null) {
                treeSizeChanged()
            }
        }
    }

    fun setWhite(index: Int, length: Int) {
        set(index, WHITE, length)
    }

    fun setBlack(index: Int, length: Int) {
        set(index, BLACK, length)
    }

    fun remove(index: Int, length: Int) {
        require(length >= 1) { "length must be at least 1: $length" }

        var remainingLength = length
        val trailingChange = if (index > treeSize) remainingLength else index + remainingLength - treeSize
        if (trailingChange > 0) {
            whiteSpace -= trailingChange
            remainingLength -= trailingChange
        }

        if (root != null && index < treeSize) {
            while (remainingLength > 0) {
                val oldTreeSize = treeSize
                root!!.remove(index, remainingLength)
                if (root != null) {
                    treeSizeChanged()
                }
                remainingLength -= oldTreeSize - treeSize
            }
            if (root != null) {
                treeSizeChanged()
            }
        }
    }

    fun clear() {
        treeSize = 0
        whiteSpace = 0
        root = null
    }

    @JvmSynthetic
    internal fun getRootNode(): BarcodeNode? = root

    @JvmSynthetic
    internal fun setRootNode(root: BarcodeNode?) {
        this.root = root
        if (root == null) {
            treeSize = 0
        }
    }

    @JvmSynthetic
    internal fun treeSize(): Int = treeSize

    @JvmSynthetic
    internal fun treeSizeChanged() {
        treeSize = root!!.size()
    }

    fun getIndex(colourIndex: Int, colour: Any?): Int =
        if (colour === WHITE) {
            if (root == null) {
                colourIndex
            } else if (colourIndex >= root!!.whiteSize()) {
                colourIndex - root!!.whiteSize() + treeSize
            } else {
                root!!.getIndexByWhiteIndex(colourIndex)
            }
        } else {
            root!!.getIndexByBlackIndex(colourIndex)
        }

    fun getColourIndex(index: Int, colour: Any?): Int =
        if (colour === WHITE) getWhiteIndex(index) else getBlackIndex(index)

    fun getWhiteIndex(index: Int): Int =
        if (root != null && index < treeSize) {
            root!!.getWhiteIndex(index)
        } else {
            if (root != null) index - treeSize + root!!.whiteSize() else index
        }

    fun getBlackIndex(index: Int): Int =
        if (root != null && index < treeSize) root!!.getBlackIndex(index) else -1

    fun getColourIndex(index: Int, left: Boolean, colour: Any?): Int =
        if (colour === WHITE) getWhiteIndex(index, left) else getBlackIndex(index, left)

    fun getWhiteIndex(index: Int, left: Boolean): Int =
        if (root == null) {
            index
        } else if (index >= treeSize) {
            index - treeSize + root!!.whiteSize()
        } else {
            root!!.getWhiteIndex(index, left)
        }

    fun getBlackIndex(index: Int, left: Boolean): Int =
        if (root == null) {
            if (left) -1 else 0
        } else if (index >= treeSize) {
            if (left) root!!.blackSize() - 1 else root!!.blackSize()
        } else {
            root!!.getBlackIndex(index, left)
        }

    fun getWhiteSequenceIndex(whiteIndex: Int): Int =
        if (root == null) {
            whiteIndex
        } else if (whiteIndex >= root!!.whiteSize()) {
            whiteIndex - root!!.whiteSize()
        } else {
            root!!.getWhiteSequenceIndex(whiteIndex)
        }

    fun getBlackBeforeWhite(whiteIndex: Int): Int =
        if (root == null) {
            -1
        } else if (whiteIndex >= root!!.whiteSize()) {
            root!!.blackSize() - 1
        } else {
            root!!.getBlackBeforeWhite(whiteIndex)
        }

    fun findSequenceOfMinimumSize(size: Int, colour: Any?): Int =
        if (root == null) {
            if (colour === BLACK) {
                -1
            } else if (whiteSpace >= size) {
                0
            } else {
                -1
            }
        } else if (colour === BLACK) {
            root!!.findSequenceOfMinimumSize(size, colour)
        } else {
            var result = root!!.findSequenceOfMinimumSize(size, colour)
            if (result == -1 && whiteSpace >= size) {
                result = treeSize
            }
            result
        }

    fun iterator(): BarcodeIterator = BarcodeIterator.create(this)

    override fun toString(): String {
        val result = StringBuilder()
        val iterator = iterator()
        while (iterator.hasNext()) {
            result.append(if (iterator.next() === BLACK) "X" else "_")
        }
        return result.toString()
    }

    companion object {
        @JvmField
        val WHITE: Any = java.lang.Boolean.FALSE

        @JvmField
        val BLACK: Any = java.lang.Boolean.TRUE
    }
}
