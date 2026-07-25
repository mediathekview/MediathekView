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
 * A BarcodeIterator is a specialized [MutableIterator] implementation for
 * moving over a [Barcode] efficiently.
 *
 * @author [Kevin Maltby](mailto:kevin@swank.ca)
 */
internal class BarcodeIterator private constructor(
    private val barcode: Barcode,
) : MutableIterator<Any> {
    private var currentNode: BarcodeNode? = barcode.getRootNode()
    private var localIndex = -1
    private var blackSoFar = 0
    private var whiteSoFar = 0

    @get:JvmName("getIndexProperty")
    val index: Int
        get() = getIndex()

    @get:JvmName("getBlackIndexProperty")
    val blackIndex: Int
        get() = getBlackIndex()


    init {
        if (currentNode != null) {
            while (currentNode!!.left != null) {
                currentNode = currentNode!!.left
            }
        }
    }

    override fun hasNext(): Boolean = index != barcode.size() - 1

    fun hasNextBlack(): Boolean = index < barcode.treeSize() - 1 && currentNode != null

    fun hasNextWhite(): Boolean {
        val node = currentNode
        return if (barcode.size() != barcode.treeSize()) {
            hasNext()
        } else if (node == null) {
            false
        } else {
            localIndex < node.whiteSpace - 1 || whiteSoFar + node.whiteSpace < barcode.whiteSize()
        }
    }

    fun hasNextColour(colour: Any?): Boolean = if (colour === Barcode.BLACK) hasNextBlack() else hasNextWhite()

    override fun next(): Any {
        localIndex++
        val node = currentNode
        if (node == null) {
            return if (index < barcode.size()) {
                Barcode.WHITE
            } else {
                throw NoSuchElementException()
            }
        } else if (localIndex >= node.whiteSpace + node.rootSize) {
            if (index < barcode.treeSize()) {
                blackSoFar += node.rootSize
                whiteSoFar += node.whiteSpace
                findNextNode()
                localIndex = 0
            } else {
                return if (index < barcode.size()) {
                    Barcode.WHITE
                } else {
                    throw NoSuchElementException()
                }
            }
        }

        val current = currentNode!!
        return if (localIndex < current.whiteSpace) Barcode.WHITE else Barcode.BLACK
    }

    fun nextBlack(): Any {
        localIndex++
        val node = currentNode ?: throw NoSuchElementException()
        if (localIndex < node.whiteSpace) {
            localIndex = node.whiteSpace
        } else if (localIndex >= node.whiteSpace + node.rootSize) {
            if (index < barcode.treeSize()) {
                whiteSoFar += node.whiteSpace
                blackSoFar += node.rootSize
                findNextNode()
                localIndex = currentNode!!.whiteSpace
            } else {
                throw NoSuchElementException()
            }
        }
        if (localIndex < currentNode!!.whiteSpace) throw IllegalStateException()
        return Barcode.BLACK
    }

    fun nextWhite(): Any {
        localIndex++
        val node = currentNode
        if (node == null) {
            return if (index < barcode.size()) {
                Barcode.WHITE
            } else {
                throw NoSuchElementException()
            }
        } else if (localIndex >= node.whiteSpace) {
            if (index < barcode.treeSize() && index + node.rootSize >= barcode.treeSize()) {
                localIndex = node.whiteSpace + node.rootSize
            }

            if (index < barcode.treeSize()) {
                blackSoFar += node.rootSize
                whiteSoFar += node.whiteSpace
                findNextNode()
                localIndex = 0
            } else {
                return if (index < barcode.size()) {
                    Barcode.WHITE
                } else {
                    throw NoSuchElementException()
                }
            }
        }
        if (localIndex >= currentNode!!.whiteSpace) throw IllegalStateException()
        return Barcode.WHITE
    }

    fun nextColour(colour: Any?): Any = if (colour === Barcode.BLACK) nextBlack() else nextWhite()

    override fun remove() {
        if (localIndex == -1) {
            throw NoSuchElementException("Cannot call remove() before next() is called.")
        } else if (currentNode == null || index >= barcode.treeSize()) {
            barcode.remove(index, 1)
            localIndex--
        } else {
            val node = currentNode!!
            if (localIndex == 0 && node.whiteSpace == 1 && index != 0) {
                findPreviousNode()
                blackSoFar -= currentNode!!.rootSize
                whiteSoFar -= currentNode!!.whiteSpace
                localIndex += currentNode!!.whiteSpace + currentNode!!.rootSize
            } else if (localIndex == node.whiteSpace && node.rootSize == 1) {
                if (localIndex == barcode.treeSize() - 1) {
                    currentNode = null
                } else if (index == barcode.treeSize() - 1) {
                    findPreviousNode()
                    blackSoFar -= currentNode!!.rootSize
                    whiteSoFar -= currentNode!!.whiteSpace
                    localIndex += currentNode!!.whiteSpace + currentNode!!.rootSize
                } else {
                    findNextNode()
                }
            }
            node.removeBaseCase(index, localIndex)
            localIndex--
        }
    }

    fun setWhite(): Int {
        if (localIndex == -1) {
            throw NoSuchElementException("Cannot call setWhite() before next() is called.")
        } else if (currentNode == null || index >= barcode.treeSize() || localIndex < currentNode!!.whiteSpace) {
            return getWhiteIndex()
        } else if (index != barcode.treeSize() - 1) {
            val node = currentNode!!
            if (node.rootSize == 1) {
                findNextNode()
                node.setWhite(index, localIndex, 1)
                if (barcode.getRootNode() != null) barcode.treeSizeChanged()
                if (currentNode!!.whiteSpace == 0 && currentNode!!.rootSize == 0) currentNode = node
                return whiteSoFar + localIndex
            } else if (localIndex == node.whiteSpace) {
                node.setWhite(index, localIndex, 1)
                if (barcode.getRootNode() != null) barcode.treeSizeChanged()
                return whiteSoFar + localIndex
            } else {
                node.setWhite(index, localIndex, 1)
                if (barcode.getRootNode() != null) barcode.treeSizeChanged()
                blackSoFar += node.rootSize
                whiteSoFar += node.whiteSpace
                findNextNode()
                localIndex = 0
                return whiteSoFar
            }
        } else if (currentNode!!.rootSize == 1) {
            val node = currentNode!!
            if (node.whiteSpace + 1 == barcode.treeSize()) {
                currentNode = null
                node.setWhite(index, localIndex, 1)
                if (barcode.getRootNode() != null) barcode.treeSizeChanged()
                return localIndex
            } else {
                findPreviousNode()
                val currentLocalIndex = localIndex
                blackSoFar -= currentNode!!.rootSize
                whiteSoFar -= currentNode!!.whiteSpace
                localIndex += currentNode!!.whiteSpace + currentNode!!.rootSize
                node.setWhite(index, currentLocalIndex, 1)
                if (barcode.getRootNode() != null) barcode.treeSizeChanged()
                if (currentNode!!.whiteSpace == 0 && currentNode!!.rootSize == 0) currentNode = node
                return whiteSoFar + localIndex - currentNode!!.rootSize
            }
        } else {
            currentNode!!.setWhite(index, localIndex, 1)
            if (barcode.getRootNode() != null) barcode.treeSizeChanged()
            return whiteSoFar + localIndex - currentNode!!.rootSize
        }
    }

    fun setBlack(): Int {
        if (localIndex == -1) {
            throw NoSuchElementException("Cannot call setBlack() before next() is called.")
        } else if (currentNode == null) {
            barcode.setBlack(index, 1)
            currentNode = barcode.getRootNode()
            return 0
        } else if (index == barcode.treeSize()) {
            barcode.setBlack(index, 1)
            if (barcode.getRootNode() != null) barcode.treeSizeChanged()
        } else if (index > barcode.treeSize()) {
            val node = currentNode!!
            barcode.setBlack(index, 1)
            if (barcode.getRootNode() != null) barcode.treeSizeChanged()
            whiteSoFar += node.whiteSpace
            blackSoFar += node.rootSize
            localIndex -= node.whiteSpace + node.rootSize
            findNextNode()
        } else if (localIndex < currentNode!!.whiteSpace && currentNode!!.whiteSpace == 1) {
            val node = currentNode!!
            if (index == 0) {
                node.setBlack(index, localIndex, 1)
                if (barcode.getRootNode() != null) barcode.treeSizeChanged()
            } else {
                findPreviousNode()
                val currentLocalIndex = localIndex
                blackSoFar -= currentNode!!.rootSize
                whiteSoFar -= currentNode!!.whiteSpace
                localIndex += currentNode!!.whiteSpace + currentNode!!.rootSize
                node.setBlack(index, currentLocalIndex, 1)
                if (barcode.getRootNode() != null) barcode.treeSizeChanged()
                if (currentNode!!.whiteSpace == 0 && currentNode!!.rootSize == 0) currentNode = node
            }
        } else if (localIndex < currentNode!!.whiteSpace) {
            val node = currentNode!!
            when (localIndex) {
                0 -> {
                    node.setBlack(index, localIndex, 1)
                    if (barcode.getRootNode() != null) barcode.treeSizeChanged()
                    blackSoFar++
                    localIndex--
                    return blackSoFar - 1
                }

                node.whiteSpace - 1 -> {
                    node.setBlack(index, localIndex, 1)
                    if (barcode.getRootNode() != null) barcode.treeSizeChanged()
                    return blackSoFar
                }

                else -> {
                    node.setBlack(index, localIndex, 1)
                    if (barcode.getRootNode() != null) barcode.treeSizeChanged()
                    whiteSoFar += localIndex
                    blackSoFar++
                    localIndex = -1
                    return blackSoFar - 1
                }
            }
        }
        return blackIndex
    }

    fun set(colour: Any?): Int = if (colour === Barcode.BLACK) setBlack() else setWhite()

    fun getIndex(): Int = blackSoFar + whiteSoFar + localIndex

    fun getBlackIndex(): Int {
        val node = currentNode
        return if (localIndex == -1) {
            blackSoFar - 1
        } else if (node == null || localIndex < node.whiteSpace || localIndex >= node.whiteSpace + node.rootSize) {
            -1
        } else {
            blackSoFar + localIndex - node.whiteSpace
        }
    }

    fun getWhiteIndex(): Int {
        val node = currentNode
        return if (node == null) {
            if (localIndex == -1 && whiteSoFar != 0) whiteSoFar - 1 else localIndex
        } else if (localIndex >= node.whiteSpace && localIndex < node.whiteSpace + node.rootSize) {
            -1
        } else if (localIndex >= node.whiteSpace + node.rootSize) {
            whiteSoFar + localIndex - node.rootSize
        } else {
            whiteSoFar + localIndex
        }
    }

    fun getColourIndex(colour: Any?): Int = if (colour === Barcode.WHITE) getWhiteIndex() else blackIndex

    companion object {
        @JvmSynthetic
        internal fun create(barcode: Barcode): BarcodeIterator = BarcodeIterator(barcode)
    }

    private fun findNextNode() {
        val node = currentNode ?: throw IllegalStateException()
        if (node.right != null) {
            currentNode = node.right
            while (currentNode!!.left != null) {
                currentNode = currentNode!!.left
            }
        } else if (node.parent!!.left === node) {
            currentNode = node.parent
        } else if (node.parent!!.right === node) {
            currentNode = node
            while (currentNode!!.parent!!.right === currentNode) {
                currentNode = currentNode!!.parent
            }
            currentNode = currentNode!!.parent
        } else {
            throw IllegalStateException()
        }
    }

    private fun findPreviousNode() {
        val node = currentNode ?: throw IllegalStateException()
        if (node.left != null) {
            currentNode = node.left
            while (currentNode!!.right != null) {
                currentNode = currentNode!!.right
            }
        } else if (node.parent!!.right === node) {
            currentNode = node.parent
        } else if (node.parent!!.left === node) {
            currentNode = node
            while (currentNode!!.parent!!.left === currentNode) {
                currentNode = currentNode!!.parent
            }
            currentNode = currentNode!!.parent
        } else {
            throw IllegalStateException()
        }
    }
}
