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

import java.util.*

/**
 * A SparseList is an ADT to complement the CompressableList and IndexedTree
 * ADTs. IndexedTree provides accessible nodes that can recalculate their index
 * on the fly so users can avoid dealing with index offsetting. CompressableList
 * provides list compression capabilites that allow a list to be accessed
 * by both the real index and a compressed index. The compressed index
 * corresponds to the index of the current value as though no nulls exist
 * in the list. While this is a powerful feature, the larger benefits of the
 * compression of nulls are a significant performance boost and smaller footprint
 * for lists that tend towards containing a significant number of nulls.
 *
 * The SparseList was created to provide the indexed accessible nodes
 * as found in IndexedTree while reaping the performance and memory enhancements
 * of CompressableList. These optimizations have been taken several steps further
 * to gain significantly better performance over the current implementation of
 * CompressableList.
 *
 * In an effort to maximize performance, this ADT does NOT validate that
 * arguments passed to methods are valid in any way. While this adds inherent
 * risk to the use of this code, this is a volatile implementation class. As
 * such, it should only be used for internal GlazedList development. It is up
 * to the calling code to do any argument validation which may be necessary. If
 * you are still concerned, consider the benefits. Being in a tree structure
 * means the methods on this ADT are often recursive. Recursively validating
 * arguments makes no sense, and has a real-world impact on performance, while
 * not a Big-Oh impact.
 *
 * Every effort has been made to squeeze the highest performance and smallest
 * footprint out of this data structure. These benefits hopefully don't come at
 * the cost of code clarity or maintainability. The memory usage of this ADT
 * is bound to the number of non-null elements. Null elements have no additional
 * memory impact on the data structure.
 *
 * The intent of this high-performance, low-cost data structure is for
 * improving the scalability of some of the GlazedLists. It is technically
 * possible to scale this ADT above the Integer.MAX_SIZE barrier imposed by
 * integer-based indexing. However, doing so requires particular care in the
 * structuring of the list and should be avoided if possible. It is advised
 * that users do their best to operate within the bounds of the Integer.MAX_SIZE
 * size limit.
 *
 * @author [Kevin Maltby](mailto:kevin@swank.ca)
 */
internal class SparseList : AbstractList<Any?>() {
    private var root: SparseListNode? = null
    private var sizeValue = 0
    private var treeSize = 0

    override val size: Int
        get() = sizeValue

    override fun add(index: Int, element: Any?) {
        when {
            element == null -> addNulls(index, 1)
            root != null -> {
                val currentRoot = root!!
                if (index >= treeSize) {
                    val movingNulls = index - treeSize
                    sizeValue -= movingNulls
                    currentRoot.insertAtEnd(element, movingNulls)
                } else {
                    currentRoot.insert(index, element)
                }
                treeSizeChanged()
            }
            else -> {
                root = SparseListNode.create(this, null, element, index)
                treeSize = index + 1
                sizeValue++
            }
        }
    }

    fun addNulls(index: Int, length: Int) {
        sizeValue += length
        if (root != null && index < treeSize) {
            root!!.insertEmptySpace(index, length)
            treeSize += length
        }
    }

    override fun get(index: Int): Any? = getNode(index)?.value

    fun getNode(index: Int): SparseListNode? =
        if (root != null && index < treeSize) {
            root!!.getNode(index)
        } else {
            null
        }

    override fun set(index: Int, element: Any?): Any? =
        if (root != null && index < treeSize) {
            val returnValue = root!!.set(index, element)
            treeSizeChanged()
            returnValue
        } else {
            if (element == null) {
                null
            } else {
                sizeValue--
                add(index, element)
                null
            }
        }

    override fun removeAt(index: Int): Any? =
        if (root != null && index < treeSize) {
            val returnValue = root!!.remove(index)
            treeSizeChanged()
            returnValue
        } else {
            sizeValue--
            null
        }

    override fun clear() {
        sizeValue = 0
        root = null
    }

    @JvmSynthetic
    internal fun setRootNode(root: SparseListNode?) {
        this.root = root
        if (root == null) {
            sizeValue -= treeSize
            treeSize = 0
        }
    }

    @JvmSynthetic
    internal fun treeSizeChanged() {
        sizeValue -= treeSize
        treeSize = root?.size() ?: 0
        sizeValue += treeSize
    }

    override fun iterator(): MutableIterator<Any?> =
        if (isEmpty()) {
            Collections.emptyIterator()
        } else {
            SparseListNode.createIterator(this, root)
        }
}
