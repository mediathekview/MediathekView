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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.impl.adt.barcode2.Element
import ca.odell.glazedlists.impl.adt.barcode2.SimpleTree
import ca.odell.glazedlists.impl.adt.barcode2.SimpleTreeIterator
import java.util.*

/** An [EventList] that shows its source [EventList] in sorted order. */
@Suppress(
    "INAPPLICABLE_JVM_NAME",
    "REDUNDANT_PROJECTION",
    "UNCHECKED_CAST",
)
class SortedList<E>(
    source: EventList<E>,
    comparator: Comparator<in E>?,
) : TransformedList<E, E>(source) {
    private var unsorted: SimpleTree<Element<*>?>? = null
    private var sorted: SimpleTree<Element<*>?>? = null

    /** One of [STRICT_SORT_ORDER] or [AVOID_MOVING_ELEMENTS]. */
    var mode: Int = STRICT_SORT_ORDER
        set(value) {
            require(value == STRICT_SORT_ORDER || value == AVOID_MOVING_ELEMENTS) {
                "Mode must be either SortedList.STRICT_SORT_ORDER or SortedList.AVOID_MOVING_ELEMENTS"
            }
            if (value == field) return

            field = value
            if (field == STRICT_SORT_ORDER) {
                this.comparator = comparator
            }
        }

    /** The comparator in use, or `null` when this list follows source order. */
    var comparator: Comparator<in E>? = null
        set(value) {
            val treeComparator: Comparator<Any?> =
                if (value != null) ElementComparator(value) else ElementRawOrderComparator()

            if (unsorted == null) {
                unsorted = SimpleTree()
                val sourceList = source!!
                for (index in sourceList.indices) {
                    unsorted!!.add(index, null, 1)
                }
                val rebuiltSorted = rebuildSortedTree(value, treeComparator, null)
                field = value
                sorted = rebuiltSorted
                linkUnsortedNodes(rebuiltSorted)
                return
            }

            val sourceList = source!!
            val newIndexByUnsortedNode =
                if (sourceList.isEmpty()) null else IdentityHashMap<Element<Element<*>?>, Int>(sourceList.size)
            val rebuiltSorted = rebuildSortedTree(value, treeComparator, newIndexByUnsortedNode)

            val reorderMap = IntArray(sourceList.size)
            var oldSortedIndex = 0
            val previousIterator = SimpleTreeIterator(sorted!!)
            while (previousIterator.hasNext()) {
                previousIterator.next()
                val oldSortedNode = previousIterator.node()
                val unsortedNode = asNode(oldSortedNode.get()!!)
                val newSortedIndex = newIndexByUnsortedNode!!.getValue(unsortedNode)
                reorderMap[newSortedIndex] = oldSortedIndex++
            }

            field = value
            sorted = rebuiltSorted
            linkUnsortedNodes(rebuiltSorted)
            if (sourceList.isEmpty()) return

            updates.beginEvent()
            updates.reorder(reorderMap)
            updates.commitEvent()
        }

    private fun rebuildSortedTree(
        comparator: Comparator<in E>?,
        treeComparator: Comparator<Any?>,
        newIndexByUnsortedNode: IdentityHashMap<Element<Element<*>?>, Int>?,
    ): SimpleTree<Element<*>?> {
        val rebuiltSorted = SimpleTree<Element<*>?>(treeComparator)
        val unsortedTree = unsorted!!
        val unsortedIterator = SimpleTreeIterator(unsortedTree)

        if (comparator == null) {
            var sortedIndex = 0
            while (unsortedIterator.hasNext()) {
                unsortedIterator.next()
                addSortedNode(rebuiltSorted, sortedIndex++, unsortedIterator.node(), newIndexByUnsortedNode)
            }
            return rebuiltSorted
        }

        val sourceList = source!!
        val indexedNodes = ArrayList<IndexedNode<E>>(sourceList.size)
        var sourceIndex = 0
        while (unsortedIterator.hasNext()) {
            unsortedIterator.next()
            indexedNodes += IndexedNode(sourceIndex, sourceList[sourceIndex], unsortedIterator.node())
            sourceIndex++
        }
        indexedNodes.sortWith { alpha, beta ->
            val result = comparator.compare(alpha.value, beta.value)
            if (result != 0) result else alpha.sourceIndex.compareTo(beta.sourceIndex)
        }
        indexedNodes.forEachIndexed { sortedIndex, indexedNode ->
            addSortedNode(rebuiltSorted, sortedIndex, indexedNode.unsortedNode, newIndexByUnsortedNode)
        }
        return rebuiltSorted
    }

    private fun addSortedNode(
        rebuiltSorted: SimpleTree<Element<*>?>,
        sortedIndex: Int,
        unsortedNode: Element<Element<*>?>,
        newIndexByUnsortedNode: IdentityHashMap<Element<Element<*>?>, Int>?,
    ) {
        rebuiltSorted.add(sortedIndex, unsortedNode, 1)
        newIndexByUnsortedNode?.put(unsortedNode, sortedIndex)
    }

    private fun linkUnsortedNodes(sortedTree: SimpleTree<Element<*>?>) {
        val sortedIterator = SimpleTreeIterator(sortedTree)
        while (sortedIterator.hasNext()) {
            sortedIterator.next()
            asNode(sortedIterator.value()!!).set(sortedIterator.node())
        }
    }

    constructor(source: EventList<E>) : this(source, naturalOrderComparator())

    init {
        source.readWriteLock.readLock().lock()
        try {
            this.comparator = comparator
            source.addListEventListener(this)
        } finally {
            source.readWriteLock.readLock().unlock()
        }
    }

    override fun listChanged(listChanges: ListEvent<E>) {
        if (listChanges.isReordering) {
            handleReordering(listChanges.reorderMap)
            return
        }

        updates.beginEvent()

        val insertNodes = ArrayDeque<Element<Element<*>?>>()
        val updateNodes = ArrayList<Element<Element<*>?>>()
        val previousValues = ArrayList<E>()
        val unsortedTree = unsorted!!

        while (listChanges.next()) {
            val unsortedIndex = listChanges.index
            when (listChanges.type) {
                ListEvent.INSERT -> {
                    val unsortedNode = unsortedTree.add(unsortedIndex, null, 1)
                    insertNodes.addLast(unsortedNode)
                }

                ListEvent.UPDATE -> {
                    val unsortedNode = unsortedTree[unsortedIndex]
                    val sortedNode = asNode(unsortedNode.get()!!)
                    sortedNode.sorted = Element.PENDING
                    updateNodes.add(sortedNode)
                    previousValues.add(listChanges.oldValue)
                }

                ListEvent.DELETE -> {
                    val unsortedNode = unsortedTree[unsortedIndex]
                    val deleted = listChanges.oldValue
                    unsortedTree.remove(unsortedNode)
                    val deleteSortedIndex = deleteByUnsortedNode(unsortedNode)
                    updates.elementDeleted(deleteSortedIndex, deleted)
                }
            }
        }

        classifyUpdatedNodes(updateNodes)
        publishUpdates(updateNodes, previousValues)

        while (insertNodes.isNotEmpty()) {
            val insertedIndex = insertByUnsortedNode(insertNodes.removeFirst())
            updates.elementInserted(insertedIndex, ListEvent.unknownValue())
        }

        updates.commitEvent()
    }

    private fun handleReordering(sourceReorder: IntArray) {
        val sortedTree = sorted!!
        val unsortedTree = unsorted!!
        val previousIndexToSortedIndex = IntArray(sortedTree.size())
        var index = 0
        val sortedIterator = SimpleTreeIterator(sortedTree)
        while (sortedIterator.hasNext()) {
            sortedIterator.next()
            val unsortedNode = asNode(sortedIterator.value()!!)
            val unsortedIndex = unsortedTree.indexOfNode(unsortedNode, ALL_COLORS)
            previousIndexToSortedIndex[unsortedIndex] = index++
        }

        val newIndexToSortedIndex = IntArray(sortedTree.size())
        for (newIndex in previousIndexToSortedIndex.indices) {
            newIndexToSortedIndex[newIndex] = previousIndexToSortedIndex[sourceReorder[newIndex]]
        }

        val unsortedNodes = arrayOfNulls<Element<Element<*>?>>(unsortedTree.size())
        index = 0
        val unsortedIterator = SimpleTreeIterator(unsortedTree)
        while (unsortedIterator.hasNext()) {
            unsortedIterator.next()
            unsortedNodes[index++] = unsortedIterator.node()
        }
        unsortedNodes.sortWith(sortedTree.comparator)

        val reorderMap = IntArray(sortedTree.size())
        var indexChanged = false
        index = 0
        val reorderedIterator = SimpleTreeIterator(sortedTree)
        while (reorderedIterator.hasNext()) {
            reorderedIterator.next()
            val sortedNode = reorderedIterator.node()
            val unsortedNode = unsortedNodes[index]!!
            sortedNode.set(unsortedNode)
            unsortedNode.set(sortedNode)
            val unsortedIndex = unsortedTree.indexOfNode(unsortedNode, ALL_COLORS)
            reorderMap[index] = newIndexToSortedIndex[unsortedIndex]
            indexChanged = indexChanged || index != reorderMap[index]
            index++
        }

        if (indexChanged) {
            updates.beginEvent()
            updates.reorder(reorderMap)
            updates.commitEvent()
        }
    }

    private fun classifyUpdatedNodes(updateNodes: List<Element<Element<*>?>>) {
        val sortedTree = sorted!!
        val nodeComparator = sortedTree.comparator

        for (sortedNode in updateNodes) {
            if (sortedNode.sorted != Element.PENDING) continue

            var lowerBound: Element<Element<*>?>? = null
            var upperBound: Element<Element<*>?>? = null
            var firstUnsortedNode = sortedNode

            var leftNeighbour = sortedNode.previous()
            while (leftNeighbour != null) {
                if (leftNeighbour.sorted != Element.SORTED) {
                    firstUnsortedNode = leftNeighbour
                    leftNeighbour = leftNeighbour.previous()
                    continue
                }
                lowerBound = leftNeighbour
                break
            }

            var rightNeighbour = sortedNode.next()
            while (rightNeighbour != null) {
                if (rightNeighbour.sorted == Element.SORTED) {
                    upperBound = rightNeighbour
                    break
                }
                rightNeighbour = rightNeighbour.next()
            }

            var current: Element<Element<*>?>? = firstUnsortedNode
            while (current !== upperBound) {
                val currentNode = current!!
                if (upperBound != null && nodeComparator.compare(currentNode.get(), upperBound.get()) > 0) {
                    currentNode.sorted = Element.UNSORTED
                    current = currentNode.next()
                    continue
                }
                if (lowerBound != null && nodeComparator.compare(currentNode.get(), lowerBound.get()) < 0) {
                    currentNode.sorted = Element.UNSORTED
                    current = currentNode.next()
                    continue
                }
                currentNode.sorted = Element.SORTED
                lowerBound = currentNode
                current = currentNode.next()
            }
        }
    }

    private fun publishUpdates(
        updateNodes: List<Element<Element<*>?>>,
        previousValues: List<E>,
    ) {
        val sortedTree = sorted!!
        for (index in updateNodes.indices) {
            val previous = previousValues[index]
            val sortedNode = updateNodes[index]
            assert(sortedNode.sorted != Element.PENDING)
            val originalIndex = sortedTree.indexOfNode(sortedNode, ALL_COLORS)

            when {
                sortedNode.sorted == Element.SORTED ->
                    updates.elementUpdated(originalIndex, previous, ListEvent.unknownValue())

                mode == AVOID_MOVING_ELEMENTS ->
                    updates.elementUpdated(originalIndex, previous, ListEvent.unknownValue())

                else -> {
                    sortedTree.remove(sortedNode)
                    updates.elementDeleted(originalIndex, previous)
                    val insertedIndex = insertByUnsortedNode(asNode(sortedNode.get()!!))
                    updates.elementInserted(insertedIndex, ListEvent.unknownValue())
                }
            }
        }
    }

    private fun insertByUnsortedNode(unsortedNode: Element<Element<*>?>): Int {
        val sortedTree = sorted!!
        val sortedNode = sortedTree.addInSortedOrder(ALL_COLORS, unsortedNode, 1)
        unsortedNode.set(sortedNode)
        return sortedTree.indexOfNode(sortedNode, ALL_COLORS)
    }

    private fun deleteByUnsortedNode(unsortedNode: Element<Element<*>?>): Int {
        val sortedTree = sorted!!
        val sortedNode = asNode(unsortedNode.get()!!)
        val sortedIndex = sortedTree.indexOfNode(sortedNode, ALL_COLORS)
        sortedTree.remove(sortedIndex, 1)
        return sortedIndex
    }

    override fun getSourceIndex(mutationIndex: Int): Int {
        val sortedNode = sorted!![mutationIndex]
        val unsortedNode = asNode(sortedNode.get()!!)
        return unsorted!!.indexOfNode(unsortedNode, ALL_COLORS)
    }

    override fun isWritable(): Boolean = true

    override fun indexOf(element: E): Int {
        val currentComparator = comparator
        if (mode != STRICT_SORT_ORDER || currentComparator == null) return super.indexOf(element)

        var index = indexOfValue(element, first = true, simulated = false)
        if (index == -1) return -1
        while (index < size) {
            val objectAtIndex = get(index)
            if (currentComparator.compare(element, objectAtIndex) != 0) return -1
            if (element == objectAtIndex) return index
            index++
        }
        return -1
    }

    override fun lastIndexOf(element: E): Int {
        val currentComparator = comparator
        if (mode != STRICT_SORT_ORDER || currentComparator == null) return super.lastIndexOf(element)

        var index = indexOfValue(element, first = false, simulated = false)
        if (index == -1) return -1
        while (index >= 0) {
            val objectAtIndex = get(index)
            if (currentComparator.compare(element, objectAtIndex) != 0) return -1
            if (element == objectAtIndex) return index
            index--
        }
        return -1
    }

    fun sortIndex(element: Any?): Int {
        check(comparator != null) { "No Comparator exists to perform this operation" }
        return indexOfValue(element, first = true, simulated = true)
    }

    fun lastSortIndex(element: Any?): Int {
        check(comparator != null) { "No Comparator exists to perform this operation" }
        return indexOfValue(element, first = false, simulated = true)
    }

    override fun contains(element: E): Boolean = indexOf(element) != -1

    private fun indexOfValue(element: Any?, first: Boolean, simulated: Boolean): Int =
        (sorted!! as SimpleTree<Any?>).indexOfValue(element, first, simulated, ALL_COLORS)

    override fun iterator(): MutableIterator<E> = SortedListIterator()

    private fun compareElements(
        comparator: Comparator<in E>,
        alpha: Any?,
        beta: Any?,
    ): Int {
        var alphaObject = alpha
        var betaObject = beta
        var alphaIndex = -1
        var betaIndex = -1

        if (alpha is Element<*>) {
            alphaIndex = unsorted!!.indexOfNode(asNode(alpha), ALL_COLORS)
            alphaObject = source!![alphaIndex]
        }
        if (beta is Element<*>) {
            betaIndex = unsorted!!.indexOfNode(asNode(beta), ALL_COLORS)
            betaObject = source!![betaIndex]
        }

        val result = comparator.compare(alphaObject as E, betaObject as E)
        if (result != 0) return result
        if (alphaIndex != -1 && betaIndex != -1) return alphaIndex - betaIndex
        return 0
    }

    private fun compareRawOrder(alpha: Any?, beta: Any?): Int {
        val alphaIndex = unsorted!!.indexOfNode(asNode(alpha as Element<*>), ALL_COLORS)
        val betaIndex = unsorted!!.indexOfNode(asNode(beta as Element<*>), ALL_COLORS)
        return alphaIndex - betaIndex
    }

    private fun elementForIterator(treeIterator: SimpleTreeIterator<Element<*>?>): E {
        val unsortedNode = asNode(treeIterator.value()!!)
        return source!![unsorted!!.indexOfNode(unsortedNode, ALL_COLORS)]
    }

    private fun newTreeIterator(): SimpleTreeIterator<Element<*>?> = SimpleTreeIterator(sorted!!)

    private fun removeIteratorElement(indexToRemove: Int): SimpleTreeIterator<Element<*>?> {
        source!!.removeAt(getSourceIndex(indexToRemove))
        return SimpleTreeIterator(sorted!!, indexToRemove, ALL_COLORS)
    }

    private class IndexedNode<E>(
        val sourceIndex: Int,
        val value: E,
        val unsortedNode: Element<Element<*>?>,
    )

    private open inner class ElementComparator(
        private val comparator: Comparator<in E>,
    ) : Comparator<Any?> {
        override fun compare(alpha: Any?, beta: Any?): Int = compareElements(comparator, alpha, beta)
    }

    private open inner class ElementRawOrderComparator : Comparator<Any?> {
        override fun compare(alpha: Any?, beta: Any?): Int = compareRawOrder(alpha, beta)
    }

    private open inner class SortedListIterator : MutableIterator<E> {
        private var treeIterator = newTreeIterator()

        override fun hasNext(): Boolean = treeIterator.hasNext()

        override fun next(): E {
            treeIterator.next()
            return elementForIterator(treeIterator)
        }

        override fun remove() {
            val indexToRemove = treeIterator.index()
            treeIterator = removeIteratorElement(indexToRemove)
        }
    }

    companion object {
        private const val ALL_COLORS: Byte = 1

        /** Elements are always kept in comparator order. */
        const val STRICT_SORT_ORDER: Int = 0

        /** Updated elements remain at their current index. */
        const val AVOID_MOVING_ELEMENTS: Int = 1

        fun <E> create(source: EventList<E>): SortedList<E> where E : Comparable<in E> = SortedList(source)

        private fun <E> naturalOrderComparator(): Comparator<E> =
            GlazedLists.comparableComparator<Comparable<Any>>() as Comparator<E>

        private fun asNode(element: Element<*>): Element<Element<*>?> =
            element as Element<Element<*>?>
    }
}
