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
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.impl.adt.Barcode
import ca.odell.glazedlists.impl.adt.barcode2.Element
import ca.odell.glazedlists.impl.adt.barcode2.SimpleTree
import ca.odell.glazedlists.impl.adt.barcode2.SimpleTreeIterator

/**
 * A flattened list of the children extracted from each element in a parent [EventList].
 *
 * Event-list children must share this list's publisher and read/write lock. Indexed add is unsupported,
 * while indexed set and remove write through to the corresponding child list.
 */
@Suppress(
    "INAPPLICABLE_JVM_NAME",
    "REDUNDANT_MODALITY_MODIFIER",
    "ReplaceGetOrSet",
    "UNCHECKED_CAST",
)
open class CollectionList<S, E>(
    source: EventList<S>,
    private val model: Model<S, E>,
) : TransformedList<S, E>(source), ListEventListener<S> {
    private val emptyChildElement: ChildElement<E> = SimpleChildElement(emptyList(), null)
    private val barcode = Barcode()
    private val childElements = SimpleTree<ChildElement<E>>()

    init {
        for (parentIndex in 0 until source.size) {
            val children = model.getChildren(source[parentIndex])
            val node = childElements.add(parentIndex, emptyChildElement, 1)
            node.set(createChildElementForList(children, node))

            barcode.addBlack(barcode.size(), 1)
            if (children.isNotEmpty()) barcode.addWhite(barcode.size(), children.size)
        }

        source.addListEventListener(this)
    }

    /** This list cannot support indexed add, although set and remove are supported. */
    open override fun isWritable(): Boolean = false

    @get:JvmName("size")
    open override val size: Int
        get() = barcode.whiteSize()

    open override fun get(index: Int): E {
        val childElement = getChildElement(index)
        val childIndexInParent = barcode.getWhiteSequenceIndex(index)
        return childElement.get(childIndexInParent)
    }

    open override fun set(index: Int, element: E): E {
        val childElement = getChildElement(index)
        val childIndexInParent = barcode.getWhiteSequenceIndex(index)
        return childElement.set(childIndexInParent, element)
    }

    @JvmName("remove")
    open override fun removeAt(index: Int): E {
        val childElement = getChildElement(index)
        val childIndexInParent = barcode.getWhiteSequenceIndex(index)
        return childElement.remove(childIndexInParent)
    }

    /** Returns the first flattened child index for [parentIndex], or `-1` when the parent has no children. */
    open fun childStartingIndex(parentIndex: Int): Int {
        if (parentIndex < 0) throw IndexOutOfBoundsException("Invalid index: $parentIndex")
        if (parentIndex >= source!!.size) throw IndexOutOfBoundsException("Invalid index: $parentIndex")

        val parentFullIndex = barcode.getIndex(parentIndex, Barcode.BLACK)
        val childFullIndex = parentFullIndex + 1
        if (childFullIndex >= barcode.size()) return -1
        if (barcode.get(childFullIndex) !== Barcode.WHITE) return -1

        val childIndex = childFullIndex - (parentIndex + 1)
        assert(barcode.getWhiteIndex(childFullIndex) == childIndex)
        return childIndex
    }

    /** Returns the last flattened child index for [parentIndex], or `-1` when the parent has no children. */
    open fun childEndingIndex(parentIndex: Int): Int {
        if (parentIndex < 0) throw IndexOutOfBoundsException("Invalid index: $parentIndex")
        if (parentIndex >= source!!.size) throw IndexOutOfBoundsException("Invalid index: $parentIndex")

        val nextParentFullIndex =
            if (parentIndex == barcode.blackSize() - 1) {
                barcode.size()
            } else {
                barcode.getIndex(parentIndex + 1, Barcode.BLACK)
            }
        val lastWhiteBeforeNextParent = nextParentFullIndex - 1
        if (barcode.get(lastWhiteBeforeNextParent) === Barcode.BLACK) return -1

        val childIndex = lastWhiteBeforeNextParent - (parentIndex + 1)
        assert(barcode.getWhiteIndex(lastWhiteBeforeNextParent) == childIndex)
        return childIndex
    }

    open override fun listChanged(listChanges: ListEvent<S>) {
        updates.beginEvent()
        while (listChanges.next()) {
            val index = listChanges.index
            when (listChanges.type) {
                ListEvent.INSERT -> handleInsert(index)
                ListEvent.DELETE -> handleDelete(index)
                ListEvent.UPDATE -> {
                    handleDelete(index)
                    handleInsert(index)
                }
            }
        }
        updates.commitEvent()
    }

    open override fun dispose() {
        super.dispose()

        val treeIterator = SimpleTreeIterator(childElements)
        while (treeIterator.hasNext()) {
            treeIterator.next()
            treeIterator.value().dispose()
        }
    }

    private fun handleInsert(parentIndex: Int) {
        val absoluteIndex = getAbsoluteIndex(parentIndex)
        val parent = source!![parentIndex]
        val children = model.getChildren(parent)

        val node = childElements.add(parentIndex, emptyChildElement, 1)
        node.set(createChildElementForList(children, node))

        barcode.addBlack(absoluteIndex, 1)
        if (children.isNotEmpty()) barcode.addWhite(absoluteIndex + 1, children.size)

        val childIndex = absoluteIndex - parentIndex
        for (element in children) {
            updates.elementInserted(childIndex, element)
        }
    }

    private fun handleDelete(sourceIndex: Int) {
        val parentIndex = getAbsoluteIndex(sourceIndex)
        val nextParentIndex = getAbsoluteIndex(sourceIndex + 1)
        val childCount = nextParentIndex - parentIndex - 1

        if (childCount > 0) {
            val firstDeletedChildIndex = parentIndex - sourceIndex
            val firstNotDeletedChildIndex = firstDeletedChildIndex + childCount
            for (childIndex in firstDeletedChildIndex until firstNotDeletedChildIndex) {
                updates.elementDeleted(firstDeletedChildIndex, get(childIndex))
            }
        }

        val removedChildElement = childElements[sourceIndex]
        childElements.remove(removedChildElement)
        removedChildElement.get().dispose()
        barcode.remove(parentIndex, 1 + childCount)
    }

    private fun getChildElement(childIndex: Int): ChildElement<E> {
        if (childIndex < 0) throw IndexOutOfBoundsException("Invalid index: $childIndex")
        if (childIndex >= size) throw IndexOutOfBoundsException("Index: $childIndex, Size: $size")

        val parentIndex = barcode.getBlackBeforeWhite(childIndex)
        return childElements[parentIndex].get()
    }

    private fun createChildElementForList(
        children: List<E>,
        node: Element<ChildElement<E>>,
    ): ChildElement<E> =
        if (children is EventList<*>) {
            EventChildElement(children as EventList<E>, node)
        } else {
            SimpleChildElement(children, node)
        }

    private fun getAbsoluteIndex(parentIndex: Int): Int {
        if (parentIndex < barcode.blackSize()) return barcode.getIndex(parentIndex, Barcode.BLACK)
        if (parentIndex == barcode.blackSize()) return barcode.size()
        throw IndexOutOfBoundsException()
    }

    @JvmSynthetic
    internal fun simpleChildRemoved(
        node: Element<*>,
        index: Int,
        removed: E,
    ) {
        val parentIndex = childElements.indexOfNode(node as Element<ChildElement<E>>, 0)
        val absoluteIndex = getAbsoluteIndex(parentIndex)
        val firstChildIndex = absoluteIndex + 1
        barcode.remove(firstChildIndex + index, 1)

        val childOffset = absoluteIndex - parentIndex
        updates.beginEvent()
        updates.elementDeleted(index + childOffset, removed)
        updates.commitEvent()
    }

    @JvmSynthetic
    internal fun simpleChildUpdated(
        node: Element<*>,
        index: Int,
        replaced: E,
    ) {
        val parentIndex = childElements.indexOfNode(node as Element<ChildElement<E>>, 0)
        val absoluteIndex = getAbsoluteIndex(parentIndex)
        val childOffset = absoluteIndex - parentIndex
        updates.beginEvent()
        updates.elementUpdated(index + childOffset, replaced, ListEvent.unknownValue())
        updates.commitEvent()
    }

    @JvmSynthetic
    internal fun eventChildChanged(
        node: Element<*>,
        children: EventList<E>,
        listChanges: ListEvent<E>,
    ) {
        val parentIndex = childElements.indexOfNode(node as Element<ChildElement<E>>, 1)
        val absoluteIndex = getAbsoluteIndex(parentIndex)
        val nextNodeIndex = getAbsoluteIndex(parentIndex + 1)

        val firstChildIndex = absoluteIndex + 1
        val previousChildrenCount = nextNodeIndex - firstChildIndex
        if (previousChildrenCount > 0) barcode.remove(firstChildIndex, previousChildrenCount)
        if (children.isNotEmpty()) barcode.addWhite(firstChildIndex, children.size)

        val childOffset = absoluteIndex - parentIndex
        updates.beginEvent()
        while (listChanges.next()) {
            val overallIndex = listChanges.index + childOffset
            when (listChanges.type) {
                ListEvent.INSERT -> updates.elementInserted(overallIndex, listChanges.newValue)
                ListEvent.UPDATE -> updates.elementUpdated(overallIndex, listChanges.oldValue, listChanges.newValue)
                ListEvent.DELETE -> updates.elementDeleted(overallIndex, listChanges.oldValue)
            }
        }
        updates.commitEvent()
    }

    @JvmSynthetic
    internal fun childNodeIndex(node: Element<*>): Int =
        childElements.indexOfNode(node as Element<ChildElement<E>>, 0)

    private interface ChildElement<E> {
        fun get(index: Int): E

        fun remove(index: Int): E

        fun set(index: Int, element: E): E

        fun dispose()
    }

    /** Maps a parent value to its child list. */
    @FunctionalInterface
    fun interface Model<P, C> {
        fun getChildren(parent: P): List<C>
    }

    private inner class SimpleChildElement(
        private val children: List<E>,
        private val node: Element<ChildElement<E>>?,
    ) : ChildElement<E> {
        override fun get(index: Int): E = children[index]

        override fun remove(index: Int): E {
            val removed = (children as MutableList<E>).removeAt(index)
            simpleChildRemoved(node!!, index, removed)
            return removed
        }

        override fun set(index: Int, element: E): E {
            val replaced = (children as MutableList<E>).set(index, element)
            simpleChildUpdated(node!!, index, replaced)
            return replaced
        }

        override fun dispose() = Unit
    }

    private inner class EventChildElement(
        private val children: EventList<E>,
        private val node: Element<ChildElement<E>>,
    ) : ChildElement<E>, ListEventListener<E> {
        init {
            require(publisher == children.publisher) {
                "If a CollectionList.Model returns EventLists, those EventLists must use the same ListEventPublisher as the CollectionList"
            }
            require(readWriteLock == children.readWriteLock) {
                "If a CollectionList.Model returns EventLists, those EventLists must use the same ReadWriteLock as the CollectionList"
            }

            children.publisher.setRelatedSubject(this, this@CollectionList)
            children.addListEventListener(this)
        }

        override fun get(index: Int): E = children[index]

        override fun remove(index: Int): E = children.removeAt(index)

        override fun set(index: Int, element: E): E = children.set(index, element)

        override fun listChanged(listChanges: ListEvent<E>) = eventChildChanged(node, children, listChanges)

        override fun dispose() {
            children.removeListEventListener(this)
            children.publisher.clearRelatedSubject(this)
        }

        override fun toString(): String = "[${childNodeIndex(node)}:$children]"
    }
}
