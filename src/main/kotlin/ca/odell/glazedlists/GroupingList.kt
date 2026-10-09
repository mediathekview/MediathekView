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
import ca.odell.glazedlists.impl.Grouper
import ca.odell.glazedlists.impl.adt.barcode2.Element
import ca.odell.glazedlists.impl.adt.barcode2.SimpleTree
import java.util.*

@Suppress("INAPPLICABLE_JVM_NAME", "REDUNDANT_PROJECTION")
class GroupingList<E> private constructor(
    source: SortedList<E>,
) : TransformedList<E, List<E>>(source) {
    private val groupLists = SimpleTree<GroupList>()
    private val grouper = Grouper(source, GrouperClient())

    constructor(source: EventList<E>) : this(source, comparableComparator())

    constructor(source: EventList<E>, comparator: Comparator<in E>?) : this(
        SortedList(source, comparator),
    )

    init {
        rebuildGroupListTreeFromBarcode()
        source.addListEventListener(this)
    }

    private fun rebuildGroupListTreeFromBarcode() {
        groupLists.clear()
        repeat(grouper.barcode.colourSize(Grouper.UNIQUE)) { index ->
            attachGroupList(index)
        }
    }

    private fun attachGroupList(index: Int) {
        val groupList = GroupList()
        groupList.setTreeNode(groupLists.add(index, groupList, 1))
    }

    private fun detachGroupList(index: Int) {
        val indexedTreeNode = groupLists[index]
        groupLists.remove(indexedTreeNode)
        indexedTreeNode.get().setTreeNode(null)
    }

    private fun groupIndexOf(treeNode: Element<GroupList>): Int = groupLists.indexOfNode(treeNode, 1)

    private fun groupCount(): Int = grouper.barcode.blackSize()

    private fun groupStartIndex(groupIndex: Int): Int = getSourceIndex(groupIndex)

    private fun groupEndIndex(groupIndex: Int): Int = grouper.barcode.getIndex(groupIndex + 1, Grouper.UNIQUE)

    private fun groupedSourceSize(): Int = grouper.barcode.size()

    @Suppress("UNCHECKED_CAST")
    fun indexOfGroup(groupElement: E): Int {
        val sortedSource = source as SortedList<E>
        val sourceIndex = sortedSource.sortIndex(groupElement)
        if (sourceIndex == sortedSource.size || grouper.comparator!!.compare(
                sortedSource[sourceIndex],
                groupElement
            ) != 0
        ) {
            return -1
        }
        return grouper.barcode.getColourIndex(sourceIndex, Grouper.UNIQUE)
    }

    private open inner class GrouperClient : Grouper.Client<E> {
        override fun groupChanged(
            index: Int,
            groupIndex: Int,
            groupChangeType: Int,
            primary: Boolean,
            elementChangeType: Int,
            oldValue: E,
            newValue: E,
            updateNextSeparator: Boolean,
            joinRight: Boolean,
        ) {
            when (groupChangeType) {
                ListEvent.INSERT -> {
                    insertGroupList(groupIndex)
                    updates.elementsInserted(groupIndex, groupIndex)
                }

                ListEvent.DELETE -> {
                    removeGroupList(groupIndex)
                    updates.elementsDeleted(groupIndex, groupIndex)
                }

                ListEvent.UPDATE -> updates.elementsUpdated(groupIndex, groupIndex)
                else -> throw IllegalStateException()
            }
        }

        private fun insertGroupList(index: Int) {
            attachGroupList(index)
        }

        private fun removeGroupList(index: Int) {
            detachGroupList(index)
        }
    }

    @Suppress("UNCHECKED_CAST")
    fun setComparator(comparator: Comparator<in E>?) {
        val effectiveComparator: Comparator<in E> = comparator ?: comparableComparator()
        (source as SortedList<E>).comparator = effectiveComparator
    }

    override fun getSourceIndex(mutationIndex: Int): Int = grouper.barcode.getIndex(mutationIndex, Grouper.UNIQUE)

    override fun isWritable(): Boolean = true

    override fun listChanged(listChanges: ListEvent<E>) {
        updates.beginEvent(true)
        val sortedSource = source as SortedList<E>
        val sourceComparator = sortedSource.comparator
        if (sourceComparator !== grouper.comparator) {
            for (group in this) {
                updates.elementDeleted(0, group)
            }
            grouper.comparator = sourceComparator
            rebuildGroupListTreeFromBarcode()
            updates.elementsInserted(0, size - 1)
        } else {
            grouper.listChanged(listChanges)
        }
        updates.commitEvent()
    }

    override fun get(index: Int): MutableList<E> = groupLists[index].get()

    @JvmName("remove")
    override fun removeAt(index: Int): MutableList<E> {
        if (index !in indices) {
            throw IndexOutOfBoundsException("Cannot remove at $index on list of size $size")
        }
        val removed = get(index)
        val result = ArrayList(removed)
        removed.clear()
        return result
    }

    override fun set(index: Int, element: List<E>): MutableList<E> {
        if (index !in indices) {
            throw IndexOutOfBoundsException("Cannot set at $index on list of size $size")
        }
        updates.beginEvent(true)
        val result = removeAt(index)
        add(index, element)
        updates.commitEvent()
        return result
    }

    override fun add(index: Int, element: List<E>) {
        source!!.addAll(element)
    }

    @get:JvmName("size")
    override val size: Int
        get() = grouper.barcode.colourSize(Grouper.UNIQUE)

    override fun dispose() {
        source!!.dispose()
        super.dispose()
    }

    private open inner class GroupList : AbstractList<E>() {
        private var treeNode: Element<GroupList>? = null

        fun setTreeNode(treeNode: Element<GroupList>?) {
            this.treeNode = treeNode
        }

        private fun getStartIndex(): Int {
            val currentTreeNode = treeNode ?: return -1
            val groupIndex = groupIndexOf(currentTreeNode)
            return groupStartIndex(groupIndex)
        }

        private fun getEndIndex(): Int {
            val currentTreeNode = treeNode ?: return -1
            val groupIndex = groupIndexOf(currentTreeNode)
            return if (groupIndex < groupCount() - 1) {
                groupEndIndex(groupIndex)
            } else {
                groupedSourceSize()
            }
        }

        private fun getSourceIndex(index: Int): Int = getStartIndex() + index

        override fun set(index: Int, element: E): E = source!!.set(getSourceIndex(index), element)

        override fun get(index: Int): E = source!![getSourceIndex(index)]

        @get:JvmName("size")
        override val size: Int
            get() = getEndIndex() - getStartIndex()

        override fun clear() {
            source!!.subList(getStartIndex(), getEndIndex()).clear()
        }

        @JvmName("remove")
        override fun removeAt(index: Int): E = source!!.removeAt(getSourceIndex(index))

        override fun add(index: Int, element: E) {
            source!!.add(getSourceIndex(index), element)
        }
    }

    companion object {
        fun <E> create(source: EventList<E>): GroupingList<E> where E : Comparable<in E> = GroupingList(source)

        @Suppress("UNCHECKED_CAST")
        private fun <E> comparableComparator(): Comparator<in E> =
            GlazedLists.comparableComparator<Comparable<Any?>>() as Comparator<in E>
    }
}
