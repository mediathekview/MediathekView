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
import ca.odell.glazedlists.impl.adt.Barcode
import ca.odell.glazedlists.impl.adt.barcode2.Element
import ca.odell.glazedlists.impl.adt.barcode2.SimpleTree
import ca.odell.glazedlists.impl.adt.barcode2.SimpleTreeIterator
import java.util.*

/** A list that adds separator objects before each group of elements. */
@Suppress("INAPPLICABLE_JVM_NAME", "UNCHECKED_CAST")
open class SeparatorList<E>(
    source: EventList<E>,
    comparator: Comparator<in E>,
    private val minimumSizeForSeparator: Int,
    defaultLimit: Int,
) : TransformedList<E, E>(SeparatorInjectorList(SortedList(source, comparator), defaultLimit)) {
    private val separatorSource = this.source as SeparatorInjectorList<E>
    private var collapsedElements = Barcode()

    init {
        rebuildCollapsedElements()
        separatorSource.addListEventListener(this)
    }

    private fun rebuildCollapsedElements() {
        collapsedElements = Barcode()
        collapsedElements.addBlack(0, separatorSource.size)
        val groupCount = separatorSource.insertedSeparators.colourSize(Barcode.BLACK)
        for (group in 0 until groupCount) {
            updateGroup(group, groupCount, false)
        }
    }

    @get:JvmName("size")
    override val size: Int
        get() = collapsedElements.colourSize(Barcode.BLACK)

    override fun getSourceIndex(mutationIndex: Int): Int =
        collapsedElements.getIndex(mutationIndex, Barcode.BLACK)

    override fun isWritable(): Boolean = true

    open fun setComparator(comparator: Comparator<E>) {
        val isEmpty = isEmpty()
        val previousCollapsedElements = collapsedElements
        var outerEventOpened = false

        try {
            if (!isEmpty) {
                updates.beginEvent()
                outerEventOpened = true
                updates.elementsDeleted(0, size - 1)
            }

            separatorSource.replaceComparator(comparator) {
                if (!isEmpty) {
                    rebuildCollapsedElements()
                    updates.elementsInserted(0, size - 1)
                }
            }
        } catch (failure: Throwable) {
            collapsedElements = previousCollapsedElements
            if (outerEventOpened) {
                try {
                    updates.discardEvent()
                } catch (rollbackFailure: Throwable) {
                    if (rollbackFailure !== failure) failure.addSuppressed(rollbackFailure)
                }
            }
            throw failure
        }

        if (!isEmpty) {
            updates.commitEvent()
        }
    }

    override fun listChanged(listChanges: ListEvent<E>) {
        updates.beginEvent(true)

        if (listChanges.isReordering) {
            var canReorder = true
            val separatorIterator = SimpleTreeIterator(separatorSource.separators)
            while (separatorIterator.hasNext()) {
                separatorIterator.next()
                val separator = separatorIterator.node().get()
                val limit = separator.limit
                if (limit == 0) continue
                if (limit >= separatorSource.size) continue
                if (limit >= separator.size()) continue
                canReorder = false
                break
            }

            if (canReorder) {
                val previousIndices = listChanges.reorderMap
                val reorderMap = IntArray(collapsedElements.colourSize(Barcode.BLACK))
                val iterator = collapsedElements.iterator()
                var groupStartSourceIndex = 0
                while (true) {
                    val newGroupFound: Boolean
                    val groupEndSourceIndex: Int
                    val leadingCollapsedElements: Int
                    if (iterator.hasNextWhite()) {
                        iterator.nextWhite()
                        groupEndSourceIndex = iterator.index
                        newGroupFound = true
                        leadingCollapsedElements = iterator.getWhiteIndex()
                    } else {
                        newGroupFound = false
                        groupEndSourceIndex = collapsedElements.size()
                        leadingCollapsedElements = collapsedElements.whiteSize()
                    }

                    for (index in groupStartSourceIndex until groupEndSourceIndex) {
                        reorderMap[index - leadingCollapsedElements] =
                            previousIndices[index] - leadingCollapsedElements
                    }

                    if (newGroupFound && iterator.hasNextBlack()) {
                        iterator.nextBlack()
                        groupStartSourceIndex = iterator.index
                    } else {
                        break
                    }
                }
                updates.reorder(reorderMap)
            } else {
                val visibleSize = collapsedElements.colourSize(Barcode.BLACK)
                if (visibleSize > 0) {
                    updates.elementsDeleted(0, visibleSize - 1)
                    updates.elementsInserted(0, visibleSize - 1)
                }
            }
        } else {
            val groupCount = separatorSource.insertedSeparators.colourSize(Barcode.BLACK)

            while (listChanges.next()) {
                val changeIndex = listChanges.index
                when (listChanges.type) {
                    ListEvent.INSERT -> {
                        collapsedElements.add(changeIndex, Barcode.BLACK, 1)
                        val viewIndex = collapsedElements.getColourIndex(changeIndex, Barcode.BLACK)
                        updates.elementInserted(viewIndex, ListEvent.unknownValue())
                    }

                    ListEvent.UPDATE -> {
                        if (collapsedElements[changeIndex] === Barcode.BLACK) {
                            val viewIndex = collapsedElements.getColourIndex(changeIndex, Barcode.BLACK)
                            updates.elementUpdated(viewIndex, ListEvent.unknownValue(), ListEvent.unknownValue())
                        }
                    }

                    ListEvent.DELETE -> {
                        val oldColor = collapsedElements[changeIndex]
                        if (oldColor === Barcode.BLACK) {
                            val viewIndex = collapsedElements.getColourIndex(changeIndex, Barcode.BLACK)
                            updates.elementDeleted(viewIndex, ListEvent.unknownValue())
                        }
                        collapsedElements.remove(changeIndex, 1)
                    }
                }
            }

            listChanges.reset()
            val groups = BitSet(groupCount)
            while (listChanges.nextBlock()) {
                val endIndex = listChanges.blockEndIndex
                val changeType = listChanges.type
                var changeIndex = listChanges.blockStartIndex
                var changeIndexGroup = -1
                var group = -1

                while (changeIndex <= endIndex) {
                    when (changeType) {
                        ListEvent.INSERT, ListEvent.UPDATE ->
                            changeIndexGroup = separatorSource.insertedSeparators.getColourIndex(
                                changeIndex,
                                true,
                                Barcode.BLACK,
                            )

                        ListEvent.DELETE -> {
                            if (changeIndex < separatorSource.insertedSeparators.size()) {
                                changeIndexGroup = separatorSource.insertedSeparators.getColourIndex(
                                    changeIndex,
                                    true,
                                    Barcode.BLACK,
                                )
                            }
                        }

                        else -> throw IllegalArgumentException("ListEvent Type: $changeType not supported")
                    }
                    changeIndex++

                    if (group != changeIndexGroup) {
                        group = changeIndexGroup
                        groups.set(group)
                        if (changeType == ListEvent.UPDATE) {
                            if (group > 0) groups.set(group - 1)
                            if (group < groupCount - 1) groups.set(group + 1)
                        }
                    }
                }
            }

            var group = groups.nextSetBit(0)
            while (group >= 0) {
                updateGroup(group, groupCount, true)
                if (group == Int.MAX_VALUE) break
                group = groups.nextSetBit(group + 1)
            }
        }

        updates.commitEvent()
    }

    private fun updateGroup(group: Int, groupCount: Int, fireEvents: Boolean) {
        val separator = separatorSource.separators[group].get()
        val separatorStart = separatorSource.insertedSeparators.getIndex(group, Barcode.BLACK)
        val nextGroup = group + 1
        val separatorEnd =
            if (nextGroup == groupCount) {
                separatorSource.insertedSeparators.size()
            } else {
                separatorSource.insertedSeparators.getIndex(nextGroup, Barcode.BLACK)
            }
        val groupSize = separatorEnd - separatorStart - 1

        if (groupSize < minimumSizeForSeparator) {
            setVisible(separatorStart, Barcode.WHITE, fireEvents)
            for (index in separatorStart + 1 until separatorEnd) {
                setVisible(index, Barcode.BLACK, fireEvents)
            }
        } else {
            setVisible(separatorStart, Barcode.BLACK, fireEvents)
            for (index in separatorStart + 1 until separatorEnd) {
                val withinLimit = index - separatorStart <= separator.limit
                setVisible(index, if (withinLimit) Barcode.BLACK else Barcode.WHITE, fireEvents)
            }
        }
    }

    private fun setVisible(index: Int, colour: Any, fireEvents: Boolean) {
        val previousColour = collapsedElements[index]
        if (colour === previousColour) return

        if (colour === Barcode.WHITE) {
            val viewIndex = collapsedElements.getColourIndex(index, Barcode.BLACK)
            if (fireEvents) updates.elementDeleted(viewIndex, ListEvent.unknownValue())
            collapsedElements.set(index, Barcode.WHITE, 1)
        } else if (colour === Barcode.BLACK) {
            collapsedElements.set(index, Barcode.BLACK, 1)
            val viewIndex = collapsedElements.getColourIndex(index, Barcode.BLACK)
            if (fireEvents) updates.elementInserted(viewIndex, ListEvent.unknownValue())
        } else {
            throw IllegalArgumentException()
        }
    }

    override fun dispose() {
        separatorSource.dispose()
        separatorSource.sortedSource.dispose()
        super.dispose()
    }

    interface Separator<E> {
        var limit: Int
        val group: List<E>
        fun first(): E
        fun size(): Int
    }

    /**
     * Kotlin has no package-private source visibility. `internal` keeps this implementation Kotlin-internal,
     * but it is public in JVM bytecode so same-module Java subclasses retain their construction path.
     */
    internal open class SeparatorInjectorList<E>(
        @JvmField @JvmSynthetic internal val sortedSource: SortedList<E>,
        @JvmField @JvmSynthetic internal val defaultLimit: Int,
    ) : TransformedList<E, E>(sortedSource) {
        @JvmField
        @JvmSynthetic
        internal val grouper: Grouper<E> = Grouper(sortedSource, GrouperClient())

        @JvmField
        @JvmSynthetic
        internal var insertedSeparators = Barcode()

        @JvmField
        @JvmSynthetic
        internal var separators = SimpleTree<GroupSeparator>()

        private var comparatorReplacementInProgress = false

        init {
            rebuildSeparators()
            sortedSource.addListEventListener(this)
        }

        private fun rebuildSeparators() {
            insertedSeparators = Barcode()
            separators = SimpleTree()
            insertedSeparators.add(0, Barcode.WHITE, source!!.size)

            val iterator = grouper.barcode.iterator()
            while (iterator.hasNextColour(Grouper.UNIQUE)) {
                iterator.nextColour(Grouper.UNIQUE)
                val groupIndex = iterator.getColourIndex(Grouper.UNIQUE)
                val sourceIndex = iterator.index
                insertedSeparators.add(groupIndex + sourceIndex, Barcode.BLACK, 1)
                val node = separators.add(groupIndex, GroupSeparator(), 1)
                node.get().setNode(node)
                node.get().limitState = defaultLimit
            }

            for (index in 0 until separators.size()) {
                separators[index].get().updateCachedValues()
            }
        }

        @JvmSynthetic
        internal fun replaceComparator(comparator: Comparator<in E>, afterRebuild: () -> Unit) {
            val previousComparator = sortedSource.comparator
            val previousInsertedSeparators = insertedSeparators
            val previousSeparators = separators
            comparatorReplacementInProgress = true
            try {
                sortedSource.comparator = comparator
                grouper.comparator = comparator
                rebuildSeparators()
                afterRebuild()
            } catch (failure: Throwable) {
                var sortedRestored = false
                var grouperRestored = false
                try {
                    sortedSource.comparator = previousComparator
                    sortedRestored = true
                } catch (rollbackFailure: Throwable) {
                    if (rollbackFailure !== failure) failure.addSuppressed(rollbackFailure)
                }
                try {
                    grouper.comparator = previousComparator
                    grouperRestored = true
                } catch (rollbackFailure: Throwable) {
                    if (rollbackFailure !== failure) failure.addSuppressed(rollbackFailure)
                }
                if (sortedRestored && grouperRestored) {
                    insertedSeparators = previousInsertedSeparators
                    separators = previousSeparators
                }
                throw failure
            } finally {
                comparatorReplacementInProgress = false
            }
        }

        override fun get(index: Int): E =
            when (insertedSeparators[index]) {
                Barcode.BLACK -> separators[getSeparatorIndex(index)].get() as E
                Barcode.WHITE -> source!![getSourceIndex(index)]
                else -> throw IllegalStateException()
            }

        override fun getSourceIndex(mutationIndex: Int): Int =
            when (insertedSeparators[mutationIndex]) {
                Barcode.BLACK -> throw IllegalArgumentException(
                    "No source index exists for the separator located at index $mutationIndex",
                )

                Barcode.WHITE -> insertedSeparators.getColourIndex(mutationIndex, Barcode.WHITE)
                else -> throw IllegalStateException()
            }

        protected open fun getSeparatorIndex(mutationIndex: Int): Int =
            when (insertedSeparators[mutationIndex]) {
                Barcode.BLACK -> insertedSeparators.getColourIndex(mutationIndex, Barcode.BLACK)
                Barcode.WHITE -> -1
                else -> throw IllegalStateException()
            }

        override fun isWritable(): Boolean = true

        @get:JvmName("size")
        override val size: Int
            get() = insertedSeparators.size()

        override fun listChanged(listChanges: ListEvent<E>) {
            if (comparatorReplacementInProgress) return

            val sourceComparator = sortedSource.comparator
            if (sourceComparator !== grouper.comparator) {
                grouper.comparator = sourceComparator
                rebuildSeparators()
                return
            }

            updates.beginEvent(true)
            if (listChanges.isReordering) {
                val previousIndices = listChanges.reorderMap
                val reorderMap = IntArray(insertedSeparators.size())
                var groupStartIndex = -1
                var groupEndIndex = 0
                var group = -1
                for (index in previousIndices.indices) {
                    if (index == groupEndIndex) {
                        group++
                        reorderMap[index + group] = index + group
                        groupStartIndex = groupEndIndex
                        val nextGroup = group + 1
                        groupEndIndex =
                            if (nextGroup < separators.size()) {
                                insertedSeparators.getIndex(nextGroup, Barcode.BLACK) - nextGroup
                            } else {
                                insertedSeparators.size()
                            }
                    }

                    val previousIndex = previousIndices[index]
                    if (previousIndex !in groupStartIndex until groupEndIndex) {
                        throw IllegalStateException()
                    }
                    reorderMap[index + group + 1] = previousIndex + group + 1
                }
                updates.reorder(reorderMap)
            } else {
                grouper.listChanged(listChanges)
            }

            for (index in 0 until separators.size()) {
                separators[index].get().updateCachedValues()
            }
            updates.commitEvent()
        }

        private open inner class GrouperClient : Grouper.Client<E> {
            @Suppress("PARAMETER_NAME_CHANGED_ON_OVERRIDE")
            override fun groupChanged(
                index: Int,
                initialGroupIndex: Int,
                groupChangeType: Int,
                @Suppress("UNUSED_PARAMETER") primary: Boolean,
                elementChangeType: Int,
                @Suppress("UNUSED_PARAMETER") oldValue: E,
                @Suppress("UNUSED_PARAMETER") newValue: E,
                updateNextSeparator: Boolean,
                joinRight: Boolean,
            ) {
                var groupIndex = initialGroupIndex
                var fixSeparatorForInsertGroupUpdateElement = false

                when (groupChangeType) {
                    ListEvent.INSERT -> {
                        val expandedIndex = index + groupIndex
                        insertedSeparators.add(expandedIndex, Barcode.BLACK, 1)
                        updates.elementInserted(expandedIndex, ListEvent.unknownValue())
                        val node = separators.add(groupIndex, GroupSeparator(), 1)
                        node.get().setNode(node)
                        node.get().limit = defaultLimit
                    }

                    ListEvent.UPDATE -> {
                        groupIndex = minOf(groupIndex, insertedSeparators.blackSize() - 1)
                        val expandedIndex = insertedSeparators.getIndex(groupIndex, Barcode.BLACK)
                        updates.elementUpdated(expandedIndex, ListEvent.unknownValue(), ListEvent.unknownValue())
                    }

                    ListEvent.DELETE -> {
                        val expandedIndex = insertedSeparators.getIndex(groupIndex, Barcode.BLACK)
                        insertedSeparators.remove(expandedIndex, 1)
                        updates.elementDeleted(expandedIndex, ListEvent.unknownValue())
                        val node = separators[groupIndex]
                        separators.remove(node)
                        node.get().setNode(null)
                        node.get().updateCachedValues()
                        groupIndex--
                    }
                }

                when (elementChangeType) {
                    ListEvent.INSERT -> {
                        val expandedIndex = index + groupIndex + 1
                        insertedSeparators.add(expandedIndex, Barcode.WHITE, 1)
                        updates.elementInserted(expandedIndex, ListEvent.unknownValue())
                    }

                    ListEvent.UPDATE -> {
                        var expandedIndex = index + groupIndex + 1
                        if (groupChangeType == ListEvent.INSERT) {
                            val separatorCount = insertedSeparators.colourSize(Barcode.BLACK)
                            if (groupIndex + 1 < separatorCount) {
                                val nextSeparatorIndex = insertedSeparators.getIndex(groupIndex + 1, Barcode.BLACK)
                                if (nextSeparatorIndex == expandedIndex) {
                                    expandedIndex++
                                    fixSeparatorForInsertGroupUpdateElement = true
                                }
                            }
                        }
                        updates.elementUpdated(expandedIndex, ListEvent.unknownValue(), ListEvent.unknownValue())
                    }

                    ListEvent.DELETE -> {
                        val expandedIndex = index + groupIndex + 1
                        var groupToShift = groupIndex
                        while (insertedSeparators[expandedIndex] === Barcode.BLACK) {
                            shiftSeparator(groupToShift++)
                        }
                        insertedSeparators.remove(expandedIndex, 1)
                        updates.elementDeleted(expandedIndex, ListEvent.unknownValue())
                    }
                }

                if (fixSeparatorForInsertGroupUpdateElement) {
                    val wrongSeparatorIndex = index + groupIndex + 1
                    assert(wrongSeparatorIndex == insertedSeparators.getIndex(groupIndex + 1, Barcode.BLACK))
                    insertedSeparators.remove(wrongSeparatorIndex, 1)
                    updates.elementDeleted(wrongSeparatorIndex, ListEvent.unknownValue())
                    insertedSeparators.add(wrongSeparatorIndex + 1, Barcode.BLACK, 1)
                    updates.elementInserted(wrongSeparatorIndex + 1, ListEvent.unknownValue())
                }

                val shiftGroupIndex = groupIndex + 1
                if (groupChangeType == ListEvent.DELETE &&
                    elementChangeType != ListEvent.INSERT &&
                    shiftGroupIndex < insertedSeparators.colourSize(Barcode.BLACK) &&
                    shiftGroupIndex < grouper.barcode.colourSize(Grouper.UNIQUE) &&
                    joinRight
                ) {
                    val collapsedGroupStartIndex = grouper.barcode.getIndex(shiftGroupIndex, Grouper.UNIQUE)
                    val separatorIndex = insertedSeparators.getIndex(shiftGroupIndex, Barcode.BLACK)
                    if (collapsedGroupStartIndex + shiftGroupIndex < separatorIndex) {
                        insertedSeparators.remove(separatorIndex, 1)
                        updates.elementDeleted(separatorIndex, ListEvent.unknownValue())
                        insertedSeparators.add(collapsedGroupStartIndex + shiftGroupIndex, Barcode.BLACK, 1)
                        updates.elementInserted(collapsedGroupStartIndex + shiftGroupIndex, ListEvent.unknownValue())
                    }
                }

                if (groupChangeType == ListEvent.UPDATE &&
                    elementChangeType == ListEvent.UPDATE &&
                    shiftGroupIndex < insertedSeparators.colourSize(Barcode.BLACK) &&
                    shiftGroupIndex < grouper.barcode.colourSize(Grouper.UNIQUE) &&
                    updateNextSeparator
                ) {
                    shiftSeparator(shiftGroupIndex)
                }
            }

            private fun shiftSeparator(shiftGroupIndex: Int) {
                val collapsedGroupStartIndex = grouper.barcode.getIndex(shiftGroupIndex, Grouper.UNIQUE)
                val separatorIndex = insertedSeparators.getIndex(shiftGroupIndex, Barcode.BLACK)
                val calculatedSeparatorPosition = collapsedGroupStartIndex + shiftGroupIndex
                if (calculatedSeparatorPosition != separatorIndex) {
                    insertedSeparators.remove(separatorIndex, 1)
                    updates.elementDeleted(separatorIndex, ListEvent.unknownValue())
                    insertedSeparators.add(calculatedSeparatorPosition, Barcode.BLACK, 1)
                    val insertPosition =
                        if (calculatedSeparatorPosition < separatorIndex) {
                            calculatedSeparatorPosition
                        } else {
                            calculatedSeparatorPosition - 1
                        }
                    updates.elementInserted(insertPosition, ListEvent.unknownValue())
                }
            }
        }

        /**
         * Kotlin has no package-private source visibility. `internal` preserves Kotlin ownership while its
         * JVM class and constructor are necessarily public; the non-static inner-class descriptor is retained.
         */
        internal open inner class GroupSeparator : Separator<E> {
            @JvmField
            @JvmSynthetic
            internal var limitState = Int.MAX_VALUE
            private var sizeState = 0
            private var firstState: E? = null
            private var node: Element<GroupSeparator>? = null

            override var limit: Int
                get() = limitState
                set(value) {
                    applyLimit(value, true)
                }

            @Suppress("SameParameterValue")
            protected open fun applyLimit(limit: Int, fireEvents: Boolean) {
                if (limitState == limit) return
                val currentNode = node ?: return
                limitState = limit
                if (fireEvents) {
                    updates.beginEvent()
                    val groupIndex = separators.indexOfNode(currentNode, 1)
                    val separatorIndex = insertedSeparators.getIndex(groupIndex, Barcode.BLACK)
                    updates.elementUpdated(separatorIndex, ListEvent.unknownValue(), ListEvent.unknownValue())
                    updates.commitEvent()
                }
            }

            override val group: List<E>
                get() = if (node == null) Collections.emptyList() else source!!.subList(start(), end())

            override fun first(): E = firstState as E

            override fun size(): Int = sizeState

            open fun setNode(node: Element<GroupSeparator>?) {
                this.node = node
            }

            private fun start(): Int {
                val currentNode = checkNotNull(node) { "SeparatorList.Group iterator node is null" }
                val separatorIndex = separators.indexOfNode(currentNode, 1)
                check(separatorIndex != -1) { "SeparatorList.Group separatorIndex not found" }
                val groupStartIndex = insertedSeparators.getIndex(separatorIndex, Barcode.BLACK)
                return groupStartIndex - separatorIndex
            }

            private fun end(): Int {
                val currentNode = checkNotNull(node) { "SeparatorList.Group iterator node is null" }
                val nextSeparatorIndex = separators.indexOfNode(currentNode, 1) + 1
                check(nextSeparatorIndex != 0) { "SeparatorList.Group nextSeparatorIndex is 0" }
                val nextGroupStartIndex =
                    if (nextSeparatorIndex == insertedSeparators.colourSize(Barcode.BLACK)) {
                        insertedSeparators.size()
                    } else {
                        insertedSeparators.getIndex(nextSeparatorIndex, Barcode.BLACK)
                    }
                return nextGroupStartIndex - nextSeparatorIndex
            }

            open fun updateCachedValues() {
                if (node != null) {
                    val start = start()
                    val end = end()
                    firstState = source!![start]
                    sizeState = end - start
                } else {
                    firstState = null
                    sizeState = 0
                }
            }

            override fun toString(): String = "$sizeState elements starting with \"${first()}\""
        }
    }
}
