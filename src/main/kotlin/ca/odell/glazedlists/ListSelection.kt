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
import ca.odell.glazedlists.event.ListEventAssembler
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.impl.adt.Barcode
import ca.odell.glazedlists.impl.adt.BarcodeIterator
import ca.odell.glazedlists.matchers.Matcher
import java.util.*
import java.util.concurrent.CopyOnWriteArrayList

/**
 * A class to provide index-based selection features. This class maintains two
 * lists derived from a single [EventList]:
 *
 * * [Selected][selected] - an [EventList] that contains only the selected values.
 * * [Deselected][deselected] - an [EventList] that contains only the deselected values.
 *
 * This design is intended to allow the sharing of selection logic between
 * all supported GUI toolkits as well as being available for use in non-GUI
 * applications and for index-based filtering.
 *
 * **Warning:** This class is thread ready but not thread safe. See [EventList]
 * for an example of thread safe code.
 *
 * @author [Kevin Maltby](mailto:kevin@swank.ca)
 */
@Suppress("INAPPLICABLE_JVM_NAME", "REDUNDANT_MODALITY_MODIFIER")
open class ListSelection<E>(
    source: EventList<E>,
) : ListEventListener<E> {
    private val internalSource: EventList<E> = source

    private var selectedList: SelectedList<E>? = null
    private var deselectedList: DeselectedList<E>? = null
    private var selectedToggleList: SelectionToggleList<E>? = null
    private var deselectedToggleList: DeselectionToggleList<E>? = null

    private var barcode = Barcode()

    private var internalLeadSelectionIndex = -1
    private var internalAnchorSelectionIndex = -1
    private var internalSelectionMode = MULTIPLE_INTERVAL_SELECTION_DEFENSIVE

    private val validSelectionMatchers: MutableCollection<Matcher<E>> = ArrayList()

    /**
     * The registered selection listeners. This is also the mutex used while
     * creating each derived list.
     */
    private val selectionListeners = CopyOnWriteArrayList<Listener>()

    init {
        barcode.add(0, DESELECTED, internalSource.size)
        internalSource.addListEventListener(this)
    }

    /**
     * Creates a new ListSelection that listens to changes on the given source
     * and initializes selection with the given array of indices.
     */
    constructor(
        source: EventList<E>,
        initialSelection: IntArray,
    ) : this(source) {
        select(initialSelection)
    }

    /**
     * Handle changes to the source list by adjusting our selection state and
     * the contents of the selected and deselected lists.
     */
    open override fun listChanged(listChanges: ListEvent<E>) {
        val minSelectionIndexBefore = minSelectionIndex
        val maxSelectionIndexBefore = maxSelectionIndex

        var selectionChanged = false

        if (listChanges.isReordering) {
            beginSelected()
            val sourceReorderMap = listChanges.reorderMap
            val selectReorderMap = IntArray(barcode.colourSize(SELECTED))
            val deselectReorderMap = IntArray(barcode.colourSize(DESELECTED))

            val previousBarcode = barcode
            barcode = Barcode()
            for (c in sourceReorderMap.indices) {
                val flag = previousBarcode[sourceReorderMap[c]]
                val wasSelected = flag !== DESELECTED
                barcode.add(c, flag, 1)
                if (wasSelected) {
                    val previousIndex = previousBarcode.getColourIndex(sourceReorderMap[c], SELECTED)
                    val currentIndex = barcode.getColourIndex(c, SELECTED)
                    selectReorderMap[currentIndex] = previousIndex
                } else {
                    val previousIndex = previousBarcode.getColourIndex(sourceReorderMap[c], DESELECTED)
                    val currentIndex = barcode.getColourIndex(c, DESELECTED)
                    deselectReorderMap[currentIndex] = previousIndex
                }
            }

            internalAnchorSelectionIndex = -1
            internalLeadSelectionIndex = -1

            addSelectedReorder(selectReorderMap)
            commitSelected()

            beginDeselected()
            addDeselectedReorder(deselectReorderMap)
            commitDeselected()

            selectionChanged = true
        } else {
            beginAll()
            while (listChanges.next()) {
                val index = listChanges.index
                val changeType = listChanges.type

                val previousSelectionIndex = barcode.getColourIndex(index, SELECTED)
                val previouslySelected = previousSelectionIndex != -1

                if (changeType == ListEvent.DELETE) {
                    if (index <= maxSelectionIndexBefore) {
                        selectionChanged = true
                    }

                    if (previouslySelected) {
                        barcode.remove(index, 1)
                        addSelectedDelete(previousSelectionIndex, listChanges.oldValue)
                    } else {
                        val deselectedIndex = barcode.getColourIndex(index, DESELECTED)
                        addDeselectedDelete(deselectedIndex, listChanges.oldValue)
                        barcode.remove(index, 1)
                    }
                } else if (changeType == ListEvent.INSERT) {
                    if (index <= maxSelectionIndexBefore) {
                        selectionChanged = true
                    }

                    if (previouslySelected) {
                        if (
                            internalSelectionMode == SINGLE_INTERVAL_SELECTION ||
                            internalSelectionMode == MULTIPLE_INTERVAL_SELECTION
                        ) {
                            barcode.add(index, SELECTED, 1)
                            addSelectedInsert(previousSelectionIndex, listChanges.newValue)
                        } else {
                            barcode.add(index, DESELECTED, 1)
                            val deselectedIndex = barcode.getColourIndex(index, DESELECTED)
                            addDeselectedInsert(deselectedIndex, listChanges.newValue)
                        }
                    } else {
                        barcode.add(index, DESELECTED, 1)
                        val deselectedIndex = barcode.getColourIndex(index, DESELECTED)
                        addDeselectedInsert(deselectedIndex, listChanges.newValue)
                    }
                } else if (changeType == ListEvent.UPDATE) {
                    if (previouslySelected) {
                        addSelectedUpdate(previousSelectionIndex, listChanges.oldValue, listChanges.newValue)
                    } else {
                        val deselectedIndex = barcode.getColourIndex(index, DESELECTED)
                        addDeselectedUpdate(deselectedIndex, listChanges.oldValue, listChanges.newValue)
                    }
                }

                internalAnchorSelectionIndex = adjustIndex(internalAnchorSelectionIndex, changeType, index)
                internalLeadSelectionIndex = adjustIndex(internalLeadSelectionIndex, changeType, index)
            }
            commitAll()
        }

        if (minSelectionIndexBefore != -1 && maxSelectionIndexBefore != -1 && selectionChanged) {
            val minSelectionIndexAfter = minSelectionIndex
            val maxSelectionIndexAfter = maxSelectionIndex

            var changeStart = minSelectionIndexBefore
            var changeFinish = maxSelectionIndexBefore

            if (minSelectionIndexAfter != -1 && minSelectionIndexAfter < changeStart) {
                changeStart = minSelectionIndexAfter
            }
            if (maxSelectionIndexAfter != -1 && maxSelectionIndexAfter > changeFinish) {
                changeFinish = maxSelectionIndexAfter
            }

            fireSelectionChanged(changeStart, changeFinish)
        }
    }

    /**
     * Add a matcher which decides when source elements are valid for selection.
     */
    open fun addValidSelectionMatcher(validSelectionMatcher: Matcher<E>) {
        validSelectionMatchers.add(validSelectionMatcher)

        var i = minSelectionIndex
        val n = maxSelectionIndex
        while (i <= n) {
            if (isSelected(i) && !isSelectable(i)) {
                deselect(i)
            }
            i++
        }
    }

    /**
     * Remove a matcher which decides when source elements are valid for selection.
     */
    open fun removeValidSelectionMatcher(validSelectionMatcher: Matcher<E>) {
        validSelectionMatchers.remove(validSelectionMatcher)
    }

    private fun adjustIndex(
        indexBefore: Int,
        changeType: Int,
        changeIndex: Int,
    ): Int {
        if (indexBefore == -1) return -1
        return if (changeType == ListEvent.DELETE) {
            if (changeIndex < indexBefore) {
                indexBefore - 1
            } else if (changeIndex == indexBefore) {
                -1
            } else {
                indexBefore
            }
        } else if (changeType == ListEvent.UPDATE) {
            indexBefore
        } else if (changeType == ListEvent.INSERT) {
            if (changeIndex <= indexBefore) indexBefore + 1 else indexBefore
        } else {
            throw IllegalStateException()
        }
    }

    /**
     * Gets an [EventList] that contains only selected values and modifies the
     * source list on mutation.
     */
    open val selected: EventList<E>
        get() {
            synchronized(selectionListeners) {
                if (selectedList == null) {
                    selectedList = SelectedList(internalSource).also {
                        internalSource.publisher.setRelatedListener(it, this)
                    }
                }
            }
            return selectedList!!
        }

    /**
     * Gets an [EventList] that contains only selected values and modifies the
     * selection state on mutation.
     */
    open val togglingSelected: EventList<E>
        get() {
            synchronized(selectionListeners) {
                if (selectedToggleList == null) {
                    selectedToggleList = SelectionToggleList(internalSource).also {
                        internalSource.publisher.setRelatedListener(it, this)
                    }
                }
            }
            return selectedToggleList!!
        }

    /**
     * Gets an [EventList] that contains only deselected values and modifies the
     * source list on mutation.
     */
    open val deselected: EventList<E>
        get() {
            synchronized(selectionListeners) {
                if (deselectedList == null) {
                    deselectedList = DeselectedList(internalSource).also {
                        internalSource.publisher.setRelatedListener(it, this)
                    }
                }
            }
            return deselectedList!!
        }

    /**
     * Gets an [EventList] that contains only deselected values and modifies the
     * selection state on mutation.
     */
    open val togglingDeselected: EventList<E>
        get() {
            synchronized(selectionListeners) {
                if (deselectedToggleList == null) {
                    deselectedToggleList = DeselectionToggleList(internalSource).also {
                        internalSource.publisher.setRelatedListener(it, this)
                    }
                }
            }
            return deselectedToggleList!!
        }

    /**
     * Get the [EventList] that selection is being managed for.
     */
    open val source: EventList<E>
        get() = internalSource

    /**
     * Inverts the current selection.
     */
    open fun invertSelection() {
        internalAnchorSelectionIndex = -1
        internalLeadSelectionIndex = -1

        beginAll()
        val i = barcode.iterator()
        while (i.hasNext()) {
            val color = i.next()
            val value = internalSource[i.index]
            val originalIndex = i.getColourIndex(color)

            if (color === SELECTED) {
                i.set(DESELECTED)
                val newIndex = i.getColourIndex(DESELECTED)
                addDeselectEvent(originalIndex, newIndex, value)
            } else {
                i.set(SELECTED)
                val newIndex = i.getColourIndex(SELECTED)
                addSelectEvent(newIndex, originalIndex, value)
            }
        }
        commitAll()

        fireSelectionChanged(0, internalSource.size - 1)
    }

    /**
     * Returns whether or not the item with the given source index is selected.
     */
    open fun isSelected(sourceIndex: Int): Boolean {
        return sourceIndex in internalSource.indices && barcode.getColourIndex(sourceIndex, SELECTED) != -1
    }

    /**
     * Deselects the element at the given index.
     */
    open fun deselect(index: Int) {
        deselect(index, index)
    }

    /**
     * Deselects all of the elements within the given range.
     */
    open fun deselect(
        start: Int,
        end: Int,
    ) {
        var adjustedEnd = end
        if (start == -1 || adjustedEnd == -1) {
            return
        } else if (internalSelectionMode == SINGLE_SELECTION) {
            val selectedIndex = maxSelectionIndex
            if (selectedIndex in start..adjustedEnd) {
                deselectAll()
            }
            return
        } else if (internalSelectionMode == SINGLE_INTERVAL_SELECTION && start > minSelectionIndex) {
            adjustedEnd = maxOf(adjustedEnd, maxSelectionIndex)
        }

        val oldAnchor = if (internalAnchorSelectionIndex == start) -1 else internalAnchorSelectionIndex
        val oldLead = if (internalLeadSelectionIndex == adjustedEnd) -1 else internalLeadSelectionIndex

        internalAnchorSelectionIndex = start
        internalLeadSelectionIndex = adjustedEnd

        setSubRangeOfRange(false, start, adjustedEnd, -1, -1, oldLead, oldAnchor)
    }

    /**
     * Deselects all of the elements in the given array of indices.
     */
    open fun deselect(indices: IntArray) {
        var firstAffectedIndex = -1
        var lastAffectedIndex = -1

        beginAll()
        var currentIndex = 0
        val i = barcode.iterator()
        while (i.hasNext() && currentIndex != indices.size) {
            val value = i.next()
            if (i.index == indices[currentIndex]) {
                if (value === SELECTED) {
                    if (firstAffectedIndex == -1) firstAffectedIndex = i.index
                    lastAffectedIndex = i.index
                    addDeselectEvent(i)
                }
                currentIndex++
            }
        }
        commitAll()

        if (firstAffectedIndex > -1) {
            fireSelectionChanged(firstAffectedIndex, lastAffectedIndex)
        }
    }

    /**
     * Deselect all elements.
     */
    open fun deselectAll() {
        setAllColor(DESELECTED)
    }

    private fun setAllColor(color: Any) {
        val oppositeColor = if (color === SELECTED) DESELECTED else SELECTED
        if (barcode.colourSize(oppositeColor) == 0) return

        var firstAffectedIndex = -1
        var lastAffectedIndex = -1

        beginAll()
        val i = barcode.iterator()
        while (i.hasNextColour(oppositeColor)) {
            i.nextColour(oppositeColor)
            val index = i.index
            val value = internalSource[index]
            if (color === SELECTED) {
                addDeselectedDelete(0, value)
                addSelectedInsert(index, value)
            } else {
                addSelectedDelete(0, value)
                addDeselectedInsert(index, value)
            }

            if (firstAffectedIndex == -1) firstAffectedIndex = index
            lastAffectedIndex = index
        }

        barcode.clear()
        barcode.add(0, color, internalSource.size)

        commitAll()
        fireSelectionChanged(firstAffectedIndex, lastAffectedIndex)
    }

    /**
     * Selects the element at the given index.
     */
    open fun select(index: Int) {
        select(index, index)
    }

    /**
     * Selects all of the elements within the given range.
     */
    open fun select(
        start: Int,
        end: Int,
    ) {
        if (start == -1 || end == -1) {
            return
        } else if (internalSelectionMode == SINGLE_SELECTION) {
            setSelection(start)
            return
        } else if (internalSelectionMode == SINGLE_INTERVAL_SELECTION) {
            var overlap = false
            val minSelectedIndex = minSelectionIndex
            val maxSelectedIndex = maxSelectionIndex
            if (minSelectedIndex - 1 <= start && start <= maxSelectedIndex + 1) overlap = true
            if (minSelectedIndex - 1 <= end && end <= maxSelectedIndex + 1) overlap = true

            if (!overlap) {
                setSelection(start, end)
                return
            }
        }

        val oldAnchor = if (internalAnchorSelectionIndex == start) -1 else internalAnchorSelectionIndex
        val oldLead = if (internalLeadSelectionIndex == end) -1 else internalLeadSelectionIndex

        internalAnchorSelectionIndex = start
        internalLeadSelectionIndex = end

        setSubRangeOfRange(true, start, end, -1, -1, oldLead, oldAnchor)
    }

    /**
     * Selects all of the elements in the given array of indices.
     */
    open fun select(indices: IntArray) {
        var firstAffectedIndex = -1
        var lastAffectedIndex = -1

        beginAll()
        var currentIndex = 0
        val i = barcode.iterator()
        while (i.hasNext() && currentIndex != indices.size) {
            val value = i.next()
            if (i.index == indices[currentIndex]) {
                if (value !== SELECTED) {
                    if (firstAffectedIndex == -1) firstAffectedIndex = i.index
                    lastAffectedIndex = i.index
                    addSelectEvent(i)
                }
                currentIndex++
            }
        }
        commitAll()

        if (firstAffectedIndex > -1) {
            fireSelectionChanged(firstAffectedIndex, lastAffectedIndex)
        }
    }

    /**
     * Select the specified element, if it exists.
     */
    @JvmName("select")
    open fun selectValue(value: E): Int {
        val index = internalSource.indexOf(value)
        if (index != -1) select(index)
        return index
    }

    /**
     * Select all of the specified values.
     */
    open fun select(values: Collection<E>): Boolean {
        val indicesToSelect = TreeSet<Int>()
        for (value in values) {
            val index = internalSource.indexOf(value)
            if (index == -1) continue
            indicesToSelect.add(index)
        }
        if (indicesToSelect.isEmpty()) return false

        val indicesToSelectAsInts = indicesToSelect.toIntArray()

        val selectionSizeBefore = selected.size
        select(indicesToSelectAsInts)
        val selectionSizeAfter = selected.size
        return selectionSizeAfter > selectionSizeBefore
    }

    /**
     * Selects all elements.
     */
    open fun selectAll() {
        setAllColor(SELECTED)
    }

    /**
     * Sets the selection to be only the element at the given index.
     */
    open fun setSelection(index: Int) {
        setSelection(index, index)
    }

    /**
     * Sets the selection to be only elements within the given range.
     */
    open fun setSelection(
        start: Int,
        end: Int,
    ) {
        var adjustedEnd = end
        if (start == -1 || adjustedEnd == -1) {
            deselectAll()
            return
        } else if (internalSelectionMode == SINGLE_SELECTION) {
            adjustedEnd = start
        }

        val oldAnchor = if (internalAnchorSelectionIndex == start) -1 else internalAnchorSelectionIndex
        val oldLead = if (internalLeadSelectionIndex == adjustedEnd) -1 else internalLeadSelectionIndex

        internalAnchorSelectionIndex = start
        internalLeadSelectionIndex = adjustedEnd

        setSubRangeOfRange(
            true,
            start,
            adjustedEnd,
            minSelectionIndex,
            maxSelectionIndex,
            oldLead,
            oldAnchor,
        )
    }

    /**
     * Sets the selection to be only the elements in the given array of indices.
     */
    open fun setSelection(indices: IntArray) {
        if (indices.isEmpty()) {
            deselectAll()
            return
        }

        var firstAffectedIndex = -1
        var lastAffectedIndex = -1

        beginAll()
        var currentIndex = 0
        val i = barcode.iterator()
        while (i.hasNext()) {
            val value = i.next()
            if (i.index == indices[currentIndex]) {
                if (value !== SELECTED) {
                    if (firstAffectedIndex == -1) firstAffectedIndex = i.index
                    lastAffectedIndex = i.index
                    addSelectEvent(i)
                }

                if (currentIndex < indices.size - 1) currentIndex++
            } else if (value === SELECTED) {
                if (firstAffectedIndex == -1) firstAffectedIndex = i.index
                lastAffectedIndex = i.index
                addDeselectEvent(i)
            }
        }
        commitAll()

        if (firstAffectedIndex > -1) {
            fireSelectionChanged(firstAffectedIndex, lastAffectedIndex)
        }
    }

    /**
     * Return the anchor of the current selection.
     */
    open var anchorSelectionIndex: Int
        get() = internalAnchorSelectionIndex
        set(value) {
            val oldAnchor = if (internalAnchorSelectionIndex == value) -1 else value

            internalAnchorSelectionIndex = value

            if (value == -1 || internalLeadSelectionIndex == -1) {
                deselectAll()
            } else if (internalSelectionMode == SINGLE_SELECTION) {
                setSubRangeOfRange(
                    true,
                    value,
                    value,
                    minSelectionIndex,
                    maxSelectionIndex,
                    -1,
                    oldAnchor,
                )
            } else if (internalSelectionMode == SINGLE_INTERVAL_SELECTION) {
                setSubRangeOfRange(
                    true,
                    value,
                    internalLeadSelectionIndex,
                    minSelectionIndex,
                    maxSelectionIndex,
                    -1,
                    oldAnchor,
                )
            } else {
                setSubRangeOfRange(true, value, internalLeadSelectionIndex, -1, -1, -1, oldAnchor)
            }
        }

    /**
     * Return the lead of the current selection.
     */
    open var leadSelectionIndex: Int
        get() = internalLeadSelectionIndex
        set(value) {
            val oldLead = if (internalLeadSelectionIndex == value) -1 else value

            val originalLeadIndex = internalLeadSelectionIndex
            internalLeadSelectionIndex = value

            if (value == -1 || internalAnchorSelectionIndex == -1) {
                deselectAll()
            } else if (internalSelectionMode == SINGLE_SELECTION) {
                setSubRangeOfRange(
                    true,
                    value,
                    value,
                    minSelectionIndex,
                    maxSelectionIndex,
                    oldLead,
                    -1,
                )
            } else if (internalSelectionMode == SINGLE_INTERVAL_SELECTION) {
                setSubRangeOfRange(
                    true,
                    internalAnchorSelectionIndex,
                    value,
                    minSelectionIndex,
                    maxSelectionIndex,
                    oldLead,
                    -1,
                )
            } else {
                setSubRangeOfRange(
                    true,
                    internalAnchorSelectionIndex,
                    value,
                    internalAnchorSelectionIndex,
                    originalLeadIndex,
                    oldLead,
                    -1,
                )
            }
        }

    private fun addSelectedReorder(selectReorderMap: IntArray) {
        selectedList?.updates()?.reorder(selectReorderMap)
        selectedToggleList?.updates()?.reorder(selectReorderMap)
    }

    private fun addDeselectedReorder(deselectReorderMap: IntArray) {
        deselectedList?.updates()?.reorder(deselectReorderMap)
        deselectedToggleList?.updates()?.reorder(deselectReorderMap)
    }

    private fun addSelectEvent(i: BarcodeIterator) {
        val value = internalSource[i.index]
        val deselectedIndex = i.getColourIndex(DESELECTED)
        val selectedIndex = i.set(SELECTED)
        addSelectEvent(selectedIndex, deselectedIndex, value)
    }

    private fun addSelectEvent(
        selectIndex: Int,
        deselectIndex: Int,
        value: E,
    ) {
        addDeselectedDelete(deselectIndex, value)
        addSelectedInsert(selectIndex, value)
    }

    private fun addDeselectEvent(i: BarcodeIterator) {
        val value = internalSource[i.index]
        val selectedIndex = i.getColourIndex(SELECTED)
        val deselectedIndex = i.set(DESELECTED)
        addDeselectEvent(selectedIndex, deselectedIndex, value)
    }

    private fun addDeselectEvent(
        selectIndex: Int,
        deselectIndex: Int,
        value: E,
    ) {
        addSelectedDelete(selectIndex, value)
        addDeselectedInsert(deselectIndex, value)
    }

    private fun addSelectedInsert(
        index: Int,
        newValue: E,
    ) {
        selectedList?.updates()?.elementInserted(index, newValue)
        selectedToggleList?.updates()?.elementInserted(index, newValue)
    }

    private fun addSelectedUpdate(
        index: Int,
        oldValue: E,
        newValue: E,
    ) {
        selectedList?.updates()?.elementUpdated(index, oldValue, newValue)
        selectedToggleList?.updates()?.elementUpdated(index, oldValue, newValue)
    }

    private fun addSelectedDelete(
        index: Int,
        oldValue: E,
    ) {
        selectedList?.updates()?.elementDeleted(index, oldValue)
        selectedToggleList?.updates()?.elementDeleted(index, oldValue)
    }

    private fun addDeselectedInsert(
        index: Int,
        value: E,
    ) {
        deselectedList?.updates()?.elementInserted(index, value)
        deselectedToggleList?.updates()?.elementInserted(index, value)
    }

    private fun addDeselectedDelete(
        index: Int,
        oldValue: E,
    ) {
        deselectedList?.updates()?.elementDeleted(index, oldValue)
        deselectedToggleList?.updates()?.elementDeleted(index, oldValue)
    }

    private fun addDeselectedUpdate(
        index: Int,
        oldValue: E,
        newValue: E,
    ) {
        deselectedList?.updates()?.elementUpdated(index, oldValue, newValue)
        deselectedToggleList?.updates()?.elementUpdated(index, oldValue, newValue)
    }

    private fun beginAll() {
        beginSelected()
        beginDeselected()
    }

    private fun commitAll() {
        commitSelected()
        commitDeselected()
    }

    private fun beginSelected() {
        selectedList?.updates()?.beginEvent()
        selectedToggleList?.updates()?.beginEvent()
    }

    private fun commitSelected() {
        selectedList?.updates()?.commitEvent()
        selectedToggleList?.updates()?.commitEvent()
    }

    private fun beginDeselected() {
        deselectedList?.updates()?.beginEvent()
        deselectedToggleList?.updates()?.beginEvent()
    }

    private fun commitDeselected() {
        deselectedList?.updates()?.commitEvent()
        deselectedToggleList?.updates()?.commitEvent()
    }

    /**
     * Set and return the current selection mode.
     */
    open var selectionMode: Int
        get() = internalSelectionMode
        set(value) {
            internalSelectionMode = value
            setSelection(minSelectionIndex, maxSelectionIndex)
        }

    /**
     * Returns the first selected index or -1 if nothing is selected.
     */
    open val minSelectionIndex: Int
        get() {
            if (barcode.colourSize(SELECTED) == 0) return -1
            return barcode.getIndex(0, SELECTED)
        }

    /**
     * Returns the last selected index or -1 if nothing is selected.
     */
    open val maxSelectionIndex: Int
        get() {
            if (barcode.colourSize(SELECTED) == 0) return -1
            return barcode.getIndex(barcode.colourSize(SELECTED) - 1, SELECTED)
        }

    private fun setSubRangeOfRange(
        selectArgument: Boolean,
        changeIndex0Argument: Int,
        changeIndex1Argument: Int,
        invertIndex0Argument: Int,
        invertIndex1Argument: Int,
        oldLead: Int,
        oldAnchor: Int,
    ) {
        var select = selectArgument
        var changeIndex0 = changeIndex0Argument
        var changeIndex1 = changeIndex1Argument
        var invertIndex0 = invertIndex0Argument
        var invertIndex1 = invertIndex1Argument

        if (
            changeIndex0 >= internalSource.size ||
            changeIndex1 >= internalSource.size ||
            ((changeIndex0 == -1 || changeIndex1 == -1) && changeIndex0 != changeIndex1)
        ) {
            throw IndexOutOfBoundsException(
                "Invalid range for selection: $changeIndex0-$changeIndex1, list size is ${internalSource.size}",
            )
        }
        if (
            invertIndex0 >= internalSource.size ||
            invertIndex1 >= internalSource.size ||
            ((invertIndex0 == -1 || invertIndex1 == -1) && invertIndex0 != invertIndex1)
        ) {
            throw IndexOutOfBoundsException(
                "Invalid range for invert selection: $invertIndex0-$invertIndex1, list size is ${internalSource.size}",
            )
        }

        if (changeIndex0 == -1) {
            if (invertIndex0 == -1) return
            changeIndex0 = invertIndex0
            changeIndex1 = invertIndex1
            select = !select
        }
        if (invertIndex0 == -1) {
            invertIndex0 = changeIndex0
            invertIndex1 = changeIndex1
        }

        val minChangeIndex = minOf(changeIndex0, changeIndex1)
        val maxChangeIndex = maxOf(changeIndex0, changeIndex1)
        val minInvertIndex = minOf(invertIndex0, invertIndex1)
        val maxInvertIndex = maxOf(invertIndex0, invertIndex1)

        val minUnionIndex = minOf(minChangeIndex, minInvertIndex)
        val maxUnionIndex = maxOf(maxChangeIndex, maxInvertIndex)

        var minChangedIndex = maxUnionIndex + 1
        var maxChangedIndex = minUnionIndex - 1
        beginAll()

        for (i in minUnionIndex..maxUnionIndex) {
            val selectionIndex = barcode.getColourIndex(i, SELECTED)
            val selectedBefore = selectionIndex != -1
            val inChangeRange = i in minChangeIndex..maxChangeIndex
            val selectedAfter = inChangeRange == select && isSelectable(i)

            if (selectedBefore != selectedAfter) {
                val value = internalSource[i]

                if (i < minChangedIndex) minChangedIndex = i
                if (i > maxChangedIndex) maxChangedIndex = i

                if (selectedBefore) {
                    barcode.set(i, DESELECTED, 1)
                    addDeselectEvent(selectionIndex, i - selectionIndex, value)
                } else {
                    barcode.set(i, SELECTED, 1)
                    val newSelectionIndex = barcode.getColourIndex(i, SELECTED)
                    addSelectEvent(newSelectionIndex, i - newSelectionIndex, value)
                }
            }
        }
        commitAll()

        if (oldLead != -1) {
            minChangedIndex = minOf(minChangedIndex, oldLead)
            maxChangedIndex = maxOf(maxChangedIndex, oldLead)
        }

        if (oldAnchor != -1) {
            minChangedIndex = minOf(minChangedIndex, oldAnchor)
            maxChangedIndex = maxOf(maxChangedIndex, oldAnchor)
        }

        if (minChangedIndex <= maxChangedIndex) {
            fireSelectionChanged(minChangedIndex, maxChangedIndex)
        }
    }

    private fun isSelectable(index: Int): Boolean {
        if (validSelectionMatchers.isEmpty()) {
            return true
        }

        val rowObject = internalSource[index]
        for (validSelectionMatcher in validSelectionMatchers) {
            if (!validSelectionMatcher.matches(rowObject)) {
                return false
            }
        }

        return true
    }

    /**
     * Register a [Listener] that will be notified when selection is changed.
     */
    open fun addSelectionListener(selectionListener: Listener) {
        selectionListeners.add(selectionListener)
    }

    /**
     * Remove a [Listener] so that it will no longer be notified when selection changes.
     */
    open fun removeSelectionListener(selectionListener: Listener) {
        selectionListeners.remove(selectionListener)
    }

    private fun fireSelectionChanged(
        start: Int,
        end: Int,
    ) {
        for (selectionListener in selectionListeners) {
            selectionListener.selectionChanged(start, end)
        }
    }

    /**
     * Disposes of this ListSelection, freeing its resources for garbage collection.
     */
    open fun dispose() {
        internalSource.removeListEventListener(this)
        selectionListeners.clear()

        selectedList?.let { internalSource.publisher.clearRelatedListener(it, this) }
        deselectedList?.let { internalSource.publisher.clearRelatedListener(it, this) }
        selectedToggleList?.let { internalSource.publisher.clearRelatedListener(it, this) }
        deselectedToggleList?.let { internalSource.publisher.clearRelatedListener(it, this) }
    }

    private fun selectedSize(): Int = barcode.colourSize(SELECTED)

    private fun selectedSourceIndex(index: Int): Int = barcode.getIndex(index, SELECTED)

    private fun deselectedSize(): Int = barcode.colourSize(DESELECTED)

    private fun deselectedSourceIndex(index: Int): Int = barcode.getIndex(index, DESELECTED)

    /**
     * A generic interface to respond to changes in selection that doesn't
     * require including a particular GUI toolkit.
     */
    @FunctionalInterface
    fun interface Listener {
        fun selectionChanged(
            changeStart: Int,
            changeEnd: Int,
        )
    }

    /**
     * The [EventList] that contains only values that are currently selected.
     */
    private open inner class SelectedList<E>(
        source: EventList<E>,
    ) : TransformedList<E, E>(source) {
        @get:JvmName("size")
        open override val size: Int
            get() = selectedSize()

        open override fun getSourceIndex(mutationIndex: Int): Int = selectedSourceIndex(mutationIndex)

        open override fun listChanged(listChanges: ListEvent<E>) {
            // Do nothing as all state changes are handled in ListSelection.listChanged().
        }

        open fun updates(): ListEventAssembler<E> = updates

        open override fun isWritable(): Boolean = true

        open override fun dispose() {
            // Do nothing.
        }
    }

    /**
     * A SelectedList that mutates the selection instead of the underlying list.
     */
    private inner class SelectionToggleList<E>(
        source: EventList<E>,
    ) : SelectedList<E>(source) {
        override fun set(
            index: Int,
            element: E,
        ): E {
            throw UnsupportedOperationException("Toggling lists don't support setting items")
        }

        override fun add(
            index: Int,
            element: E,
        ) {
            val sourceIndex = this.source!!.indexOf(element)
            require(sourceIndex != -1) { "Added item $element must be in source list" }
            this@ListSelection.select(sourceIndex)
        }

        @JvmName("remove")
        override fun removeAt(index: Int): E {
            if (index !in indices) {
                throw IndexOutOfBoundsException("Cannot remove at $index on list of size $size")
            }
            val sourceIndex = getSourceIndex(index)
            this@ListSelection.deselect(sourceIndex)
            return this.source!![sourceIndex]
        }
    }

    /**
     * The [EventList] that contains only values that are not currently selected.
     */
    private open inner class DeselectedList<E>(
        source: EventList<E>,
    ) : TransformedList<E, E>(source) {
        @get:JvmName("size")
        open override val size: Int
            get() = deselectedSize()

        open override fun getSourceIndex(mutationIndex: Int): Int = deselectedSourceIndex(mutationIndex)

        open override fun listChanged(listChanges: ListEvent<E>) {
            // Do nothing as all state changes are handled in ListSelection.listChanged().
        }

        open fun updates(): ListEventAssembler<E> = updates

        open override fun isWritable(): Boolean = true

        open override fun dispose() {
            // Do nothing.
        }
    }

    /**
     * A DeselectedList that mutates the selection instead of the underlying list.
     */
    private inner class DeselectionToggleList<E>(
        source: EventList<E>,
    ) : DeselectedList<E>(source) {
        override fun set(
            index: Int,
            element: E,
        ): E {
            throw UnsupportedOperationException("Toggling lists don't support setting items")
        }

        override fun add(
            index: Int,
            element: E,
        ) {
            val sourceIndex = this.source!!.indexOf(element)
            require(sourceIndex != -1) { "Added item $element must be in source list" }
            this@ListSelection.deselect(sourceIndex)
        }

        @JvmName("remove")
        override fun removeAt(index: Int): E {
            if (index !in indices) {
                throw IndexOutOfBoundsException("Cannot remove at $index on list of size $size")
            }
            val sourceIndex = getSourceIndex(index)
            this@ListSelection.select(sourceIndex)
            return this.source!![sourceIndex]
        }
    }

    companion object {
        const val SINGLE_SELECTION = 0
        const val SINGLE_INTERVAL_SELECTION = 1
        const val MULTIPLE_INTERVAL_SELECTION = 2
        const val MULTIPLE_INTERVAL_SELECTION_DEFENSIVE = 103

        private val SELECTED: Any = Barcode.BLACK
        private val DESELECTED: Any = Barcode.WHITE
    }
}
