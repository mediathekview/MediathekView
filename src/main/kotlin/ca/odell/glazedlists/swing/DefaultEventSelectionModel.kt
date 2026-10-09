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
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.ListSelection
import ca.odell.glazedlists.matchers.Matcher
import java.util.concurrent.CopyOnWriteArrayList
import javax.swing.event.ListSelectionEvent
import javax.swing.event.ListSelectionListener

/** Adapts a [ListSelection] to Swing's selection-model API. */
class DefaultEventSelectionModel<E>(
    private val source: EventList<E>,
    private val disposeSource: Boolean,
) : AdvancedListSelectionModel<E> {
    private val listSelection: ListSelection<E>
    override var enabled = true
    private val selectionListener: ListSelection.Listener = SwingSelectionListener()
    private val listeners: MutableList<ListSelectionListener> = CopyOnWriteArrayList()
    private var valueIsAdjusting = false
    private var fullChangeStart = -1
    private var fullChangeFinish = -1

    init {
        source.readWriteLock.readLock().lock()
        try {
            listSelection = ListSelection(source)
            listSelection.addSelectionListener(selectionListener)
        } finally {
            source.readWriteLock.readLock().unlock()
        }
    }

    constructor(source: EventList<E>) : this(source, false)

    override val selected: EventList<E>
        get() {
            source.readWriteLock.readLock().lock()
            return try {
                listSelection.selected
            } finally {
                source.readWriteLock.readLock().unlock()
            }
        }

    override val togglingSelected: EventList<E>
        get() {
            source.readWriteLock.readLock().lock()
            return try {
                listSelection.togglingSelected
            } finally {
                source.readWriteLock.readLock().unlock()
            }
        }

    override val deselected: EventList<E>
        get() {
            source.readWriteLock.readLock().lock()
            return try {
                listSelection.deselected
            } finally {
                source.readWriteLock.readLock().unlock()
            }
        }

    override val togglingDeselected: EventList<E>
        get() {
            source.readWriteLock.readLock().lock()
            return try {
                listSelection.togglingDeselected
            } finally {
                source.readWriteLock.readLock().unlock()
            }
        }

    private inner class SwingSelectionListener : ListSelection.Listener {
        override fun selectionChanged(changeStart: Int, changeEnd: Int) {
            fireSelectionChanged(changeStart, changeEnd)
        }
    }

    private fun fireSelectionChanged(changeStart: Int, changeFinish: Int) {
        if (valueIsAdjusting) {
            if (fullChangeStart == -1 || changeStart < fullChangeStart) fullChangeStart = changeStart
            if (fullChangeFinish == -1 || changeFinish > fullChangeFinish) fullChangeFinish = changeFinish
        }

        val event = ListSelectionEvent(this, changeStart, changeFinish, valueIsAdjusting)
        for (listener in listeners) listener.valueChanged(event)
    }

    override fun invertSelection() {
        source.readWriteLock.writeLock().lock()
        try {
            listSelection.invertSelection()
        } finally {
            source.readWriteLock.writeLock().unlock()
        }
    }

    override fun setSelectionInterval(index0: Int, index1: Int) {
        if (!enabled) return
        source.readWriteLock.writeLock().lock()
        try {
            listSelection.setSelection(index0, index1)
        } finally {
            source.readWriteLock.writeLock().unlock()
        }
    }

    override fun addSelectionInterval(index0: Int, index1: Int) {
        if (!enabled) return
        source.readWriteLock.writeLock().lock()
        try {
            listSelection.select(index0, index1)
        } finally {
            source.readWriteLock.writeLock().unlock()
        }
    }

    override fun removeSelectionInterval(index0: Int, index1: Int) {
        if (!enabled) return
        if (index0 == 0 && index1 == 0 && source.isEmpty()) return
        source.readWriteLock.writeLock().lock()
        try {
            listSelection.deselect(index0, index1)
        } finally {
            source.readWriteLock.writeLock().unlock()
        }
    }

    override fun isSelectedIndex(index: Int): Boolean = listSelection.isSelected(index)

    override fun getAnchorSelectionIndex(): Int = listSelection.anchorSelectionIndex

    override fun setAnchorSelectionIndex(anchorSelectionIndex: Int) {
        if (!enabled) return
        source.readWriteLock.writeLock().lock()
        try {
            listSelection.anchorSelectionIndex = anchorSelectionIndex
        } finally {
            source.readWriteLock.writeLock().unlock()
        }
    }

    override fun getLeadSelectionIndex(): Int = listSelection.leadSelectionIndex

    override fun setLeadSelectionIndex(leadSelectionIndex: Int) {
        if (!enabled) return
        source.readWriteLock.writeLock().lock()
        try {
            listSelection.leadSelectionIndex = leadSelectionIndex
        } finally {
            source.readWriteLock.writeLock().unlock()
        }
    }

    override fun getMinSelectionIndex(): Int = listSelection.minSelectionIndex

    override fun getMaxSelectionIndex(): Int = listSelection.maxSelectionIndex

    override fun clearSelection() {
        if (!enabled) return
        source.readWriteLock.writeLock().lock()
        try {
            listSelection.deselectAll()
        } finally {
            source.readWriteLock.writeLock().unlock()
        }
    }

    override fun isSelectionEmpty(): Boolean {
        source.readWriteLock.readLock().lock()
        return try {
            listSelection.selected.isEmpty()
        } finally {
            source.readWriteLock.readLock().unlock()
        }
    }

    override fun insertIndexInterval(index: Int, length: Int, before: Boolean) = Unit

    override fun removeIndexInterval(index0: Int, index1: Int) = Unit

    override fun setValueIsAdjusting(valueIsAdjusting: Boolean) {
        this.valueIsAdjusting = valueIsAdjusting
        if (!valueIsAdjusting && fullChangeStart != -1 && fullChangeFinish != -1) {
            source.readWriteLock.writeLock().lock()
            try {
                fireSelectionChanged(fullChangeStart, fullChangeFinish)
                fullChangeStart = -1
                fullChangeFinish = -1
            } finally {
                source.readWriteLock.writeLock().unlock()
            }
        }
    }

    override fun getValueIsAdjusting(): Boolean = valueIsAdjusting

    override fun setSelectionMode(selectionMode: Int) {
        source.readWriteLock.writeLock().lock()
        try {
            listSelection.selectionMode = selectionMode
        } finally {
            source.readWriteLock.writeLock().unlock()
        }
    }

    override fun getSelectionMode(): Int = listSelection.selectionMode

    override fun addValidSelectionMatcher(validSelectionMatcher: Matcher<E>) {
        listSelection.addValidSelectionMatcher(validSelectionMatcher)
    }

    override fun removeValidSelectionMatcher(validSelectionMatcher: Matcher<E>) {
        listSelection.removeValidSelectionMatcher(validSelectionMatcher)
    }

    override fun addListSelectionListener(listener: ListSelectionListener) {
        listeners.add(listener)
    }

    override fun removeListSelectionListener(listener: ListSelectionListener) {
        listeners.remove(listener)
    }

    override fun dispose() {
        listSelection.removeSelectionListener(selectionListener)
        listSelection.dispose()
        if (disposeSource) source.dispose()
    }
}
