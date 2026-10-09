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
import ca.odell.glazedlists.impl.adt.Barcode
import ca.odell.glazedlists.impl.adt.BarcodeIterator
import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.MatcherEditor
import ca.odell.glazedlists.matchers.Matchers

@Suppress("INAPPLICABLE_JVM_NAME")
class FilterList<E> private constructor(
    source: EventList<E>,
    matcher: Matcher<in E>?,
    matcherEditor: MatcherEditor<in E>?,
) : TransformedList<E, E>(source) {
    private var flagList = Barcode()
    private var currentMatcher: Matcher<in E>? = Matchers.trueMatcher()
    private var currentEditor: MatcherEditor<in E>? = null
    private val listener: MatcherEditor.Listener<E> = PrivateMatcherEditorListener()

    @Volatile
    private var disposed = false

    constructor(source: EventList<E>) : this(source, null, null)

    constructor(source: EventList<E>, matcher: Matcher<in E>?) : this(source, matcher, null)

    constructor(source: EventList<E>, matcherEditor: MatcherEditor<in E>?) : this(source, null, matcherEditor)

    init {
        var matcherListenerInstalled = false
        var sourceListenerRegistrationAttempted = false
        var initializationFailure: Throwable? = null
        source.readWriteLock.writeLock().lock()
        try {
            flagList.addBlack(0, source.size)
            if (matcher != null) {
                currentMatcher = matcher
                changed()
            } else if (matcherEditor != null) {
                currentEditor = matcherEditor
                matcherListenerInstalled = true
                typedCurrentEditor().addMatcherEditorListener(listener)
                currentMatcher = matcherEditor.matcher
                changed()
            }
            sourceListenerRegistrationAttempted = true
            source.addListEventListener(this)
        } catch (failure: RuntimeException) {
            disposed = true
            initializationFailure = failure
        } catch (failure: Error) {
            disposed = true
            initializationFailure = failure
        } finally {
            source.readWriteLock.writeLock().unlock()
        }

        val failure = initializationFailure
        if (failure != null) {
            if (sourceListenerRegistrationAttempted) {
                try {
                    source.removeListEventListener(this)
                } catch (cleanupFailure: RuntimeException) {
                    failure.addSuppressed(cleanupFailure)
                } catch (cleanupFailure: Error) {
                    failure.addSuppressed(cleanupFailure)
                }
            }
            if (matcherListenerInstalled) {
                try {
                    typedCurrentEditor().removeMatcherEditorListener(listener)
                } catch (cleanupFailure: RuntimeException) {
                    failure.addSuppressed(cleanupFailure)
                } catch (cleanupFailure: Error) {
                    failure.addSuppressed(cleanupFailure)
                }
            }
            currentEditor = null
            currentMatcher = null
            when (failure) {
                is RuntimeException -> throw failure
                is Error -> throw failure
                else -> throw failure
            }
        }
    }

    fun setMatcher(matcher: Matcher<in E>?) {
        if (currentEditor != null) {
            typedCurrentEditor().removeMatcherEditorListener(listener)
            currentEditor = null
        }
        if (matcher != null) {
            changeMatcherWithLocks(null, matcher, MatcherEditor.Event.CHANGED)
        } else {
            changeMatcherWithLocks(null, null, MatcherEditor.Event.MATCH_ALL)
        }
    }

    fun setMatcherEditor(editor: MatcherEditor<in E>?) {
        if (currentEditor != null) {
            typedCurrentEditor().removeMatcherEditorListener(listener)
        }
        currentEditor = editor
        if (editor != null) {
            typedCurrentEditor().addMatcherEditorListener(listener)
            changeMatcherWithLocks(editor, editor.matcher, MatcherEditor.Event.CHANGED)
        } else {
            changeMatcherWithLocks(null, null, MatcherEditor.Event.MATCH_ALL)
        }
    }

    override fun dispose() {
        super.dispose()
        if (currentEditor != null) {
            typedCurrentEditor().removeMatcherEditorListener(listener)
        }
        disposed = true
        currentEditor = null
        currentMatcher = null
    }

    @Suppress("RedundantModalityModifier")
    final override fun listChanged(listChanges: ListEvent<E>) {
        updates.beginEvent()
        if (listChanges.isReordering) {
            val sourceReorderMap = listChanges.reorderMap
            val filterReorderMap = IntArray(flagList.blackSize())
            val previousFlagList = flagList
            flagList = Barcode()
            for (i in sourceReorderMap.indices) {
                val flag = previousFlagList[sourceReorderMap[i]]
                flagList.add(i, flag, 1)
                if (flag !== Barcode.WHITE) {
                    filterReorderMap[flagList.getBlackIndex(i)] = previousFlagList.getBlackIndex(sourceReorderMap[i])
                }
            }
            updates.reorder(filterReorderMap)
        } else {
            while (listChanges.next()) {
                val sourceIndex = listChanges.index
                when (listChanges.type) {
                    ListEvent.DELETE -> {
                        val filteredIndex = flagList.getBlackIndex(sourceIndex)
                        if (filteredIndex != -1) {
                            updates.elementDeleted(filteredIndex, listChanges.oldValue)
                        }
                        flagList.remove(sourceIndex, 1)
                    }

                    ListEvent.INSERT -> {
                        val element = source!![sourceIndex]
                        if (currentMatcher!!.matches(element)) {
                            flagList.addBlack(sourceIndex, 1)
                            updates.elementInserted(flagList.getBlackIndex(sourceIndex), element)
                        } else {
                            flagList.addWhite(sourceIndex, 1)
                        }
                    }

                    ListEvent.UPDATE -> {
                        val filteredIndex = flagList.getBlackIndex(sourceIndex)
                        val wasIncluded = filteredIndex != -1
                        val updated = source!![sourceIndex]
                        val include = currentMatcher!!.matches(updated)
                        if (wasIncluded && !include) {
                            flagList.setWhite(sourceIndex, 1)
                            updates.elementDeleted(filteredIndex, listChanges.oldValue)
                        } else if (!wasIncluded && include) {
                            flagList.setBlack(sourceIndex, 1)
                            updates.elementInserted(flagList.getBlackIndex(sourceIndex), updated)
                        } else if (wasIncluded) {
                            updates.elementUpdated(filteredIndex, listChanges.oldValue, updated)
                        }
                    }
                }
            }
        }
        updates.commitEvent()
    }

    private fun changeMatcherWithLocks(
        matcherEditor: MatcherEditor<in E>?,
        matcher: Matcher<in E>?,
        changeType: Int,
    ) {
        readWriteLock.writeLock().lock()
        try {
            changeMatcher(matcherEditor, matcher, changeType)
        } finally {
            readWriteLock.writeLock().unlock()
        }
    }

    private fun changeMatcher(
        matcherEditor: MatcherEditor<in E>?,
        matcher: Matcher<in E>?,
        changeType: Int,
    ) {
        if (disposed) return
        check(currentEditor === matcherEditor)
        when (changeType) {
            MatcherEditor.Event.CONSTRAINED -> {
                currentMatcher = matcher
                constrained()
            }

            MatcherEditor.Event.RELAXED -> {
                currentMatcher = matcher
                relaxed()
            }

            MatcherEditor.Event.CHANGED -> {
                currentMatcher = matcher
                changed()
            }

            MatcherEditor.Event.MATCH_ALL -> {
                currentMatcher = Matchers.trueMatcher()
                matchAll()
            }

            MatcherEditor.Event.MATCH_NONE -> {
                currentMatcher = Matchers.falseMatcher()
                matchNone()
            }
        }
    }

    private fun matchNone() {
        updates.beginEvent()
        for (element in this) {
            updates.elementDeleted(0, element)
        }
        flagList.clear()
        flagList.addWhite(0, source!!.size)
        updates.commitEvent()
    }

    private fun matchAll() {
        updates.beginEvent()
        val iterator: BarcodeIterator = flagList.iterator()
        while (iterator.hasNextWhite()) {
            iterator.nextWhite()
            val index = iterator.index
            updates.elementInserted(index, source!![index])
        }
        flagList.clear()
        flagList.addBlack(0, source!!.size)
        updates.commitEvent()
    }

    private fun relaxed() {
        updates.beginEvent()
        val iterator: BarcodeIterator = flagList.iterator()
        while (iterator.hasNextWhite()) {
            iterator.nextWhite()
            val element = source!![iterator.index]
            if (currentMatcher!!.matches(element)) {
                updates.elementInserted(iterator.setBlack(), element)
            }
        }
        updates.commitEvent()
    }

    private fun constrained() {
        updates.beginEvent()
        val iterator: BarcodeIterator = flagList.iterator()
        while (iterator.hasNextBlack()) {
            iterator.nextBlack()
            val value = source!![iterator.index]
            if (!currentMatcher!!.matches(value)) {
                val blackIndex = iterator.blackIndex
                iterator.setWhite()
                updates.elementDeleted(blackIndex, value)
            }
        }
        updates.commitEvent()
    }

    private fun changed() {
        updates.beginEvent()
        val iterator: BarcodeIterator = flagList.iterator()
        while (iterator.hasNext()) {
            iterator.next()
            val filteredIndex = iterator.blackIndex
            val wasIncluded = filteredIndex != -1
            val value = source!![iterator.index]
            val include = currentMatcher!!.matches(value)
            if (wasIncluded && !include) {
                iterator.setWhite()
                updates.elementDeleted(filteredIndex, value)
            } else if (!wasIncluded && include) {
                updates.elementInserted(iterator.setBlack(), value)
            }
        }
        updates.commitEvent()
    }

    private open inner class PrivateMatcherEditorListener : MatcherEditor.Listener<E> {
        override fun changedMatcher(matcherEvent: MatcherEditor.Event<E>) {
            changeMatcherWithLocks(matcherEvent.matcherEditor, matcherEvent.matcher, matcherEvent.type)
        }
    }

    @Suppress("UNCHECKED_CAST")
    private fun typedCurrentEditor(): MatcherEditor<E> = currentEditor as MatcherEditor<E>

    @get:JvmName("size")
    override val size: Int
        get() = flagList.blackSize()

    override fun getSourceIndex(mutationIndex: Int): Int = flagList.getIndex(mutationIndex, Barcode.BLACK)

    override fun isWritable(): Boolean = true
}
