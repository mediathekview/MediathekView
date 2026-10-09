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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.impl

import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.impl.adt.Barcode
import java.util.*

private val UNIQUE_WITH_DUPLICATE = Any()
private val TODO = Barcode.BLACK
private val DONE = Barcode.WHITE

private const val LEFT_GROUP = -1
private const val NO_GROUP = 0
private const val RIGHT_GROUP = 1

/**
 * Manages the groups created by dividing a [SortedList] using a [Comparator].
 * The [Client] receives callbacks as groups are inserted, updated, and deleted.
 */
internal open class Grouper<E>(
    private val sortedList: SortedList<E>,
    private val clientState: Client<E>,
) {
    private var comparatorState: Comparator<in E>? = null
    private var barcodeState: Barcode? = null

    open val client: Client<E>
        get() = clientState

    open var comparator: Comparator<in E>?
        get() = comparatorState
        set(value) {
            if (comparatorState === value) return
            comparatorState = value

            val rebuiltBarcode = Barcode()
            barcodeState = rebuiltBarcode
            for (index in sortedList.indices) {
                rebuiltBarcode.add(index, if (groupTogether(index, index - 1)) DUPLICATE else UNIQUE, 1)
            }
        }

    open val barcode: Barcode
        get() = platformValue(barcodeState)

    private val internalClient: Client<E>
        get() = clientState

    private val internalComparator: Comparator<in E>
        get() = comparatorState!!

    private val internalBarcode: Barcode
        get() = barcodeState!!

    init {
        comparator = sortedList.comparator
    }

    /**
     * Updates the grouping state in two passes and reports the resulting group changes to [client].
     */
    @Suppress("BooleanLiteralArgument")
    open fun listChanged(listChanges: ListEvent<E>) {
        val toDoList = Barcode()
        toDoList.addWhite(0, internalBarcode.size())

        val removedValues = ArrayDeque<Any>()
        var lastFakedUniqueChangeIndex = -1
        while (listChanges.next()) {
            val changeIndex = listChanges.index
            val changeType = listChanges.type

            when (changeType) {
                ListEvent.INSERT -> {
                    internalBarcode.add(changeIndex, UNIQUE, 1)
                    toDoList.add(changeIndex, TODO, 1)
                }

                ListEvent.UPDATE -> {
                    if (internalBarcode[changeIndex] === UNIQUE &&
                        changeIndex + 1 < internalBarcode.size() &&
                        internalBarcode[changeIndex + 1] === DUPLICATE &&
                        (changeIndex == 0 || !uniqueElementAddedToLeftInSameGroup(toDoList, changeIndex))
                    ) {
                        if (changeIndex != lastFakedUniqueChangeIndex) {
                            internalBarcode.set(changeIndex, UNIQUE, 2)
                            toDoList.set(changeIndex, TODO, 1)
                            lastFakedUniqueChangeIndex = changeIndex + 1
                        }
                    }
                }

                ListEvent.DELETE -> {
                    var deleted = internalBarcode[changeIndex]
                    internalBarcode.remove(changeIndex, 1)
                    toDoList.remove(changeIndex, 1)

                    if (deleted === UNIQUE &&
                        changeIndex < internalBarcode.size() &&
                        internalBarcode[changeIndex] === DUPLICATE
                    ) {
                        internalBarcode.set(changeIndex, UNIQUE, 1)
                        deleted = UNIQUE_WITH_DUPLICATE
                    }
                    removedValues.addLast(deleted)
                }
            }
        }

        val tryJoinResult = TryJoinResult<E>()
        listChanges.reset()
        while (listChanges.next()) {
            val changeIndex = listChanges.index
            val changeType = listChanges.type
            val oldValue = listChanges.oldValue

            when (changeType) {
                ListEvent.INSERT -> {
                    tryJoinExistingGroup(changeIndex, toDoList, tryJoinResult)
                    if (tryJoinResult.group == NO_GROUP) {
                        internalClient.groupChanged(
                            changeIndex,
                            tryJoinResult.groupIndex,
                            ListEvent.INSERT,
                            true,
                            changeType,
                            ListEvent.unknownValue(),
                            tryJoinResult.newFirstInGroup,
                            false,
                            false,
                        )
                    } else {
                        internalClient.groupChanged(
                            changeIndex,
                            tryJoinResult.groupIndex,
                            ListEvent.UPDATE,
                            true,
                            changeType,
                            tryJoinResult.oldFirstInGroup,
                            tryJoinResult.newFirstInGroup,
                            false,
                            false,
                        )
                    }
                }

                ListEvent.UPDATE -> {
                    var oldGroup = when {
                        toDoList[changeIndex] === TODO ->
                            if (changeIndex + 1 < internalBarcode.size()) RIGHT_GROUP else NO_GROUP

                        internalBarcode[changeIndex] === DUPLICATE -> LEFT_GROUP
                        else -> NO_GROUP
                    }

                    tryJoinExistingGroup(changeIndex, toDoList, tryJoinResult)

                    val successor = changeIndex + 1
                    if (tryJoinResult.group == LEFT_GROUP) {
                        if (successor < internalBarcode.size() &&
                            internalBarcode[successor] === UNIQUE &&
                            toDoList[successor] === DONE &&
                            groupTogether(changeIndex, successor)
                        ) {
                            internalBarcode.set(successor, DUPLICATE, 1)
                            oldGroup = NO_GROUP
                        }
                    } else if (tryJoinResult.group == NO_GROUP &&
                        successor < internalBarcode.size() &&
                        internalBarcode[successor] === DUPLICATE &&
                        !groupTogether(changeIndex, successor)
                    ) {
                        internalBarcode.set(successor, UNIQUE, 1)
                        oldGroup = RIGHT_GROUP
                    }

                    val groupIndex = tryJoinResult.groupIndex
                    when (tryJoinResult.group) {
                        NO_GROUP -> when (oldGroup) {
                            NO_GROUP -> internalClient.groupChanged(
                                changeIndex,
                                groupIndex,
                                ListEvent.UPDATE,
                                true,
                                changeType,
                                oldValue,
                                tryJoinResult.newFirstInGroup,
                                false,
                                false,
                            )

                            LEFT_GROUP -> {
                                val firstFromPreviousGroup = sortedList[internalBarcode.getIndex(groupIndex - 1, UNIQUE)]
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex - 1,
                                    ListEvent.UPDATE,
                                    false,
                                    changeType,
                                    firstFromPreviousGroup,
                                    firstFromPreviousGroup,
                                    false,
                                    false,
                                )
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex,
                                    ListEvent.INSERT,
                                    true,
                                    changeType,
                                    ListEvent.unknownValue(),
                                    tryJoinResult.newFirstInGroup,
                                    false,
                                    false,
                                )
                            }

                            RIGHT_GROUP -> {
                                val firstFromNextGroup = sortedList[internalBarcode.getIndex(groupIndex + 1, UNIQUE)]
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex,
                                    ListEvent.INSERT,
                                    true,
                                    changeType,
                                    ListEvent.unknownValue(),
                                    tryJoinResult.newFirstInGroup,
                                    false,
                                    false,
                                )
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex + 1,
                                    ListEvent.UPDATE,
                                    false,
                                    changeType,
                                    oldValue,
                                    firstFromNextGroup,
                                    false,
                                    false,
                                )
                            }
                        }

                        LEFT_GROUP -> when (oldGroup) {
                            NO_GROUP -> {
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex,
                                    ListEvent.UPDATE,
                                    true,
                                    changeType,
                                    tryJoinResult.oldFirstInGroup,
                                    tryJoinResult.newFirstInGroup,
                                    false,
                                    false,
                                )
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex + 1,
                                    ListEvent.DELETE,
                                    false,
                                    changeType,
                                    oldValue,
                                    ListEvent.unknownValue(),
                                    false,
                                    false,
                                )
                            }

                            LEFT_GROUP -> internalClient.groupChanged(
                                changeIndex,
                                groupIndex,
                                ListEvent.UPDATE,
                                true,
                                changeType,
                                tryJoinResult.oldFirstInGroup,
                                tryJoinResult.newFirstInGroup,
                                false,
                                false,
                            )

                            RIGHT_GROUP -> {
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex,
                                    ListEvent.UPDATE,
                                    true,
                                    changeType,
                                    tryJoinResult.oldFirstInGroup,
                                    tryJoinResult.newFirstInGroup,
                                    true,
                                    false,
                                )
                                if (groupIndex + 1 < internalBarcode.blackSize()) {
                                    val firstFromNextGroup = sortedList[internalBarcode.getIndex(groupIndex + 1, UNIQUE)]
                                    internalClient.groupChanged(
                                        changeIndex,
                                        groupIndex + 1,
                                        ListEvent.UPDATE,
                                        false,
                                        changeType,
                                        oldValue,
                                        firstFromNextGroup,
                                        false,
                                        false,
                                    )
                                }
                            }
                        }

                        RIGHT_GROUP -> when (oldGroup) {
                            NO_GROUP -> {
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex,
                                    ListEvent.DELETE,
                                    false,
                                    changeType,
                                    oldValue,
                                    ListEvent.unknownValue(),
                                    false,
                                    true,
                                )
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex,
                                    ListEvent.UPDATE,
                                    true,
                                    changeType,
                                    tryJoinResult.oldFirstInGroup,
                                    tryJoinResult.newFirstInGroup,
                                    false,
                                    true,
                                )
                            }

                            LEFT_GROUP -> {
                                if (groupIndex - 1 >= 0) {
                                    val firstFromPreviousGroup = sortedList[internalBarcode.getIndex(groupIndex - 1, UNIQUE)]
                                    internalClient.groupChanged(
                                        changeIndex,
                                        groupIndex - 1,
                                        ListEvent.UPDATE,
                                        false,
                                        changeType,
                                        firstFromPreviousGroup,
                                        firstFromPreviousGroup,
                                        true,
                                        true,
                                    )
                                }
                                internalClient.groupChanged(
                                    changeIndex,
                                    groupIndex,
                                    ListEvent.UPDATE,
                                    true,
                                    changeType,
                                    tryJoinResult.oldFirstInGroup,
                                    tryJoinResult.newFirstInGroup,
                                    false,
                                    true,
                                )
                            }

                            RIGHT_GROUP -> internalClient.groupChanged(
                                changeIndex,
                                groupIndex,
                                ListEvent.UPDATE,
                                true,
                                changeType,
                                tryJoinResult.oldFirstInGroup,
                                tryJoinResult.newFirstInGroup,
                                false,
                                true,
                            )
                        }
                    }
                }

                ListEvent.DELETE -> {
                    val deleted = removedValues.removeFirst()
                    val sourceDeletedIndex = if (deleted === DUPLICATE) changeIndex - 1 else changeIndex
                    val groupDeletedIndex =
                        if (sourceDeletedIndex < internalBarcode.size()) {
                            internalBarcode.getBlackIndex(sourceDeletedIndex, true)
                        } else {
                            internalBarcode.blackSize()
                        }

                    if (deleted === UNIQUE) {
                        if (changeIndex >= internalBarcode.size() && changeIndex == lastFakedUniqueChangeIndex) {
                            internalClient.groupChanged(
                                changeIndex,
                                groupDeletedIndex - 1,
                                ListEvent.UPDATE,
                                true,
                                changeType,
                                oldValue,
                                ListEvent.unknownValue(),
                                false,
                                true,
                            )
                            lastFakedUniqueChangeIndex = -1
                        } else {
                            internalClient.groupChanged(
                                changeIndex,
                                groupDeletedIndex,
                                ListEvent.DELETE,
                                true,
                                changeType,
                                oldValue,
                                ListEvent.unknownValue(),
                                false,
                                true,
                            )
                        }
                    } else {
                        val newValueInGroup =
                            if (groupDeletedIndex < internalBarcode.blackSize()) {
                                sortedList[internalBarcode.getIndex(groupDeletedIndex, UNIQUE)]
                            } else {
                                ListEvent.unknownValue()
                            }
                        val oldValueInGroup =
                            if (deleted === UNIQUE_WITH_DUPLICATE) oldValue else newValueInGroup

                        internalClient.groupChanged(
                            changeIndex,
                            groupDeletedIndex,
                            ListEvent.UPDATE,
                            true,
                            changeType,
                            oldValueInGroup,
                            newValueInGroup,
                            false,
                            true,
                        )
                    }
                }
            }
        }
    }

    private fun uniqueElementAddedToLeftInSameGroup(toDoList: Barcode, changeIndex: Int): Boolean =
        internalBarcode[changeIndex - 1] === UNIQUE &&
                toDoList[changeIndex - 1] === TODO &&
                groupTogether(changeIndex - 1, changeIndex)

    private fun groupTogether(sourceIndex0: Int, sourceIndex1: Int): Boolean {
        return sourceIndex0 in sortedList.indices &&
            sourceIndex1 in sortedList.indices &&
            internalComparator.compare(sortedList[sourceIndex0], sortedList[sourceIndex1]) == 0
    }

    private fun tryJoinExistingGroup(
        changeIndex: Int,
        toDoList: Barcode,
        result: TryJoinResult<E>,
    ): TryJoinResult<E> {
        val predecessorIndex = changeIndex - 1
        if (groupTogether(predecessorIndex, changeIndex)) {
            internalBarcode.set(changeIndex, DUPLICATE, 1)
            val groupIndex = internalBarcode.getColourIndex(changeIndex, true, UNIQUE)
            val indexOfFirstInGroup = internalBarcode.getIndex(groupIndex, UNIQUE)
            val firstElementInGroup = sortedList[indexOfFirstInGroup]
            return result.set(LEFT_GROUP, groupIndex, firstElementInGroup, firstElementInGroup)
        }

        var successorIndex = changeIndex + 1
        while (true) {
            if (groupTogether(changeIndex, successorIndex)) {
                if (toDoList[successorIndex] === DONE && internalBarcode[successorIndex] === UNIQUE) {
                    internalBarcode.set(changeIndex, UNIQUE, 1)
                    internalBarcode.set(successorIndex, DUPLICATE, 1)
                    val groupIndex = internalBarcode.getColourIndex(changeIndex, UNIQUE)
                    val oldFirstElementInGroup = sortedList[successorIndex]
                    val newFirstElementInGroup = sortedList[changeIndex]
                    return result.set(RIGHT_GROUP, groupIndex, oldFirstElementInGroup, newFirstElementInGroup)
                }
                successorIndex++
            } else {
                internalBarcode.set(changeIndex, UNIQUE, 1)
                val groupIndex = internalBarcode.getColourIndex(changeIndex, UNIQUE)
                val onlyElementInGroup = sortedList[changeIndex]
                return result.set(NO_GROUP, groupIndex, ListEvent.unknownValue(), onlyElementInGroup)
            }
        }
    }

    private class TryJoinResult<E> {
        var group = NO_GROUP
            private set
        var groupIndex = 0
            private set
        var oldFirstInGroup: E = ListEvent.unknownValue()
            private set
        var newFirstInGroup: E = ListEvent.unknownValue()
            private set

        fun set(
            group: Int,
            groupIndex: Int,
            oldFirstInGroup: E,
            newFirstInGroup: E,
        ): TryJoinResult<E> {
            this.group = group
            this.groupIndex = groupIndex
            this.oldFirstInGroup = oldFirstInGroup
            this.newFirstInGroup = newFirstInGroup
            return this
        }
    }

    @FunctionalInterface
    fun interface Client<E> {
        fun groupChanged(
            index: Int,
            groupIndex: Int,
            groupChangeType: Int,
            primary: Boolean,
            elementChangeType: Int,
            oldValue: E,
            newValue: E,
            updateNextSeparator: Boolean,
            joinRight: Boolean,
        )
    }

    companion object {
        @JvmField
        val UNIQUE: Any = Barcode.BLACK

        @JvmField
        val DUPLICATE: Any = Barcode.WHITE
    }
}

@Suppress("UNCHECKED_CAST")
private fun <T> platformValue(value: T?): T = value as T
