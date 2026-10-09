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
package ca.odell.glazedlists

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class EventProcessingQueueBehaviorTest {
    @Test
    fun equalSortKeysPreserveInsertionOrderWithinBatchedEvent() {
        val source = BasicEventList<String>()

        SortedList(source, compareBy(String::length)).use { sorted ->
            source.addAll(listOf("aa", "bb", "cc"))

            assertEquals(listOf("aa", "bb", "cc"), sorted)
        }
    }

    @Test
    fun uniqueListRemainsConsistentAfterDifferentDeletionTypesInBatchedEvent() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "b", "b", "c")) }

        TransactionList(source).use { transaction ->
            UniqueList(transaction).use { unique ->
                transaction.withTransaction {
                    transaction.removeAt(0)
                    transaction.removeAt(0)
                }

                assertEquals(listOf("b", "c"), unique)
            }
        }
    }
}
