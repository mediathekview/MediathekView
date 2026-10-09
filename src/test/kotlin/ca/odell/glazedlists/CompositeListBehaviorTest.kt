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

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.concurrent.locks.ReentrantReadWriteLock

internal class CompositeListBehaviorTest {
    @Test
    fun memberListSharesInfrastructureAndPropagatesChanges() {
        val composite = CompositeList<String>()
        val member = composite.createMemberList<String>()

        assertSame(composite.publisher, member.publisher)
        assertSame(composite.readWriteLock, member.readWriteLock)

        composite.addMemberList(member)
        member.addAll(listOf("one", "two"))
        assertEquals(listOf("one", "two"), composite.toList())

        member[1] = "second"
        assertEquals(listOf("one", "second"), composite.toList())

        composite.removeMemberList(member)
        member.add("detached")
        assertEquals(emptyList<String>(), composite.toList())
    }

    @Test
    fun addMemberListRejectsDifferentPublisherAndLock() {
        val composite = CompositeList<String>()
        val differentPublisher = BasicEventList<String>(null, composite.readWriteLock)
        val differentLock = BasicEventList<String>(composite.publisher, ReentrantReadWriteLock())

        val publisherFailure = assertThrows(IllegalArgumentException::class.java) {
            composite.addMemberList(differentPublisher)
        }
        assertEquals("Member list must share publisher with CompositeList", publisherFailure.message)

        val lockFailure = assertThrows(IllegalArgumentException::class.java) {
            composite.addMemberList(differentLock)
        }
        assertEquals("Member list must share lock with CompositeList", lockFailure.message)
    }

    @Test
    fun removeMemberListUsesIdentityRatherThanListEquality() {
        val composite = CompositeList<String>()
        val member = composite.createMemberList<String>().apply { add("value") }
        val equalButDistinct = composite.createMemberList<String>().apply { add("value") }
        composite.addMemberList(member)

        val failure = assertThrows(IllegalArgumentException::class.java) {
            composite.removeMemberList(equalButDistinct)
        }

        assertEquals("Cannot remove list [value] which is not in this CompositeList", failure.message)
        assertEquals(listOf("value"), composite.toList())
    }
}
