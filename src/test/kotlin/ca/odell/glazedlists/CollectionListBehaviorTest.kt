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

import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.event.SequenceDependenciesEventPublisher
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.concurrent.locks.ReentrantReadWriteLock

internal class CollectionListBehaviorTest {
    @Test
    fun initialFlatteningIncludesEmptyParentsAndMapsEveryBoundary() {
        val parents =
            BasicEventList<Parent>().apply {
                addAll(
                    listOf(
                        Parent("first", mutableListOf("a", "b")),
                        Parent("empty", mutableListOf()),
                        Parent("last", mutableListOf("c")),
                    ),
                )
            }
        val flattened = CollectionList(parents, parentModel)

        assertEquals(listOf("a", "b", "c"), flattened.toList())
        assertEquals(3, flattened.size)
        assertEquals(0, flattened.childStartingIndex(0))
        assertEquals(1, flattened.childEndingIndex(0))
        assertEquals(-1, flattened.childStartingIndex(1))
        assertEquals(-1, flattened.childEndingIndex(1))
        assertEquals(2, flattened.childStartingIndex(2))
        assertEquals(2, flattened.childEndingIndex(2))

        assertFailure<IndexOutOfBoundsException>("Invalid index: -1") { flattened.childStartingIndex(-1) }
        assertFailure<IndexOutOfBoundsException>("Invalid index: 3") { flattened.childEndingIndex(3) }
        val invalidChildIndex = -1
        assertFailure<IndexOutOfBoundsException>("Invalid index: -1") { flattened[invalidChildIndex] }
        assertFailure<IndexOutOfBoundsException>("Index: 3, Size: 3") { flattened[3] }
        assertFailure<IllegalStateException>("Non-writable List cannot be modified") { flattened.add(0, "x") }
    }

    @Test
    fun plainMutableChildrenSupportSetAndRemoveWithExactEvents() {
        val firstChildren = mutableListOf("a", "b")
        val lastChildren = mutableListOf("c")
        val parents =
            BasicEventList<Parent>().apply {
                addAll(
                    listOf(
                        Parent("first", firstChildren),
                        Parent("empty", mutableListOf()),
                        Parent("last", lastChildren),
                    ),
                )
            }
        val flattened = CollectionList(parents, parentModel)
        val events = flattened.recordEvents()

        assertEquals("b", flattened.set(1, "B"))
        assertEquals("a", flattened.removeAt(0))
        assertEquals("c", flattened.removeAt(1))

        assertEquals(listOf("B"), firstChildren)
        assertTrue(lastChildren.isEmpty())
        assertEquals(listOf("B"), flattened.toList())
        assertEquals(
            listOf(
                listOf(Change(ListEvent.UPDATE, 1, "b", UNKNOWN)),
                listOf(Change(ListEvent.DELETE, 0, "a", UNKNOWN)),
                listOf(Change(ListEvent.DELETE, 1, "c", UNKNOWN)),
            ),
            events,
        )
    }

    @Test
    fun parentInsertDeleteAndUpdatePublishExactFlattenedValues() {
        val parents =
            BasicEventList<Parent>().apply {
                addAll(
                    listOf(
                        Parent("first", mutableListOf("a", "b")),
                        Parent("last", mutableListOf("c")),
                    ),
                )
            }
        val flattened = CollectionList(parents, parentModel)
        val events = flattened.recordEvents()

        parents.add(1, Parent("middle", mutableListOf("x", "y")))
        parents.removeAt(0)
        parents[1] = Parent("replacement", mutableListOf("z"))

        assertEquals(listOf("x", "y", "z"), flattened.toList())
        assertEquals(
            listOf(
                listOf(
                    Change(ListEvent.INSERT, 2, UNKNOWN, "y"),
                    Change(ListEvent.INSERT, 3, UNKNOWN, "x"),
                ),
                listOf(
                    Change(ListEvent.DELETE, 0, "a", UNKNOWN),
                    Change(ListEvent.DELETE, 0, "b", UNKNOWN),
                ),
                listOf(
                    Change(ListEvent.DELETE, 2, "c", UNKNOWN),
                    Change(ListEvent.INSERT, 2, UNKNOWN, "z"),
                ),
            ),
            events,
        )
    }

    @Test
    fun bufferedParentBatchPreservesNormalizedEventOrderingAndValues() {
        val backing =
            BasicEventList<Parent>().apply {
                addAll(
                    listOf(
                        Parent("first", mutableListOf("a", "b")),
                        Parent("middle", mutableListOf("c")),
                        Parent("empty", mutableListOf()),
                    ),
                )
            }
        val parents = TransactionList(backing)
        val flattened = CollectionList(parents, parentModel)
        val events = flattened.recordEvents()

        parents.beginEvent()
        parents.removeAt(0)
        parents[0] = Parent("replacement", mutableListOf("d", "e"))
        parents.add(1, Parent("inserted", mutableListOf("x")))
        parents.commitEvent()

        assertEquals(listOf("d", "e", "x"), flattened.toList())
        assertEquals(1, events.size)
        assertEquals(
            listOf(
                Change(ListEvent.INSERT, 0, UNKNOWN, "e"),
                Change(ListEvent.INSERT, 1, UNKNOWN, "d"),
                Change(ListEvent.INSERT, 2, UNKNOWN, "x"),
                Change(ListEvent.DELETE, 3, "a", UNKNOWN),
                Change(ListEvent.DELETE, 3, "b", UNKNOWN),
                Change(ListEvent.DELETE, 3, "c", UNKNOWN),
            ),
            events.single(),
        )
    }

    @Test
    fun eventListChildrenForwardExactEventsRegisterRelationshipsAndDetachWithTheirParent() {
        val parents = BasicEventList<List<String>>()
        val plain = mutableListOf("p")
        val child = BasicEventList<String>(parents.publisher, parents.readWriteLock).apply { addAll(listOf("a", "b")) }
        parents.addAll(listOf(plain, child, emptyList()))
        val flattened = CollectionList(parents, listModel)
        val events = flattened.recordEvents()
        val publisher = parents.publisher as SequenceDependenciesEventPublisher
        val childListener = publisher.getListeners<ListEventListener<String>>(child).single()

        assertSame(flattened, relatedSubjects(publisher)[childListener])

        child.add(1, "x")
        child[0] = "A"
        child.removeAt(2)

        assertEquals(listOf("p", "A", "x"), flattened.toList())
        assertEquals(
            listOf(
                listOf(Change(ListEvent.INSERT, 2, UNKNOWN, "x")),
                listOf(Change(ListEvent.UPDATE, 1, "a", "A")),
                listOf(Change(ListEvent.DELETE, 3, "b", UNKNOWN)),
            ),
            events,
        )

        events.clear()
        parents.removeAt(1)
        assertEquals(listOf("p"), flattened.toList())
        assertEquals(
            listOf(
                Change(ListEvent.DELETE, 1, "A", UNKNOWN),
                Change(ListEvent.DELETE, 1, "x", UNKNOWN),
            ),
            events.single(),
        )
        assertTrue(publisher.getListeners<ListEventListener<String>>(child).isEmpty())
        assertFalse(relatedSubjects(publisher).containsKey(childListener))

        events.clear()
        child.add("detached")
        assertTrue(events.isEmpty())
        assertEquals(listOf("p"), flattened.toList())
    }

    @Test
    fun eventListChildrenMustSharePublisherAndLockBeforeListenerRegistration() {
        val source = BasicEventList<List<String>>()
        val differentPublisherChild = BasicEventList<String>(null, source.readWriteLock).apply { add("value") }
        source.add(differentPublisherChild)

        assertFailure<IllegalArgumentException>(
            "If a CollectionList.Model returns EventLists, those EventLists must use the same ListEventPublisher as the CollectionList",
        ) {
            CollectionList(source, listModel)
        }
        val differentPublisher = differentPublisherChild.publisher as SequenceDependenciesEventPublisher
        assertTrue(differentPublisher.getListeners<ListEventListener<String>>(differentPublisherChild).isEmpty())

        source.clear()
        val differentLockChild = BasicEventList<String>(source.publisher, ReentrantReadWriteLock()).apply { add("value") }
        source.add(differentLockChild)
        assertFailure<IllegalArgumentException>(
            "If a CollectionList.Model returns EventLists, those EventLists must use the same ReadWriteLock as the CollectionList",
        ) {
            CollectionList(source, listModel)
        }
        val sharedPublisher = source.publisher as SequenceDependenciesEventPublisher
        assertTrue(sharedPublisher.getListeners<ListEventListener<String>>(differentLockChild).isEmpty())
    }

    @Test
    fun disposeDetachesParentAndAllEventListChildrenAndClearsRelationships() {
        val parents = BasicEventList<List<String>>()
        val first = BasicEventList<String>(parents.publisher, parents.readWriteLock).apply { add("a") }
        val second = BasicEventList<String>(parents.publisher, parents.readWriteLock).apply { add("b") }
        parents.addAll(listOf(first, second))
        val flattened = CollectionList(parents, listModel)
        val events = flattened.recordEvents()
        val publisher = parents.publisher as SequenceDependenciesEventPublisher
        val childListeners =
            listOf(
                publisher.getListeners<ListEventListener<String>>(first).single(),
                publisher.getListeners<ListEventListener<String>>(second).single(),
            )

        flattened.dispose()

        assertTrue(publisher.getListeners<ListEventListener<List<String>>>(parents).none { it === flattened })
        assertTrue(publisher.getListeners<ListEventListener<String>>(first).isEmpty())
        assertTrue(publisher.getListeners<ListEventListener<String>>(second).isEmpty())
        assertTrue(childListeners.none(relatedSubjects(publisher)::containsKey))

        parents.add(emptyList())
        first.add("detached")
        second[0] = "detached"
        assertTrue(events.isEmpty())
        assertEquals(2, flattened.size)
    }

    @Test
    fun modelRemainsASamAndFunctionApplyDelegatesToGetChildren() {
        val model = CollectionList.Model<String, Int> { parent -> parent.indices.toList() }

        assertEquals(listOf(0, 1, 2), model.getChildren("abc"))
    }

    private fun EventList<String>.recordEvents(): MutableList<List<Change>> =
        mutableListOf<List<Change>>().also { batches ->
            addListEventListener { event ->
                val batch = mutableListOf<Change>()
                while (event.next()) {
                    batch +=
                        Change(
                            type = event.type,
                            index = event.index,
                            oldValue = normalize(event.oldValue),
                            newValue = normalize(event.newValue),
                        )
                }
                batches += batch
            }
        }

    private fun normalize(value: Any?): Any? = if (value === ListEvent.UNKNOWN_VALUE) UNKNOWN else value

    @Suppress("UNCHECKED_CAST")
    private fun relatedSubjects(publisher: SequenceDependenciesEventPublisher): Map<Any?, Any?> {
        val field = SequenceDependenciesEventPublisher::class.java.getDeclaredField("listenersToRelatedSubjects")
        field.isAccessible = true
        return field.get(publisher) as Map<Any?, Any?>
    }

    private inline fun <reified T : Throwable> assertFailure(message: String?, crossinline action: () -> Unit) {
        val failure = assertThrows(T::class.java) { action() }
        assertEquals(message, failure.message)
    }

    private data class Parent(
        val name: String,
        val children: MutableList<String>,
    )

    private data class Change(
        val type: Int,
        val index: Int,
        val oldValue: Any?,
        val newValue: Any?,
    )

    private companion object {
        private const val UNKNOWN = "<unknown>"
        private val parentModel = CollectionList.Model<Parent, String> { it.children }
        private val listModel = CollectionList.Model<List<String>, String> { it }
    }
}
