package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.impl.UpgradeDetectingReadWriteLock
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.*
import java.util.concurrent.locks.ReentrantReadWriteLock


internal class BasicEventListBehaviorTest {
    @Test
    fun kotlinUseDisposesEventListEvenWhenTheBlockFails() {
        val source = TrackingEventList<String>()

        assertThrows(IllegalStateException::class.java) {
            source.use { error("expected failure") }
        }

        assertTrue(source.disposed)
    }

    @Test
    fun bulkMutationsProduceSingleCoherentEvents() {
        val source = BasicEventList<String>()
        val consistency = ConsistencyListener(source)

        source.addAll(listOf("A", "B", "C"))
        source.clear()

        assertEquals(listOf(3, 3), consistency.changeCounts)
        assertEquals(2, consistency.eventCount)
        assertTrue(source.isEmpty())
    }

    @Test
    fun indexedAddAllPublishesInsertedInstancesInOrder() {
        val first = Box(1)
        val second = Box(2)
        val source = BasicEventList<Box>().apply { add(Box(0)) }
        val inserted = mutableListOf<Pair<Int, Box>>()
        source.addListEventListener { event ->
            while (event.next()) {
                if (event.type == ListEvent.INSERT) inserted += event.index to event.newValue
            }
        }

        assertTrue(source.addAll(0, listOf(first, second)))

        assertEquals(listOf(first, second, Box(0)), source)
        assertEquals(listOf(0, 1), inserted.map { it.first })
        assertSame(first, inserted[0].second)
        assertSame(second, inserted[1].second)
    }

    @Test
    fun constructorsPreserveOrCreateTheirInfrastructure() {
        val defaultList = BasicEventList<String>()
        assertInstanceOf(UpgradeDetectingReadWriteLock::class.java, defaultList.readWriteLock)

        val lock = ReentrantReadWriteLock()
        assertSame(lock, BasicEventList<String>(lock).readWriteLock)

        val publisher = defaultList.publisher
        val configured = BasicEventList<String>(4, publisher, lock)
        assertSame(publisher, configured.publisher)
        assertSame(lock, configured.readWriteLock)

        val fallbackLock = BasicEventList<String>(4, publisher, null)
        assertSame(publisher, fallbackLock.publisher)
        assertInstanceOf(UpgradeDetectingReadWriteLock::class.java, fallbackLock.readWriteLock)

        assertThrows(IllegalArgumentException::class.java) {
            BasicEventList<String>(-1)
        }
    }

    @Test
    fun emptyBulkOperationsAreNoOps() {
        val source = BasicEventList<String>()
        var events = 0
        source.addListEventListener { events++ }

        assertFalse(source.addAll(emptyList()))
        source.clear()
        assertFalse(source.removeIf { true })

        assertEquals(0, events)
    }

    @Test
    fun failedIndexedOperationsValidateBeforeEventsAndLeaveTheListReusable() {
        val failures = listOf<(BasicEventList<String>) -> Unit>(
            { it.add(-1, "bad") },
            { it.add(2, "bad") },
            { it.addAll(-1, emptyList()) },
            { it.addAll(2, emptyList()) },
            { it.removeAt(1) },
            { it[1] = "bad" },
        )

        failures.forEach { operation ->
            val source = BasicEventList<String>().apply { add("A") }
            var events = 0
            source.addListEventListener { events++ }

            assertThrows(IndexOutOfBoundsException::class.java) { operation(source) }
            source.add("B")

            assertEquals(listOf("A", "B"), source)
            assertEquals(1, events)
        }
    }

    @Test
    fun callbackFailuresAreAtomicAndLeaveTheListReusable() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B")) }
        var events = 0
        source.addListEventListener { events++ }

        val replacementFailure = assertThrows(IllegalStateException::class.java) {
            source.replaceAll { value -> if (value == "B") error("replace failed") else value.lowercase() }
        }
        assertEquals("replace failed", replacementFailure.message)
        assertEquals(listOf("A", "B"), source)

        val predicateFailure = assertThrows(IllegalStateException::class.java) {
            source.removeIf { value -> if (value == "B") error("predicate failed") else true }
        }
        assertEquals("predicate failed", predicateFailure.message)
        assertEquals(listOf("A", "B"), source)

        val failingElements = object : AbstractCollection<String>() {
            override val size: Int = 2
            override fun iterator(): MutableIterator<String> = object : MutableIterator<String> {
                private var index = 0
                override fun hasNext(): Boolean = index < size
                override fun next(): String = if (index++ == 0) "C" else error("iteration failed")
                override fun remove() = throw UnsupportedOperationException()
            }
        }
        val iterationFailure = assertThrows(IllegalStateException::class.java) {
            source.addAll(1, failingElements)
        }
        assertEquals("iteration failed", iterationFailure.message)
        assertEquals(listOf("A", "B"), source)

        source.add("C")
        source.replaceAll { it.lowercase() }
        assertEquals(listOf("a", "b", "c"), source)
        assertEquals(2, events)
    }

    @Test
    fun removeIfPublishesDeletedInstancesAndPreservesSurvivors() {
        val first = Box(1)
        val second = Box(2)
        val third = Box(3)
        val source = BasicEventList<Box>().apply { addAll(listOf(first, second, third)) }
        val removed = mutableListOf<Box>()
        source.addListEventListener { event ->
            while (event.next()) {
                if (event.type == ListEvent.DELETE) removed += event.oldValue
            }
        }

        assertTrue(source.removeIf { it.value != 2 })

        assertEquals(listOf(second), source)
        assertEquals(listOf(first, third), removed)
        assertSame(first, removed[0])
        assertSame(third, removed[1])
    }

    @Test
    fun removeIfKeepsBulkDeletionsOnTheLinearEventPath() {
        val source = BasicEventList<Int>().apply { addAll(0 until 32) }
        source.addListEventListener { event -> assertTrue(event.usesLinearRepresentation()) }

        assertTrue(source.removeIf { it % 2 == 0 })

        assertEquals((1 until 32 step 2).toList(), source)
    }

    @Test
    fun replaceAllUsesIdentityToDecideWhetherToPublishUpdates() {
        val first = Box(1)
        val second = Box(2)
        val source = BasicEventList<Box>().apply { addAll(listOf(first, second)) }
        val updates = mutableListOf<Pair<Box, Box>>()
        source.addListEventListener { event ->
            while (event.next()) {
                if (event.type == ListEvent.UPDATE) updates += event.oldValue to event.newValue
            }
        }

        source.replaceAll { it }
        assertTrue(updates.isEmpty())

        source.replaceAll { Box(it.value) }
        assertEquals(2, updates.size)
        assertSame(first, updates[0].first)
        assertNotSame(first, updates[0].second)
        assertEquals(listOf(Box(1), Box(2)), source)
    }

    @Test
    fun replaceAllKeepsBulkUpdatesOnTheLinearEventPath() {
        val source = BasicEventList<Int>().apply { addAll(0 until 32) }
        source.addListEventListener { event -> assertTrue(event.usesLinearRepresentation()) }

        source.replaceAll { it + 1 }

        assertEquals((1..32).toList(), source)
    }

    @Test
    fun traversalMethodsDelegateToTheBackingArrayList() {
        val source = BasicEventList<Int>().apply { addAll(listOf(1, 2, 3)) }
        val visited = mutableListOf<Int>()

        source.forEach(visited::add)

        assertEquals(listOf(1, 2, 3), visited)
        assertEquals(listOf(1, 2, 3), source.stream().toList())
        assertEquals(listOf(1, 2, 3), source.parallelStream().toList())
        assertTrue(source.spliterator().hasCharacteristics(Spliterator.ORDERED or Spliterator.SIZED))
    }

    @Test
    fun sortingProducesCoherentReorderEvent() {
        val source = BasicEventList<String>().apply { addAll(listOf("B", "A", "C")) }
        val sorted = SortedList(source, naturalOrder())
        val consistency = ConsistencyListener(sorted)

        sorted.comparator = reverseOrder()

        assertEquals(listOf("C", "B", "A"), sorted)
        assertEquals(listOf(true), consistency.reorderings)
    }

    private class ConsistencyListener<E>(private val source: EventList<E>) : ListEventListener<E> {
        private var expected = source.toMutableList()
        val changeCounts = mutableListOf<Int>()
        val reorderings = mutableListOf<Boolean>()
        val eventCount: Int get() = changeCounts.size

        init {
            source.addListEventListener(this)
        }

        override fun listChanged(listChanges: ListEvent<E>) {
            if (listChanges.isReordering) {
                expected = listChanges.reorderMap.mapTo(ArrayList(expected.size), expected::get)
                changeCounts += listChanges.reorderMap.size
                reorderings += true
            } else {
                var changeCount = 0
                var previousIndex = -1
                var previousType = ListEvent.DELETE
                while (listChanges.next()) {
                    val index = listChanges.index
                    val type = listChanges.type
                    assertTrue(index > previousIndex || index == previousIndex && previousType == ListEvent.DELETE)
                    when (type) {
                        ListEvent.INSERT -> expected.add(index, source[index])
                        ListEvent.DELETE -> assertSame(expected.removeAt(index), listChanges.oldValue)
                        ListEvent.UPDATE -> assertSame(expected.set(index, source[index]), listChanges.oldValue)
                    }
                    previousIndex = index
                    previousType = type
                    changeCount++
                }
                changeCounts += changeCount
                reorderings += false
            }
            assertEquals(expected, source)
        }
    }

    private class TrackingEventList<E> : AbstractEventList<E>() {
        var disposed = false
            private set

        override val size: Int = 0

        override fun get(index: Int): E = throw IndexOutOfBoundsException(index)

        override fun dispose() {
            disposed = true
        }
    }

    private fun ListEvent<*>.usesLinearRepresentation(): Boolean {
        val linearIterator = javaClass.getDeclaredField("linearIterator")
        linearIterator.isAccessible = true
        return linearIterator.get(this) != null
    }

    private data class Box(val value: Int)
}
