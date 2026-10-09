package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventAssembler
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.event.ListEventPublisher
import ca.odell.glazedlists.impl.WeakReferenceProxy
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.concurrent.locks.ReadWriteLock
import java.util.concurrent.locks.ReentrantReadWriteLock

internal class AbstractEventListBehaviorTest {
    @Test
    fun constructorsPreservePublisherIdentityAndRequireSubclassesToInitializeTheLock() {
        val defaultList = ReadOnlyEventList<String>()
        val customPublisher = ListEventAssembler.createListEventPublisher()
        val customLock = ReentrantReadWriteLock()
        val configuredList = ReadOnlyEventList<String>(publisher = customPublisher, lock = customLock)

        assertNotNull(defaultList.publisher)
        assertThrows(UninitializedPropertyAccessException::class.java) {
            defaultList.readWriteLock
        }
        assertSame(defaultList, assemblerSourceOf(defaultList))
        assertSame(defaultList.publisher, assemblerPublisherOf(defaultList))
        assertTrue(listenersOf(defaultList).isEmpty())

        assertSame(customPublisher, configuredList.publisher)
        assertSame(customLock, configuredList.readWriteLock)
        assertSame(configuredList, assemblerSourceOf(configuredList))
        assertSame(customPublisher, assemblerPublisherOf(configuredList))
        assertTrue(listenersOf(configuredList).isEmpty())
    }

    @Test
    fun defaultMethodsDelegateThroughTheCoreHooks() {
        val list = RecordingEventList(listOf("A", "B", "A"))

        list.resetHookCounts()
        assertFalse(list.isEmpty())
        assertEquals(1, list.sizeCalls)
        assertEquals(0, list.getCalls)

        list.resetHookCounts()
        assertEquals(2, list.lastIndexOf("A"))
        assertEquals(1, list.sizeCalls)
        assertEquals(1, list.getCalls)

        list.resetHookCounts()
        assertTrue(list.add("tail"))
        assertEquals(4, list.sizeCalls)
        assertEquals(listOf(3 to "tail"), list.addCalls)
        assertEquals(listOf("A", "B", "A", "tail"), list.snapshot())
    }

    @Test
    fun kotlinSubclassesCanUseProtectedInfrastructureToPublishMutations() {
        val list = RecordingEventList<String>()

        val events =
            captureBatches(list) {
                list.add(0, "A")
                list.add(1, "B")
                list[0] = "AA"
                list.removeAt(1)
            }

        assertEquals(listOf("AA"), list.snapshot())
        assertEquals(
            listOf(
                EventBatch(listOf(EventStep(ListEvent.INSERT, 0, newValue = "A"))),
                EventBatch(listOf(EventStep(ListEvent.INSERT, 1, newValue = "B"))),
                EventBatch(listOf(EventStep(ListEvent.UPDATE, 0, oldValue = "A", newValue = "AA"))),
                EventBatch(listOf(EventStep(ListEvent.DELETE, 1, oldValue = "B"))),
            ),
            events,
        )
    }

    @Test
    fun listenerRegistrationPreservesIdentityAndDuplicates() {
        val source = BasicEventList<String>()
        val duplicate = ListEventListener<String> {}
        val other = ListEventListener<String> {}

        source.addListEventListener(duplicate)
        source.addListEventListener(duplicate)
        source.addListEventListener(other)

        assertEquals(listOf(duplicate, duplicate, other), listenersOf(source))

        source.removeListEventListener(duplicate)
        assertEquals(listOf(duplicate, other), listenersOf(source))

        source.removeListEventListener(ListEventListener {})
        assertEquals(listOf(duplicate, other), listenersOf(source))

        source.removeListEventListener(duplicate)
        source.removeListEventListener(other)
        assertTrue(listenersOf(source).isEmpty())
    }

    @Test
    fun listenerMutationDuringDispatchUsesTheCurrentEventSnapshot() {
        val source = BasicEventList<String>()
        var firstCalls = 0
        var secondCalls = 0
        var lateCalls = 0
        lateinit var second: ListEventListener<String>
        val late = ListEventListener<String> { lateCalls++ }
        val first =
            ListEventListener<String> {
                firstCalls++
                source.removeListEventListener(second)
                source.addListEventListener(late)
            }
        second = ListEventListener { secondCalls++ }

        source.addListEventListener(first)
        source.addListEventListener(second)

        source += "A"
        source += "B"

        assertEquals(2, firstCalls)
        assertEquals(1, secondCalls)
        assertEquals(1, lateCalls)
    }

    @Test
    fun iteratorUsesSimpleIteratorWithoutListenerRegistrationAndKeepsBoundaryMessages() {
        val source = RecordingEventList(listOf("A", "B"))
        val iterator = source.iterator()

        assertTrue(listenersOf(source).isEmpty())
        assertTrue(iterator.hasNext())
        assertEquals("A", iterator.next())
        iterator.remove()
        assertEquals(listOf("B"), source.snapshot())

        val removeFailure = assertThrows(IllegalStateException::class.java) { iterator.remove() }
        assertEquals("Cannot remove() without a prior call to next() or previous()", removeFailure.message)

        assertEquals("B", iterator.next())
        val nextFailure = assertThrows(NoSuchElementException::class.java) { iterator.next() }
        assertEquals("Cannot retrieve element 1 on a list of size 1", nextFailure.message)
    }

    @Test
    fun listIteratorRegistersAWeakProxyAndRetainsItsCursorAndIllegalStateQuirks() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B", "C")) }
        val iterator = source.listIterator(1)

        assertEquals(listOf(WeakReferenceProxy::class.java.name), listenersOf(source).map { it.javaClass.name })
        assertTrue(iterator.hasNext())
        assertTrue(iterator.hasPrevious())
        assertEquals(1, iterator.nextIndex())
        assertEquals(0, iterator.previousIndex())

        source.add(0, "X")
        assertEquals("B", iterator.next())

        iterator.add("Y")
        assertEquals(listOf("X", "A", "B", "Y", "C"), source)
        iterator.set("BB")
        assertEquals(listOf("X", "A", "BB", "Y", "C"), source)
        iterator.remove()
        assertEquals(listOf("X", "A", "Y", "C"), source)

        val removeFailure = assertThrows(IllegalStateException::class.java) { iterator.remove() }
        assertEquals("Cannot remove() without a prior call to next() or previous()", removeFailure.message)

        val setFailure = assertThrows(IllegalStateException::class.java) { iterator.set("ignored") }
        assertEquals("Cannot set() without a prior call to next() or previous()", setFailure.message)

        val start = source.listIterator(0)
        val previousFailure = assertThrows(NoSuchElementException::class.java) { start.previous() }
        assertEquals("Cannot retrieve element 0 on a list of size 4", previousFailure.message)

        val end = source.listIterator(source.size)
        val nextFailure = assertThrows(NoSuchElementException::class.java) { end.next() }
        assertEquals("Cannot retrieve element 4 on a list of size 4", nextFailure.message)
    }

    @Test
    fun subListIsALiveBackedEventListWithNestedViewsAndLocalIndices() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B", "C", "D", "E")) }
        val sub = source.subList(1, 4)
        val nested = sub.subList(1, 2)
        val subEvents = captureBatches(sub as EventList<String>) { source.add(2, "X") }

        assertInstanceOf(EventList::class.java, sub)
        assertEquals(listOf("B", "X", "C", "D"), sub)
        assertEquals(listOf("C"), nested)
        assertEquals(listOf(EventBatch(listOf(EventStep(ListEvent.INSERT, 1, newValue = "X")))), subEvents)
        assertEquals(listOf(WeakReferenceProxy::class.java.name), listenersOf(source).map { it.javaClass.name })

        sub.removeAt(2)
        nested.clear()

        assertEquals(listOf("A", "B", "X", "D", "E"), source)
        assertEquals(listOf("B", "X", "D"), sub)
        assertTrue(nested.isEmpty())
    }

    @Test
    fun subListRejectsInvalidRangesWithItsCurrentExceptionTypeAndMessage() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B", "C")) }

        val negative = assertThrows(IllegalArgumentException::class.java) { source.subList(-1, 1) }
        assertEquals("The range -1-1 is not valid over a list of size 3", negative.message)

        val reversed = assertThrows(IllegalArgumentException::class.java) { source.subList(2, 1) }
        assertEquals("The range 2-1 is not valid over a list of size 3", reversed.message)

        val oversized = assertThrows(IllegalArgumentException::class.java) { source.subList(0, 4) }
        assertEquals("The range 0-4 is not valid over a list of size 3", oversized.message)
    }

    @Test
    fun containsAndIndexBasedOperationsUseTheSearchObjectsEqualsDirectionAndFirstMatchRemoval() {
        val alpha = StoredValue("alpha")
        val beta = StoredValue("beta")
        val source = RecordingEventList<Any>(listOf(alpha, beta, alpha))

        assertTrue(source.contains(SearchValue("beta")))
        assertFalse(source.contains(ReverseSearchValue("beta")))
        assertEquals(0, source.indexOf(SearchValue("alpha")))
        assertEquals(2, source.lastIndexOf(SearchValue("alpha")))
        assertTrue(source.remove(SearchValue("alpha")))
        assertEquals(listOf(beta, alpha), source.snapshot())
        assertFalse(source.remove(SearchValue("missing")))
    }

    @Test
    fun bulkOperationsUseSingleEventsKeepTheirCurrentOrderAndDiscardUnsupportedFailures() {
        val source = RecordingEventList(listOf("A", "D"))
        val events =
            captureBatches(source) {
                assertTrue(source.addAll(1, listOf("B", "C")))
                assertTrue(source.removeAll(listOf("B", "D")))
                assertFalse(source.retainAll(listOf("A", "C")))
                source.clear()
            }

        assertTrue(source.isEmpty())
        assertEquals(
            listOf(
                EventBatch(listOf(EventStep(ListEvent.INSERT, 1, newValue = "B"), EventStep(ListEvent.INSERT, 2, newValue = "C"))),
                EventBatch(listOf(EventStep(ListEvent.DELETE, 1, oldValue = "B"), EventStep(ListEvent.DELETE, 2, oldValue = "D"))),
                EventBatch(listOf(EventStep(ListEvent.DELETE, 0, oldValue = "A"), EventStep(ListEvent.DELETE, 0, oldValue = "C"))),
            ),
            events,
        )

        val readOnlyNoOp = ReadOnlyEventList(listOf("A"))
        assertFalse(readOnlyNoOp.addAll(emptyList()))
        assertFalse(readOnlyNoOp.removeAll(emptyList()))
        assertFalse(readOnlyNoOp.retainAll(listOf("A")))

        val readOnlyAdd = ReadOnlyEventList(listOf("A"))
        val addFailure = assertThrows(UnsupportedOperationException::class.java) { readOnlyAdd.addAll(0, listOf("B")) }
        assertEquals("this list does not support add()", addFailure.message)
        assertFalse(updatesOf(readOnlyAdd).isEventInProgress())

        val readOnlyRemove = ReadOnlyEventList(listOf("A"))
        val removeFailure = assertThrows(UnsupportedOperationException::class.java) { readOnlyRemove.removeIf { true } }
        assertEquals("this list does not support remove()", removeFailure.message)
        assertTrue(updatesOf(readOnlyRemove).isEventInProgress())

        val readOnlyReplace = ReadOnlyEventList(listOf("A"))
        val replaceFailure = assertThrows(UnsupportedOperationException::class.java) { readOnlyReplace.replaceAll { "$it!" } }
        assertEquals("this list does not support set()", replaceFailure.message)
        assertFalse(updatesOf(readOnlyReplace).isEventInProgress())

        val readOnlyClear = ReadOnlyEventList(listOf("A"))
        val clearFailure = assertThrows(UnsupportedOperationException::class.java) { readOnlyClear.clear() }
        assertEquals("this list does not support remove()", clearFailure.message)
        assertTrue(updatesOf(readOnlyClear).isEventInProgress())
    }

    @Test
    fun toArrayVariantsPreserveSnapshotReuseNullTerminationAndArrayStoreTiming() {
        val source = RecordingEventList<Any?>(listOf("A", null, "C"))

        val raw = source.toArray()
        source[0] = "AA"
        assertArrayEquals(arrayOf("A", null, "C"), raw)

        val exact = arrayOfNulls<String>(3)
        val exactResult = source.toArray(exact)
        assertSame(exact, exactResult)
        assertArrayEquals(arrayOf("AA", null, "C"), exactResult)

        val oversized = arrayOfNulls<String>(5).apply {
            this[3] = "tail"
            this[4] = "sentinel"
        }
        val oversizedResult = source.toArray(oversized)
        assertSame(oversized, oversizedResult)
        assertArrayEquals(arrayOf("AA", null, "C", null, "sentinel"), oversizedResult)

        val smaller = emptyArray<String?>()
        val smallerResult = source.toArray(smaller)
        assertNotSame(smaller, smallerResult)
        assertArrayEquals(arrayOf("AA", null, "C"), smallerResult)

        val mixed = RecordingEventList<Any>(listOf("ok", 7))
        val typed = arrayOfNulls<String>(2)
        val failure = assertThrows(ArrayStoreException::class.java) { mixed.toArray(typed) }
        assertEquals("java.lang.Integer", failure.message)
        assertEquals("ok", typed[0])
        assertNull(typed[1])
    }

    @Test
    fun equalsHashCodeAndToStringRetainTheirCurrentSemanticsIncludingSelfReference() {
        val source = RecordingEventList<Any?>(listOf("A", null, "C"))
        val equalList = arrayListOf<Any?>("A", null, "C")

        assertEquals(
            true,
            AbstractEventList::class.java.getMethod("equals", Any::class.java).invoke(source, source),
        )
        assertTrue(source == equalList)
        assertTrue(equalList == source)
        assertFalse(source.equals(null))
        assertFalse(source.equals("not a list"))
        assertEquals(equalList.hashCode(), source.hashCode())
        assertEquals("[A, null, C]", source.toString())

        val selfReferential = RecordingEventList<Any>()
        selfReferential.add(selfReferential)
        assertThrows(StackOverflowError::class.java) { selfReferential.toString() }
    }

    @Test
    fun unsupportedDefaultMutationsKeepTheirExactMessagesAndTiming() {
        val readOnly = ReadOnlyEventList(listOf("A", "B"))

        val addFailure = assertThrows(UnsupportedOperationException::class.java) { readOnly.add("C") }
        assertEquals("this list does not support add()", addFailure.message)
        assertEquals(2, readOnly.sizeCalls)

        val setFailure = assertThrows(UnsupportedOperationException::class.java) { readOnly[0] = "AA" }
        assertEquals("this list does not support set()", setFailure.message)

        val removeFailure = assertThrows(UnsupportedOperationException::class.java) { readOnly.removeAt(0) }
        assertEquals("this list does not support remove()", removeFailure.message)

        assertFalse(readOnly.remove("missing"))

        val matchedRemove = assertThrows(UnsupportedOperationException::class.java) { readOnly.remove("A") }
        assertEquals("this list does not support remove()", matchedRemove.message)
    }

    private fun <E> captureBatches(source: EventList<E>, action: () -> Unit): List<EventBatch> {
        val batches = mutableListOf<EventBatch>()
        val listener =
            ListEventListener<E> { event ->
                if (event.isReordering) {
                    batches += EventBatch(reorderMap = event.reorderMap.copyOf())
                    return@ListEventListener
                }

                val steps = mutableListOf<EventStep>()
                while (event.next()) {
                    steps +=
                        when (event.type) {
                            ListEvent.INSERT -> EventStep(ListEvent.INSERT, event.index, newValue = event.newValue)
                            ListEvent.UPDATE -> EventStep(ListEvent.UPDATE, event.index, oldValue = event.oldValue, newValue = event.newValue)
                            ListEvent.DELETE -> EventStep(ListEvent.DELETE, event.index, oldValue = event.oldValue)
                            else -> error("Unexpected event type ${event.type}")
                        }
                }
                batches += EventBatch(steps)
            }
        source.addListEventListener(listener)
        try {
            action()
        } finally {
            source.removeListEventListener(listener)
        }
        return batches
    }

    private fun updatesOf(list: AbstractEventList<*>): ListEventAssembler<Any?> {
        val field = AbstractEventList::class.java.getDeclaredField("updates")
        field.isAccessible = true
        @Suppress("UNCHECKED_CAST")
        return field.get(list) as ListEventAssembler<Any?>
    }

    private fun listenersOf(list: AbstractEventList<*>): List<Any> =
        updatesOf(list).getListEventListeners().map { it as Any }

    private fun assemblerSourceOf(list: AbstractEventList<*>): Any? {
        val field = ListEventAssembler::class.java.getDeclaredField("sourceList")
        field.isAccessible = true
        return field.get(updatesOf(list))
    }

    private fun assemblerPublisherOf(list: AbstractEventList<*>): Any? {
        val field = ListEventAssembler::class.java.getDeclaredField("publisher")
        field.isAccessible = true
        return field.get(updatesOf(list))
    }

    @Suppress("SameParameterValue")
    private fun invokeListenerMethod(
        source: AbstractEventList<*>,
        name: String,
        argument: Any?,
    ) {
        val method = AbstractEventList::class.java.getMethod(name, ListEventListener::class.java)
        try {
            method.invoke(source, argument)
        } catch (failure: java.lang.reflect.InvocationTargetException) {
            throw failure.targetException
        }
    }

    private fun invokeMethod(
        source: AbstractEventList<*>,
        name: String,
        parameterTypes: Array<out Class<*>>,
        arguments: Array<out Any?>,
    ): Any? {
        val method = AbstractEventList::class.java.getMethod(name, *parameterTypes)
        return try {
            method.invoke(source, *arguments)
        } catch (failure: java.lang.reflect.InvocationTargetException) {
            throw failure.targetException
        }
    }

    @Suppress("ArrayInDataClass")
    private data class EventBatch(
        val steps: List<EventStep> = emptyList(),
        val reorderMap: IntArray? = null,
    )

    private data class EventStep(
        val type: Int,
        val index: Int,
        val oldValue: Any? = null,
        val newValue: Any? = null,
    )

    private class RecordingEventList<E>(
        initial: Collection<E> = emptyList(),
        publisher: ListEventPublisher? = null,
        lock: ReadWriteLock? = null,
    ) : AbstractEventList<E>(publisher ?: ListEventAssembler.createListEventPublisher()) {
        private val data = ArrayList(initial)

        var sizeCalls = 0
            private set
        var getCalls = 0
            private set
        val addCalls = mutableListOf<Pair<Int, E>>()

        init {
            if (lock != null) readWriteLock = lock
        }

        override val size: Int
            get() {
                sizeCalls++
                return data.size
            }

        override fun get(index: Int): E {
            getCalls++
            return data[index]
        }

        override fun add(index: Int, element: E) {
            addCalls += index to element
            updates.beginEvent()
            updates.elementInserted(index, element)
            data.add(index, element)
            updates.commitEvent()
        }

        override fun set(index: Int, element: E): E {
            updates.beginEvent()
            val old = data.set(index, element)
            updates.elementUpdated(index, old, element)
            updates.commitEvent()
            return old
        }

        override fun removeAt(index: Int): E {
            updates.beginEvent()
            val old = data.removeAt(index)
            updates.elementDeleted(index, old)
            updates.commitEvent()
            return old
        }

        override fun dispose() = Unit

        fun resetHookCounts() {
            sizeCalls = 0
            getCalls = 0
            addCalls.clear()
        }

        fun snapshot(): List<E> = ArrayList(data)
    }

    private class ReadOnlyEventList<E>(
        initial: Collection<E> = emptyList(),
        publisher: ListEventPublisher? = null,
        lock: ReadWriteLock? = null,
    ) : AbstractEventList<E>(publisher ?: ListEventAssembler.createListEventPublisher()) {
        private val data = ArrayList(initial)

        var sizeCalls = 0
            private set
        var getCalls = 0
            private set

        init {
            if (lock != null) readWriteLock = lock
        }

        override val size: Int
            get() {
                sizeCalls++
                return data.size
            }

        override fun get(index: Int): E {
            getCalls++
            return data[index]
        }

        override fun dispose() = Unit
    }

    private class StoredValue(private val id: String) {
        override fun toString(): String = "StoredValue($id)"
    }

    private class SearchValue(private val id: String) {
        override fun equals(other: Any?): Boolean = other is StoredValue && other.toString() == "StoredValue($id)"

        override fun hashCode(): Int = id.hashCode()
    }

    private class ReverseSearchValue(private val id: String) {
        override fun equals(other: Any?): Boolean = other is ReverseSearchValue && other.id == id

        override fun hashCode(): Int = id.hashCode()
    }
}
