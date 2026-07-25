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
import ca.odell.glazedlists.event.ListEventListener
import org.jspecify.annotations.Nullable
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.reflect.Modifier
import java.util.*
import java.util.concurrent.locks.ReentrantReadWriteLock

internal class ObservableElementListBehaviorTest {
    @Test
    fun constructionAttachesConnectorBeforeInstallingElementsAndSourceListener() {
        val first = Element("first")
        val second = Element("second")
        val trace = mutableListOf<String>()
        val source = TracingEventList(listOf(first, null, second), trace)
        val firstListener = ListenerToken("first-listener")
        val connector = object : ObservableElementList.Connector<Element?> {
            override fun installListener(element: Element?): EventListener? {
                trace += "install:${element!!.id}"
                return if (element === first) firstListener else null
            }

            override fun uninstallListener(element: Element?, listener: EventListener) {
                trace += "uninstall:${element!!.id}:${(listener as ListenerToken).id}"
            }

            override fun setObservableElementList(list: ObservableElementChangeHandler<out Element?>?) {
                if (list == null) {
                    trace += "connector-detach"
                } else {
                    val observed = list as ObservableElementList<*>
                    trace += "connector-attach:${observed.size}:${(observed[0] as Element).id}"
                }
            }
        }

        val observed = ObservableElementList(source, connector)

        assertEquals(
            listOf(
                "connector-attach:3:first",
                "install:first",
                "install:second",
                "source-attach",
            ),
            trace,
        )

        observed.dispose()

        assertEquals(
            listOf(
                "connector-attach:3:first",
                "install:first",
                "install:second",
                "source-attach",
                "source-detach",
                "uninstall:first:first-listener",
                "connector-detach",
            ),
            trace,
        )
    }

    @Test
    fun sharedListenerRegistrySwitchesOnceAndRetainsExactPerElementListeners() {
        val first = Element("first")
        val second = Element("second")
        val distinct = Element("distinct")
        val laterShared = Element("later-shared")
        val sharedListener = ListenerToken("shared")
        val distinctListener = ListenerToken("distinct")
        val connector = RecordingConnector { element ->
            if (element === distinct) distinctListener else sharedListener
        }
        val source = BasicEventList<Element>().apply { addAll(listOf(first, second)) }
        val observed = ObservableElementList(source, connector)

        assertTrue(singleListenerModeOf(observed))

        source.add(distinct)
        assertFalse(singleListenerModeOf(observed))
        source.add(laterShared)
        source.remove(second)
        source.remove(distinct)

        assertFalse(singleListenerModeOf(observed), "the switch to the per-element registry is irreversible")
        assertEquals(2, connector.uninstallations.size)
        assertSame(second, connector.uninstallations[0].element)
        assertSame(sharedListener, connector.uninstallations[0].listener)
        assertSame(distinct, connector.uninstallations[1].element)
        assertSame(distinctListener, connector.uninstallations[1].listener)

        observed.dispose()

        assertSame(first, connector.uninstallations[2].element)
        assertSame(sharedListener, connector.uninstallations[2].listener)
        assertSame(laterShared, connector.uninstallations[3].element)
        assertSame(sharedListener, connector.uninstallations[3].listener)
    }

    @Test
    fun sourceMutationsInstallAndRemoveListenersByIdentityRatherThanEquality() {
        val original = Element("equal")
        val equalReplacement = Element("equal")
        val inserted = Element("inserted")
        val source = BasicEventList<Element>().apply { add(original) }
        val connector = RecordingConnector { element -> ListenerToken("listener-${element.identity}") }
        val observed = ObservableElementList(source, connector)
        val originalListener = connector.installations.single().listener

        source[0] = original

        assertEquals(1, connector.installations.size)
        assertTrue(connector.uninstallations.isEmpty())

        source[0] = equalReplacement
        source.add(inserted)
        source.removeAt(0)

        assertEquals(3, connector.installations.size)
        assertSame(equalReplacement, connector.installations[1].element)
        assertSame(inserted, connector.installations[2].element)
        assertEquals(2, connector.uninstallations.size)
        assertSame(original, connector.uninstallations[0].element)
        assertSame(originalListener, connector.uninstallations[0].listener)
        assertSame(equalReplacement, connector.uninstallations[1].element)
        assertSame(connector.installations[1].listener, connector.uninstallations[1].listener)

        observed.dispose()
    }

    @Test
    fun unknownOldValuesFallBackToThePrivateIdentityCopyForUpdateAndDeleteCleanup() {
        val original = Element("equal")
        val equalReplacement = Element("equal")
        val source = UnknownValueEventList(listOf(original))
        val connector = RecordingConnector { element -> ListenerToken("listener-${element.identity}") }
        val observed = ObservableElementList(source, connector)
        val events = mutableListOf<RecordedEvent>()
        observed.addListEventListener(recordingListener(events))
        val originalListener = connector.installations.single().listener

        source.replaceWithUnknownValues(0, equalReplacement)
        source.removeWithUnknownValue(0)

        assertEquals(2, connector.installations.size)
        assertSame(equalReplacement, connector.installations[1].element)
        assertEquals(2, connector.uninstallations.size)
        assertSame(original, connector.uninstallations[0].element)
        assertSame(originalListener, connector.uninstallations[0].listener)
        assertSame(equalReplacement, connector.uninstallations[1].element)
        assertSame(connector.installations[1].listener, connector.uninstallations[1].listener)

        assertEquals(listOf(ListEvent.UPDATE, ListEvent.DELETE), events.map { it.changes.single().type })
        events.flatMap(RecordedEvent::changes).forEach { change ->
            assertSame(ListEvent.UNKNOWN_VALUE, change.oldValue)
            assertSame(ListEvent.UNKNOWN_VALUE, change.newValue)
        }

        observed.dispose()
    }

    @Test
    fun outerMutationsWriteThroughAndReturnTheSourceValues() {
        val first = Element("first")
        val replacement = Element("replacement")
        val inserted = Element("inserted")
        val appended = Element("appended")
        val source = BasicEventList<Element>().apply { add(first) }
        val connector = RecordingConnector { ListenerToken("listener") }
        val observed = ObservableElementList(source, connector)

        assertSame(first, observed.set(0, replacement))
        observed.add(0, inserted)
        assertTrue(observed.add(appended))
        assertSame(replacement, observed.removeAt(1))

        assertEquals(listOf(inserted, appended), source.toList())
        assertEquals(source.toList(), observed.toList())
        assertEquals(4, connector.installations.size)
        assertEquals(2, connector.uninstallations.size)
        assertSame(first, connector.uninstallations[0].element)
        assertSame(replacement, connector.uninstallations[1].element)

        observed.dispose()
    }

    @Test
    fun elementChangedUsesIdentityAtEveryDuplicatePositionAndPublishesUnknownNewValues() {
        val duplicate = Element("same")
        val equalButDistinct = Element("same")
        val source = BasicEventList<Element>().apply { addAll(listOf(duplicate, equalButDistinct, duplicate)) }
        val connector = RecordingConnector { ListenerToken("listener") }
        val observed = ObservableElementList(source, connector)
        val events = mutableListOf<RecordedEvent>()
        observed.addListEventListener(recordingListener(events))

        observed.elementChanged(duplicate)

        assertEquals(1, events.size)
        assertEquals(listOf(0, 2), events.single().changes.map(Change::index))
        events.single().changes.forEach { change ->
            assertEquals(ListEvent.UPDATE, change.type)
            assertSame(duplicate, change.oldValue)
            assertSame(ListEvent.UNKNOWN_VALUE, change.newValue)
        }

        observed.elementChanged(Element("same"))
        assertEquals(1, events.size, "an equal object that is not in the list does not match")

        observed.elementChanged(null)
        assertEquals(1, events.size)

        observed.dispose()
    }

    @Test
    fun sourceInsertUpdateDeleteAndReorderEventsAreForwardedExactly() {
        val first = Element("first")
        val replacement = Element("replacement")
        val source = BasicEventList<Element>()
        val connector = RecordingConnector { ListenerToken("listener") }
        val observed = ObservableElementList(source, connector)
        val events = mutableListOf<RecordedEvent>()
        observed.addListEventListener(recordingListener(events))

        source.add(first)
        source[0] = replacement
        source.removeAt(0)

        assertEquals(3, events.size)
        assertChange(events[0].changes.single(), ListEvent.INSERT, ListEvent.UNKNOWN_VALUE, first)
        assertChange(events[1].changes.single(), ListEvent.UPDATE, first, replacement)
        assertChange(events[2].changes.single(), ListEvent.DELETE, replacement, ListEvent.UNKNOWN_VALUE)

        observed.dispose()

        val unsorted = BasicEventList<Element>().apply {
            addAll(listOf(Element("b"), Element("a"), Element("c")))
        }
        val sortedSource = SortedList(unsorted, null)
        val reordered = ObservableElementList(sortedSource, RecordingConnector { ListenerToken("listener") })
        val reorderEvents = mutableListOf<RecordedEvent>()
        reordered.addListEventListener(recordingListener(reorderEvents))

        sortedSource.comparator = compareBy(Element::id)

        assertEquals(listOf("a", "b", "c"), reordered.map(Element::id))
        assertEquals(listOf(1, 0, 2), reorderEvents.single().reorderMap)
        assertTrue(reorderEvents.single().changes.isEmpty())

        reordered.dispose()
        sortedSource.dispose()
    }

    @Test
    fun disposalDetachesSourceBeforeOrderedCleanupAndIsIdempotentAndSilentAfterward() {
        val first = Element("first")
        val second = Element("second")
        val late = Element("late")
        val trace = mutableListOf<String>()
        val source = TracingEventList(listOf(first, second), trace)
        lateinit var retainedHandler: ObservableElementChangeHandler<out Element>
        var observedEvents = 0
        var mutatedDuringCleanup = false
        val connector = object : ObservableElementList.Connector<Element> {
            override fun installListener(element: Element): EventListener {
                trace += "install:${element.id}"
                return ListenerToken(element.id)
            }

            override fun uninstallListener(element: Element, listener: EventListener) {
                trace += "uninstall:${element.id}"
                if (!mutatedDuringCleanup) {
                    mutatedDuringCleanup = true
                    source.add(late)
                }
            }

            override fun setObservableElementList(list: ObservableElementChangeHandler<out Element>?) {
                if (list == null) trace += "connector-detach"
                else {
                    retainedHandler = list
                    trace += "connector-attach"
                }
            }
        }
        val observed = ObservableElementList(source, connector)
        observed.addListEventListener { observedEvents++ }
        trace.clear()

        observed.dispose()
        observed.dispose()
        source.add(Element("after-dispose"))
        retainedHandler.elementChanged(first)

        assertEquals(
            listOf("source-detach", "uninstall:first", "uninstall:second", "connector-detach"),
            trace,
        )
        assertEquals(listOf(first, second, late, Element("after-dispose")), source.toList())
        assertEquals(0, observedEvents)
    }

    @Test
    fun connectorSupertypeVarianceAndNullableCallbacksRemainUsableFromKotlin() {
        val installed = mutableListOf<Number>()
        val callbacks = mutableListOf<ObservableElementChangeHandler<out Number>?>()
        val connector: ObservableElementList.Connector<Number> =
            object : ObservableElementList.Connector<Number> {
                override fun installListener(element: Number): EventListener? {
                    installed += element
                    return null
                }

                override fun uninstallListener(element: Number, listener: EventListener) = Unit

                override fun setObservableElementList(list: ObservableElementChangeHandler<out Number>?) {
                    callbacks += list
                }
            }
        val source = BasicEventList<Int>().apply { add(1) }

        val observed: ObservableElementList<Int> = ObservableElementList(source, connector)
        observed.add(2)
        observed.dispose()

        assertEquals(listOf(1, 2), installed)
        assertNotNull(callbacks.first())
        assertNull(callbacks.last())
    }

    @Test
    fun publicAndOpenSurfaceAndConnectorDescriptorsMatchTheJavaBaseline() {
        val type = ObservableElementList::class.java
        assertTrue(Modifier.isPublic(type.modifiers))
        assertFalse(Modifier.isFinal(type.modifiers))
        assertEquals(listOf("E"), type.typeParameters.map { it.name })
        assertEquals("ca.odell.glazedlists.TransformedList<E, E>", type.genericSuperclass.typeName)
        assertEquals(
            listOf("ca.odell.glazedlists.ObservableElementChangeHandler<E>"),
            type.genericInterfaces.map { it.typeName },
        )

        val constructor = type.declaredConstructors.single()
        assertTrue(Modifier.isPublic(constructor.modifiers))
        assertEquals(
            listOf(
                "ca.odell.glazedlists.EventList<E>",
                "ca.odell.glazedlists.ObservableElementList${'$'}Connector<? super E>",
            ),
            constructor.genericParameterTypes.map { it.typeName },
        )

        assertMethod(type.getDeclaredMethod("listChanged", ListEvent::class.java), Modifier.PUBLIC)
        assertMethod(type.getDeclaredMethod("dispose"), Modifier.PUBLIC)
        assertMethod(type.getDeclaredMethod("elementChanged", Any::class.java), Modifier.PUBLIC)
        assertMethod(type.getDeclaredMethod("isWritable"), Modifier.PROTECTED)

        val connector = ObservableElementList.Connector::class.java
        assertTrue(connector.isInterface)
        assertTrue(Modifier.isPublic(connector.modifiers))
        assertTrue(Modifier.isStatic(connector.modifiers))
        assertTrue(Modifier.isAbstract(connector.modifiers))
        assertEquals(listOf("E"), connector.typeParameters.map { it.name })

        val install = connector.getDeclaredMethod("installListener", Any::class.java)
        assertEquals(EventListener::class.java, install.returnType)
        assertEquals("E", install.genericParameterTypes.single().typeName)
        assertEquals("java.util.EventListener", install.genericReturnType.typeName)

        val uninstall = connector.getDeclaredMethod("uninstallListener", Any::class.java, EventListener::class.java)
        assertEquals(Void.TYPE, uninstall.returnType)
        assertEquals(listOf("E", "java.util.EventListener"), uninstall.genericParameterTypes.map { it.typeName })

        val setList = connector.getDeclaredMethod("setObservableElementList", ObservableElementChangeHandler::class.java)
        assertEquals(Void.TYPE, setList.returnType)
        assertEquals(
            "ca.odell.glazedlists.ObservableElementChangeHandler<? extends E>",
            setList.genericParameterTypes.single().typeName,
        )
        assertTrue(setList.annotatedParameterTypes.single().isAnnotationPresent(Nullable::class.java))
    }

    private fun assertMethod(method: java.lang.reflect.Method, visibility: Int) {
        assertEquals(visibility, method.modifiers and (Modifier.PUBLIC or Modifier.PROTECTED or Modifier.PRIVATE), method.toString())
        assertFalse(Modifier.isFinal(method.modifiers), method.toString())
        assertFalse(Modifier.isStatic(method.modifiers), method.toString())
    }

    private fun assertChange(change: Change, type: Int, oldValue: Any?, newValue: Any?) {
        assertEquals(type, change.type)
        assertEquals(0, change.index)
        assertSame(oldValue, change.oldValue)
        assertSame(newValue, change.newValue)
    }

    private fun singleListenerModeOf(observed: ObservableElementList<*>): Boolean {
        val field = ObservableElementList::class.java.getDeclaredField("singleListenerMode")
        field.trySetAccessible()
        return field.getBoolean(observed)
    }

    private fun <E> recordingListener(target: MutableList<RecordedEvent>): ListEventListener<E> =
        ListEventListener { event ->
            if (event.isReordering) {
                target += RecordedEvent(reorderMap = event.reorderMap.toList())
            } else {
                val changes = mutableListOf<Change>()
                while (event.next()) {
                    changes += Change(event.type, event.index, event.oldValue, event.newValue)
                }
                target += RecordedEvent(changes = changes)
            }
        }

    private data class Change(
        val type: Int,
        val index: Int,
        val oldValue: Any?,
        val newValue: Any?,
    )

    private data class RecordedEvent(
        val reorderMap: List<Int>? = null,
        val changes: List<Change> = emptyList(),
    )

    private class Element(val id: String) {
        val identity: Int = System.identityHashCode(this)

        override fun equals(other: Any?): Boolean = other is Element && other.id == id
        override fun hashCode(): Int = id.hashCode()
        override fun toString(): String = "Element($id)"
    }

    private class ListenerToken(val id: String) : EventListener

    private data class Installation(
        val element: Element,
        val listener: EventListener,
    )

    private class RecordingConnector(
        private val listenerFactory: (Element) -> EventListener?,
    ) : ObservableElementList.Connector<Element> {
        val installations = mutableListOf<Installation>()
        val uninstallations = mutableListOf<Installation>()
        var handler: ObservableElementChangeHandler<out Element>? = null
            private set

        override fun installListener(element: Element): EventListener? {
            val listener = listenerFactory(element)
            if (listener != null) installations += Installation(element, listener)
            return listener
        }

        override fun uninstallListener(element: Element, listener: EventListener) {
            uninstallations += Installation(element, listener)
        }

        override fun setObservableElementList(list: ObservableElementChangeHandler<out Element>?) {
            handler = list
        }
    }

    private class TracingEventList<E>(
        initial: Collection<E>,
        private val trace: MutableList<String>,
    ) : AbstractEventList<E>() {
        private val data = initial.toMutableList()

        init {
            readWriteLock = ReentrantReadWriteLock()
        }

        override val size: Int
            get() = data.size

        override fun get(index: Int): E = data[index]

        override fun addListEventListener(listChangeListener: ListEventListener<in E>) {
            trace += "source-attach"
            super.addListEventListener(listChangeListener)
        }

        override fun removeListEventListener(listChangeListener: ListEventListener<in E>) {
            trace += "source-detach"
            super.removeListEventListener(listChangeListener)
        }

        override fun add(index: Int, element: E) {
            updates.beginEvent()
            data.add(index, element)
            updates.elementInserted(index, element)
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
    }

    private class UnknownValueEventList<E>(initial: Collection<E>) : AbstractEventList<E>() {
        private val data = initial.toMutableList()

        init {
            readWriteLock = ReentrantReadWriteLock()
        }

        override val size: Int
            get() = data.size

        override fun get(index: Int): E = data[index]

        override fun dispose() = Unit

        fun replaceWithUnknownValues(index: Int, element: E) {
            updates.beginEvent()
            data[index] = element
            updates.elementsUpdated(index, index)
            updates.commitEvent()
        }

        fun removeWithUnknownValue(index: Int): E {
            updates.beginEvent()
            val removed = data.removeAt(index)
            updates.elementsDeleted(index, index)
            updates.commitEvent()
            return removed
        }
    }
}
