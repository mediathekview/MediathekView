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
import ca.odell.glazedlists.gui.AdvancedTableFormat
import ca.odell.glazedlists.gui.WritableTableFormat
import ca.odell.glazedlists.impl.ObservableConnector
import ca.odell.glazedlists.impl.WeakReferenceProxy
import ca.odell.glazedlists.matchers.Matcher
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeSupport
import java.util.concurrent.locks.ReentrantReadWriteLock

/** Characterizes the Glazed Lists factories and EventList extensions after their Kotlin migration. */
internal class GlazedListsBehaviorTest {
    @Test
    fun replaceAllOverloadsPreserveMinimalEventsAndUpdates() {
        val target = BasicEventList<String?>().apply { addAll(listOf(null, "A", "C")) }

        val ordinaryEvents = captureEvents(target) {
            target.replaceAll(listOf(null, "B", "C"), false)
        }

        assertEquals(listOf(null, "B", "C"), target.toList())
        assertEquals(
            listOf(
                EventStep(ListEvent.DELETE, 1, "A", ListEvent.UNKNOWN_VALUE),
                EventStep(ListEvent.INSERT, 1, ListEvent.UNKNOWN_VALUE, "B"),
            ),
            ordinaryEvents,
        )

        val original = Keyed(1, "original")
        val replacement = Keyed(1, "replacement")
        val comparator = compareBy<Keyed> { it.key }
        val preserving = BasicEventList<Keyed>().apply { add(original) }
        assertTrue(captureEvents(preserving) {
            preserving.replaceAll(listOf(replacement), false, comparator)
        }.isEmpty())
        assertSame(original, preserving.single())

        val updating = BasicEventList<Keyed>().apply { add(original) }
        assertEquals(
            listOf(EventStep(ListEvent.UPDATE, 0, original, replacement)),
            captureEvents(updating) {
                updating.replaceAll(listOf(replacement), true, comparator)
            },
        )
        assertSame(replacement, updating.single())

    }

    @Test
    fun replaceAllSortedUsesNaturalOrderingAndPreservesItsExactEventShapeAndUpdatePolicy() {
        val target = BasicEventList<Int>().apply { addAll(listOf(1, 3, 5)) }

        val events = captureEvents(target) {
            target.replaceAllSorted(listOf(1, 2, 5, 7), false, null)
        }

        assertEquals(listOf(1, 2, 5, 7), target)
        assertEquals(
            listOf(
                EventStep(ListEvent.INSERT, 1, ListEvent.UNKNOWN_VALUE, 2),
                EventStep(ListEvent.DELETE, 2, 3, ListEvent.UNKNOWN_VALUE),
                EventStep(ListEvent.INSERT, 3, ListEvent.UNKNOWN_VALUE, 7),
            ),
            events,
        )

        val original = Keyed(1, "original")
        val replacement = Keyed(1, "replacement")
        val comparator = compareBy<Keyed> { it.key }
        val preserving = BasicEventList<Keyed>().apply { add(original) }
        preserving.replaceAllSorted(listOf(replacement), false, comparator)
        assertSame(original, preserving.single())

        val updating = BasicEventList<Keyed>().apply { add(original) }
        assertEquals(
            listOf(EventStep(ListEvent.UPDATE, 0, original, replacement)),
            captureEvents(updating) {
                updating.replaceAllSorted(listOf(replacement), true, comparator)
            },
        )
        assertSame(replacement, updating.single())
    }

    @Test
    fun comparatorFactoriesPreserveSingletonsNullOrderingAndCaseInsensitiveBehavior() {
        assertSame(GlazedLists.booleanComparator(), GlazedLists.booleanComparator())
        assertSame(GlazedLists.comparableComparator<String>(), GlazedLists.comparableComparator<String>())
        assertSame(GlazedLists.reverseComparator<String>(), GlazedLists.reverseComparator<String>())
        assertSame(String.CASE_INSENSITIVE_ORDER, GlazedLists.caseInsensitiveComparator())

        val booleans = mutableListOf(true, null, false)
        booleans.sortWith(GlazedLists.booleanComparator())
        assertEquals(listOf(null, false, true), booleans)

        val natural = GlazedLists.comparableComparator<String>()
        assertTrue(natural.compare(null, "A") < 0)
        val strings = mutableListOf("b", "A")
        strings.sortWith(natural)
        assertEquals(listOf("A", "b"), strings)
        assertTrue(GlazedLists.reverseComparator<String>().compare("a", "b") > 0)
        assertEquals(0, GlazedLists.caseInsensitiveComparator().compare("Alpha", "aLPHa"))
    }

    @Test
    fun comparatorChainsReverseAndBeanPropertyOverloadsPreserveOrderingAndInputOwnership() {
        val byGroup = compareBy<FactoryBean> { it.group }
        val byName = compareBy<FactoryBean> { it.name }
        val comparatorList = mutableListOf(byGroup, byName)
        val listChain = GlazedLists.chainComparators(comparatorList)
        comparatorList.clear()
        assertTrue(listChain.compare(FactoryBean("b", 1), FactoryBean("a", 2)) < 0)

        val comparatorArray = arrayOf<Comparator<FactoryBean>>(byGroup, byName)
        val varargChain = GlazedLists.chainComparators(*comparatorArray)
        comparatorArray[0] = byName
        assertTrue(varargChain.compare(FactoryBean("b", 1), FactoryBean("a", 2)) < 0)

        val reverse = GlazedLists.reverseComparator(byName)
        assertTrue(reverse.compare(FactoryBean("a", 1), FactoryBean("b", 1)) > 0)

        val propertyChain = GlazedLists.beanPropertyComparator(FactoryBean::class.java, "group", "name")
        assertTrue(propertyChain.compare(FactoryBean("z", 1), FactoryBean("a", 2)) < 0)
        assertTrue(propertyChain.compare(FactoryBean("a", 1), FactoryBean("z", 1)) < 0)
        assertTrue(propertyChain.compare(null, FactoryBean("a", 1)) < 0)

        val descendingName = GlazedLists.beanPropertyComparator(
            FactoryBean::class.java,
            "name",
            Comparator.reverseOrder<String>(),
        )
        assertTrue(descendingName.compare(FactoryBean("a", 1), FactoryBean("b", 1)) > 0)
    }

    @Suppress("UNCHECKED_CAST")
    @Test
    fun allFourTableFormatOverloadsPreserveLazyTypedAdvancedAndEditableContracts() {
        val properties = arrayOf("count", "name", "payload")
        val labels = arrayOf("Count", "Name", "Payload")
        val bean = FactoryBean("alpha", 7, NonComparable("opaque"))

        val lazy = GlazedLists.tableFormat<FactoryBean>(properties, labels)
        assertEquals(3, lazy.getColumnCount())
        assertEquals("Name", lazy.getColumnName(1))
        assertEquals("alpha", lazy.getColumnValue(bean, 1))
        val lazyAdvanced = assertInstanceOf(AdvancedTableFormat::class.java, lazy) as AdvancedTableFormat<FactoryBean>
        assertEquals(Any::class.java, lazyAdvanced.getColumnClass(0))
        assertSame(GlazedLists.comparableComparator<Comparable<Any?>>(), lazyAdvanced.getColumnComparator(0))

        val typed = GlazedLists.tableFormat(FactoryBean::class.java, properties, labels)
        val typedAdvanced = assertInstanceOf(AdvancedTableFormat::class.java, typed) as AdvancedTableFormat<FactoryBean>
        assertEquals(Int::class.javaObjectType, typedAdvanced.getColumnClass(0))
        assertEquals(String::class.java, typedAdvanced.getColumnClass(1))
        assertEquals(NonComparable::class.java, typedAdvanced.getColumnClass(2))
        assertNotNull(typedAdvanced.getColumnComparator(0))
        assertNotNull(typedAdvanced.getColumnComparator(1))
        assertNull(typedAdvanced.getColumnComparator(2))

        val lazyEditable = GlazedLists.tableFormat<FactoryBean>(properties, labels, booleanArrayOf(false, true, false))
        val lazyWritable = assertInstanceOf(WritableTableFormat::class.java, lazyEditable) as WritableTableFormat<FactoryBean>
        assertFalse(lazyWritable.isEditable(bean, 0))
        assertTrue(lazyWritable.isEditable(bean, 1))
        assertSame(bean, lazyWritable.setColumnValue(bean, "changed", 1))
        assertEquals("changed", bean.name)

        val typedEditable = GlazedLists.tableFormat(
            FactoryBean::class.java,
            properties,
            labels,
            booleanArrayOf(true, true, false),
        )
        val typedWritable = assertInstanceOf(WritableTableFormat::class.java, typedEditable) as WritableTableFormat<FactoryBean>
        assertSame(bean, typedWritable.setColumnValue(bean, 9, 0))
        assertEquals(9, bean.count)
    }

    @Test
    fun tableFormatFactoriesRetainArrayLengthFailureTiming() {
        assertThrows(ArrayIndexOutOfBoundsException::class.java) {
            GlazedLists.tableFormat(
                FactoryBean::class.java,
                arrayOf("name", "count"),
                arrayOf("Name", "Count"),
                booleanArrayOf(true),
            )
        }
    }

    @Test
    fun textFilteratorAndFilteratorOverloadsPreserveLazyEagerNullAndSingletonBehavior() {
        val bean = FactoryBean("alpha", 7)
        val lazyText = GlazedLists.textFilterator<FactoryBean>("count", "name")
        val eagerText = GlazedLists.textFilterator(FactoryBean::class.java, "count", "name")
        val lazyValues = GlazedLists.filterator<Any, FactoryBean>("count", "name")
        val eagerValues = GlazedLists.filterator<Any, FactoryBean>(FactoryBean::class.java, "count", "name")

        val lazyStrings = mutableListOf("existing")
        val eagerStrings = mutableListOf<String>()
        val values = mutableListOf<Any>()
        lazyText.getFilterStrings(lazyStrings, bean)
        eagerText.getFilterStrings(eagerStrings, bean)
        lazyValues.getFilterValues(values, bean)
        eagerValues.getFilterValues(values, bean)
        assertEquals(listOf("existing", "7", "alpha"), lazyStrings)
        assertEquals(listOf("7", "alpha"), eagerStrings)
        assertEquals(listOf(7, "alpha", 7, "alpha"), values)

        val toStringFilterator = GlazedLists.toStringTextFilterator<Any?>()
        assertSame(toStringFilterator, GlazedLists.toStringTextFilterator<Any?>())
        val strings = mutableListOf<String>()
        toStringFilterator.getFilterStrings(strings, null)
        toStringFilterator.getFilterStrings(strings, 42)
        assertEquals(listOf("42"), strings)
    }

    @Test
    fun thresholdEvaluatorAndListCollectionModelRetainLazyBehavior() {
        val evaluator = GlazedLists.thresholdEvaluator<FactoryBean>("count")
        assertEquals(3, evaluator.evaluate(FactoryBean("three", 3)))
        assertEquals(9, evaluator.evaluate(FactoryBean("nine", 9)))
        assertThrows(NullPointerException::class.java) { evaluator.evaluate(null) }

        val model = GlazedLists.listCollectionListModel<String>()
        val parent = mutableListOf("a", "b")
        assertSame(parent, model.getChildren(parent))
    }

    @Test
    fun eventListAndEventListOfOverloadsCreateIndependentListsAndPreserveCollaboratorIdentity() {
        val array = arrayOf<String?>("a", null)
        val fromArray = GlazedLists.eventListOf(*array)
        array[0] = "changed"
        assertEquals(listOf("a", null), fromArray)

        val collection = mutableListOf<String?>("b", null)
        val fromCollection = GlazedLists.eventList(collection)
        collection[0] = "changed"
        assertEquals(listOf("b", null), fromCollection)
        assertNotSame(collection, fromCollection)

        val publisher = ListEventAssembler.createListEventPublisher()
        val lock = ReentrantReadWriteLock()
        val configuredArray = GlazedLists.eventListOf(publisher, lock, "x", "y")
        val configuredCollection = GlazedLists.eventList(publisher, lock, listOf("z"))
        assertSame(publisher, configuredArray.publisher)
        assertSame(lock, configuredArray.readWriteLock)
        assertSame(publisher, configuredCollection.publisher)
        assertSame(lock, configuredCollection.readWriteLock)
        assertEquals(listOf("x", "y"), configuredArray)
        assertEquals(listOf("z"), configuredCollection)
        assertNotSame(configuredArray, configuredCollection)
    }

    @Test
    fun readOnlyAndFunctionTransformsRemainLiveForwardEventsAndDetachOnDispose() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "bbbb")) }
        val readOnly = source.asReadOnly()
        val mapped = source.transform(String::length)
        var readOnlyEvents = 0
        var mappedEvents = 0
        readOnly.addListEventListener { readOnlyEvents++ }
        mapped.addListEventListener { mappedEvents++ }

        assertEquals(listOf("a", "bbbb"), readOnly)
        assertEquals(listOf(1, 4), mapped)
        assertThrows(UnsupportedOperationException::class.java) { readOnly.add("blocked") }
        assertThrows(IllegalStateException::class.java) { mapped.add(3) }

        source[0] = "ccc"
        assertEquals(listOf("ccc", "bbbb"), readOnly)
        assertEquals(listOf(3, 4), mapped)
        assertEquals(1, readOnlyEvents)
        assertEquals(1, mappedEvents)

        readOnly.dispose()
        mapped.dispose()
        source.add("xx")
        assertEquals(1, readOnlyEvents)
        assertEquals(1, mappedEvents)
    }

    @Test
    fun weakReferenceProxyForwardsAndSupportsDeterministicDisposalWithoutGcAssumptions() {
        val source = BasicEventList<String>()
        var changes = 0
        val target = ListEventListener<String> { event -> while (event.next()) changes++ }
        val proxy = GlazedLists.weakReferenceProxy(source, target)
        source.addListEventListener(proxy)

        assertInstanceOf(WeakReferenceProxy::class.java, proxy)
        assertSame(target, (proxy as WeakReferenceProxy<String>).referent)
        source.add("first")
        assertEquals(1, changes)

        proxy.dispose()
        source.add("second")
        assertEquals(1, changes)
    }

    @Test
    fun allFiveBeanConnectorOverloadsAttachFilterAndDetach() {
        assertBeanConnectorUpdates(GlazedLists.beanConnector(ObservableFactoryBean::class.java), setOf("accepted", "ignored"))
        assertBeanConnectorUpdates(
            GlazedLists.beanConnector(ObservableFactoryBean::class.java, true, "accepted"),
            setOf("accepted"),
        )
        assertBeanConnectorUpdates(
            GlazedLists.beanConnector(
                ObservableFactoryBean::class.java,
                Matcher { event -> event.propertyName == "accepted" },
            ),
            setOf("accepted"),
        )
        assertBeanConnectorUpdates(
            GlazedLists.beanConnector(
                ObservableFactoryBean::class.java,
                "listen",
                "unlisten",
            ),
            setOf("accepted", "ignored"),
        )
        assertBeanConnectorUpdates(
            GlazedLists.beanConnector(
                ObservableFactoryBean::class.java,
                "listen",
                "unlisten",
                Matcher { event -> event.propertyName != "ignored" },
            ),
            setOf("accepted"),
        )
    }

    @Test
    fun observableConnectorAttachesForwardsAndDetaches() {
        val bean = DirectObservableBean()
        val source = BasicEventList<DirectObservableBean>().apply { add(bean) }
        val observed = ObservableElementList(source, GlazedLists.observableConnector())
        var updates = 0
        observed.addListEventListener { event ->
            while (event.next()) if (event.type == ListEvent.UPDATE) updates++
        }

        assertEquals(1, bean.listenerCount)
        bean.fire("value")
        assertEquals(1, updates)
        observed.remove(bean)
        assertEquals(0, bean.listenerCount)
        bean.fire("value")
        assertEquals(1, updates)

        observed.dispose()
    }

    @Test
    fun fixedMatcherEditorKeepsTheExactMatcher() {
        val matcher = Matcher<String> { it.startsWith("a") }
        val editor = GlazedLists.fixedMatcherEditor(matcher)
        assertSame(matcher, editor.matcher)
        assertTrue(editor.matcher.matches("apple"))
        assertFalse(editor.matcher.matches("pear"))
    }

    @Test
    fun functionFactoriesPreserveConstantsBeanValuesAndNullableResults() {
        val nullConstant = GlazedLists.constantFunction<Any, String?>(null)
        assertNull(nullConstant("ignored"))
        val value = Any()
        assertSame(value, GlazedLists.constantFunction<String, Any>(value)("ignored"))

        val bean = FactoryBean(null, 5)
        val beanFunction = GlazedLists.beanFunction<FactoryBean, String?>(FactoryBean::class.java, "name")
        val stringFunction = GlazedLists.toStringFunction(FactoryBean::class.java, "count")
        val nullableStringFunction = GlazedLists.toStringFunction(FactoryBean::class.java, "name")
        assertNull(beanFunction(bean))
        assertEquals("5", stringFunction(bean))
        assertNull(nullableStringFunction(bean))
        assertThrows(IllegalArgumentException::class.java) {
            GlazedLists.beanFunction<FactoryBean, Any>(FactoryBean::class.java, "missing")
        }
        assertThrows(IllegalArgumentException::class.java) {
            GlazedLists.toStringFunction(FactoryBean::class.java, "missing")
        }
    }

    @Test
    fun synchronizeToInitializesTracksDriftAndStopsAfterDispose() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "b")) }
        val target = arrayListOf("stale")
        val listener = source.synchronizeTo(target)
        assertEquals(source, target)

        source.add(1, "x")
        source[0] = "A"
        source.removeAt(2)
        assertEquals(listOf("A", "x"), target)

        target.add("drift")
        val failure = assertThrows(IllegalStateException::class.java) { source.add("detected") }
        assertEquals("Synchronize EventList target has been modified", failure.message)

        listener.dispose()
        val snapshot = ArrayList(target)
        source.add("detached")
        assertEquals(snapshot, target)
        assertDoesNotThrow(listener::dispose)
    }

    @Test
    fun typeSafetyListenerReturnsInstalledListenerAndCanBeRemoved() {
        val source = BasicEventList<Any?>()
        val listener = source.enforceTypes(
            linkedSetOf<Class<*>?>(String::class.java, null),
        )
        source.add("allowed")
        source.add(null)
        val failure = assertThrows(IllegalArgumentException::class.java) { source.add(42) }
        assertTrue(failure.message!!.contains("illegal type"))

        source.removeListEventListener(listener)
        assertDoesNotThrow { source.add(43) }
    }

    @Test
    fun bothMultiMapExtensionsStayLiveWriteThroughAndDetachOnDispose() {
        val naturalSource = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1")) }
        val natural = naturalSource.synchronizeToMultiMap { it.substring(0, 1) }
        assertEquals(listOf("a1", "a2"), natural["a"])
        natural["a"]!!.add("a3")
        assertEquals(listOf("a1", "a2", "a3", "b1"), naturalSource)
        natural.remove("b")
        assertEquals(listOf("a1", "a2", "a3"), naturalSource)

        val groupedSource = BasicEventList<String>().apply { addAll(listOf("Alpha", "atom", "beta")) }
        val grouped = groupedSource.synchronizeToMultiMap(
            String.CASE_INSENSITIVE_ORDER,
        ) { it.substring(0, 1) }
        assertEquals(listOf("Alpha", "atom"), grouped["A"])
        grouped["A"] = mutableListOf("Axe")
        assertEquals(listOf("beta", "Axe"), groupedSource)

        val oldNaturalValues = natural.values
        natural.dispose()
        grouped.dispose()
        assertTrue(natural.isEmpty())
        assertTrue(grouped.isEmpty())
        naturalSource.add("c1")
        groupedSource.add("charlie")
        assertTrue(natural.isEmpty())
        assertTrue(grouped.isEmpty())
        assertEquals(listOf(listOf("a1", "a2", "a3")), oldNaturalValues.map { it.toList() })
    }

    @Test
    fun synchronizedMapStaysLiveWritesThroughViewsAndDetachesOnDispose() {
        val source = BasicEventList<String>().apply { addAll(listOf("alpha", "beta")) }
        val map = source.synchronizeToMap { it.first() }
        assertSame(source, map.values)
        source.add("charlie")
        assertEquals("charlie", map['c'])

        val entry = map.entries.first { it.key == 'a' }
        assertEquals("alpha", entry.setValue("amber"))
        assertEquals(listOf("amber", "beta", "charlie"), source)
        assertTrue(map.keys.remove('b'))
        assertEquals(listOf("amber", "charlie"), source)

        val oldKeys = map.keys
        map.dispose()
        assertTrue(map.isEmpty())
        assertTrue(oldKeys.isEmpty())
        assertNotSame(oldKeys, map.keys)
        source.add("delta")
        assertTrue(map.isEmpty())
        assertEquals(listOf("amber", "charlie", "delta"), map.values)
    }

    private fun assertBeanConnectorUpdates(
        connector: ObservableElementList.Connector<ObservableFactoryBean>,
        expectedProperties: Set<String>,
    ) {
        val bean = ObservableFactoryBean()
        val source = BasicEventList<ObservableFactoryBean>().apply { add(bean) }
        val observed = ObservableElementList(source, connector)
        val updates = mutableListOf<String>()
        observed.addListEventListener { event ->
            while (event.next()) {
                if (event.type == ListEvent.UPDATE) updates += bean.lastProperty
            }
        }

        assertEquals(1, bean.listenerCount)
        bean.fire("accepted")
        bean.fire("ignored")
        assertEquals(expectedProperties, updates.toSet())
        assertEquals(expectedProperties.size, updates.size)

        observed.remove(bean)
        assertEquals(0, bean.listenerCount)
        bean.fire("accepted")
        assertEquals(expectedProperties.size, updates.size)
        observed.dispose()
    }

    private fun <E> captureEvents(target: EventList<E>, action: () -> Unit): List<EventStep> {
        val events = mutableListOf<EventStep>()
        target.addListEventListener { event ->
            while (event.next()) {
                events += EventStep(event.type, event.index, event.oldValue, event.newValue)
            }
        }
        action()
        return events
    }

    private data class EventStep(
        val type: Int,
        val index: Int,
        val oldValue: Any?,
        val newValue: Any?,
    )

    private data class Keyed(val key: Int, val label: String)

    @Suppress("unused")
    class FactoryBean(
        var name: String?,
        var count: Int,
        var payload: NonComparable = NonComparable("default"),
    ) {
        val group: Int
            get() = count
    }

    data class NonComparable(val label: String)

    class ObservableFactoryBean {
        private val changes = PropertyChangeSupport(this)
        var lastProperty: String = ""
            private set

        val listenerCount: Int
            get() = changes.propertyChangeListeners.size

        fun fire(property: String) {
            lastProperty = property
            changes.firePropertyChange(property, 0, 1)
        }

        fun listen(listener: PropertyChangeListener) = changes.addPropertyChangeListener(listener)

        fun unlisten(listener: PropertyChangeListener) = changes.removePropertyChangeListener(listener)

        fun addPropertyChangeListener(listener: PropertyChangeListener) = changes.addPropertyChangeListener(listener)

        fun removePropertyChangeListener(listener: PropertyChangeListener) = changes.removePropertyChangeListener(listener)
    }

    class DirectObservableBean : ObservableConnector.PropertyChangeObservable {
        private val changes = PropertyChangeSupport(this)

        val listenerCount: Int
            get() = changes.propertyChangeListeners.size

        fun fire(property: String) = changes.firePropertyChange(property, 0, 1)

        override fun addPropertyChangeListener(listener: PropertyChangeListener) =
            changes.addPropertyChangeListener(listener)

        override fun removePropertyChangeListener(listener: PropertyChangeListener) =
            changes.removePropertyChangeListener(listener)
    }
}
