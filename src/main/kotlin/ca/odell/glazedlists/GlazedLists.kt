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

import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.event.ListEventPublisher
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.impl.ListCollectionListModel
import ca.odell.glazedlists.impl.ObservableConnector
import ca.odell.glazedlists.impl.WeakReferenceProxy
import ca.odell.glazedlists.impl.beans.*
import ca.odell.glazedlists.impl.filter.StringTextFilterator
import ca.odell.glazedlists.impl.functions.ConstantFunction
import ca.odell.glazedlists.impl.sort.*
import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.MatcherEditor
import ca.odell.glazedlists.matchers.Matchers
import java.beans.PropertyChangeEvent
import java.util.concurrent.locks.ReadWriteLock

/** A factory for creating objects used with Glazed Lists. */
@Suppress("UNCHECKED_CAST")
object GlazedLists {
    /** Groups the reusable stateless implementations returned by this facade. */
    private object Singletons {
        val BOOLEAN_COMPARATOR: Comparator<Boolean?> = BooleanComparator()
        val COMPARABLE_COMPARATOR: Comparator<*> = ComparableComparator<Comparable<Any?>>()
        val REVERSED_COMPARABLE: Comparator<*> =
            ReverseComparator(COMPARABLE_COMPARATOR as Comparator<Any?>)
        val STRING_TEXT_FILTERATOR: TextFilterator<Any?> = StringTextFilterator()
    }

    fun <T> beanPropertyComparator(
        clazz: Class<T>,
        property: String,
        vararg properties: String,
    ): Comparator<T> {
        val firstComparator = beanPropertyComparator(clazz, property, Singletons.COMPARABLE_COMPARATOR)
        if (properties.isEmpty()) return firstComparator
        return chainComparators(
            buildList(properties.size + 1) {
                add(firstComparator)
                properties.forEach {
                    add(beanPropertyComparator(clazz, it, Singletons.COMPARABLE_COMPARATOR))
                }
            },
        )
    }

    fun <T> beanPropertyComparator(
        className: Class<T>,
        property: String,
        propertyComparator: Comparator<*>,
    ): Comparator<T> = BeanPropertyComparator(className, property, propertyComparator) as Comparator<T>

    fun booleanComparator(): Comparator<Boolean?> = Singletons.BOOLEAN_COMPARATOR

    fun caseInsensitiveComparator(): Comparator<String> = String.CASE_INSENSITIVE_ORDER

    fun <T> chainComparators(
        comparators: List<Comparator<T>>,
    ): Comparator<T> = ComparatorChain(comparators)

    fun <T> chainComparators(vararg comparators: Comparator<T>): Comparator<T> =
        ComparatorChain(comparators.toList())

    fun <T> comparableComparator(): Comparator<T> where T : Comparable<T> =
        Singletons.COMPARABLE_COMPARATOR as Comparator<T>

    fun <T> reverseComparator(): Comparator<T> where T : Comparable<T> =
        Singletons.REVERSED_COMPARABLE as Comparator<T>

    fun <T> reverseComparator(forward: Comparator<T>): Comparator<T> = ReverseComparator(forward)

    fun <T : Any> tableFormat(
        propertyNames: Array<String>,
        columnLabels: Array<String>,
    ): TableFormat<T> = BeanTableFormat(null, propertyNames, columnLabels)

    fun <T : Any> tableFormat(
        baseClass: Class<T>,
        propertyNames: Array<String>,
        columnLabels: Array<String>,
    ): TableFormat<T> = BeanTableFormat(baseClass, propertyNames, columnLabels)

    fun <T : Any> tableFormat(
        propertyNames: Array<String>,
        columnLabels: Array<String>,
        editable: BooleanArray,
    ): TableFormat<T> = BeanTableFormat(null, propertyNames, columnLabels, editable)

    fun <T : Any> tableFormat(
        baseClass: Class<T>,
        propertyNames: Array<String>,
        columnLabels: Array<String>,
        editable: BooleanArray,
    ): TableFormat<T> = BeanTableFormat(baseClass, propertyNames, columnLabels, editable)

    fun <E> textFilterator(vararg propertyNames: String): TextFilterator<E> =
        BeanTextFilterator<Any?, E>(*propertyNames)

    fun <E> textFilterator(
        beanClass: Class<E>,
        vararg propertyNames: String,
    ): TextFilterator<E> = BeanTextFilterator<Any?, E>(beanClass, *propertyNames)

    fun <D, E> filterator(vararg propertyNames: String): Filterator<D, E> =
        BeanTextFilterator(*propertyNames)

    fun <D, E> filterator(
        beanClass: Class<E>,
        vararg propertyNames: String,
    ): Filterator<D, E> = BeanTextFilterator(beanClass, *propertyNames)

    fun <E> toStringTextFilterator(): TextFilterator<E> =
        Singletons.STRING_TEXT_FILTERATOR as TextFilterator<E>

    fun <E> thresholdEvaluator(propertyName: String): ThresholdList.Evaluator<E?> =
        BeanThresholdEvaluator<Any>(propertyName) as ThresholdList.Evaluator<E?>

    fun <E> listCollectionListModel(): CollectionList.Model<List<E>, E> =
        ListCollectionListModel()

    fun <E> eventListOf(vararg contents: E): EventList<E> = eventList(contents.asList())

    fun <E> eventList(contents: Collection<E>): EventList<E> {
        val result = BasicEventList<E>(contents.size)
        result.addAll(contents)
        return result
    }

    fun <E> eventListOf(
        publisher: ListEventPublisher?,
        lock: ReadWriteLock?,
        vararg contents: E,
    ): EventList<E> = eventList(publisher, lock, contents.asList())

    fun <E> eventList(
        publisher: ListEventPublisher?,
        lock: ReadWriteLock?,
        contents: Collection<E>,
    ): EventList<E> {
        val result = BasicEventList<E>(contents.size, publisher, lock)
        result.addAll(contents)
        return result
    }

    fun <E> weakReferenceProxy(
        source: EventList<E>,
        target: ListEventListener<E>,
    ): ListEventListener<E> = WeakReferenceProxy(source, target)

    fun <E> beanConnector(beanClass: Class<E>): ObservableElementList.Connector<E> =
        BeanConnector(beanClass)

    fun <E> beanConnector(
        beanClass: Class<E>,
        matchPropertyNames: Boolean,
        vararg propertyNames: String,
    ): ObservableElementList.Connector<E> =
        beanConnector(beanClass, Matchers.propertyEventNameMatcher(matchPropertyNames, *propertyNames))

    fun <E> beanConnector(
        beanClass: Class<E>,
        eventMatcher: Matcher<PropertyChangeEvent>,
    ): ObservableElementList.Connector<E> = BeanConnector(beanClass, eventMatcher)

    fun <E> beanConnector(
        beanClass: Class<E>,
        addListener: String,
        removeListener: String,
    ): ObservableElementList.Connector<E> = BeanConnector(beanClass, addListener, removeListener)

    fun <E> beanConnector(
        beanClass: Class<E>,
        addListener: String,
        removeListener: String,
        eventMatcher: Matcher<PropertyChangeEvent>,
    ): ObservableElementList.Connector<E> =
        BeanConnector(beanClass, addListener, removeListener, eventMatcher)

    fun <E> observableConnector(): ObservableElementList.Connector<E>
            where E : ObservableConnector.PropertyChangeObservable = ObservableConnector()

    fun <E> fixedMatcherEditor(matcher: Matcher<E>): MatcherEditor<E> = MatcherEditor.fromMatcher(matcher)

    fun <E, V> constantFunction(value: V): (E) -> V = ConstantFunction(value)

    fun <E> toStringFunction(
        beanClass: Class<E>,
        propertyName: String,
    ): (E) -> String? = StringBeanFunction(beanClass, propertyName)

    fun <E, V> beanFunction(beanClass: Class<E>, propertyName: String): (E) -> V =
        BeanFunction(beanClass, propertyName)
}
