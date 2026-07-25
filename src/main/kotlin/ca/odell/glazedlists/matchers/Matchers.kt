/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.Filterator
import ca.odell.glazedlists.impl.matchers.*
import java.beans.PropertyChangeEvent
import java.lang.reflect.Array as ReflectArray

/** A factory for creating [Matcher] instances. */
object Matchers {
    /**
     * Provides a proxy whose listeners and registration with [matcherEditor] are weakly
     * referenced.
    */
    fun <E> weakReferenceProxy(matcherEditor: MatcherEditor<E>): MatcherEditor<E> =
        WeakReferenceMatcherEditor(matcherEditor)

    /** Returns a matcher that matches every value. */
    fun <E> trueMatcher(): Matcher<E> = TrueMatcher.getInstance()

    /** Returns a matcher that matches no values. */
    fun <E> falseMatcher(): Matcher<E> = FalseMatcher.getInstance()

    /** Returns a matcher with the opposite result of [original]. */
    fun <E> invert(original: Matcher<E>): Matcher<E> = NotMatcher(original)

    /** Returns a matcher that matches only `null`. */
    fun <E> isNull(): Matcher<E> = NullMatcher.getInstance()

    /** Returns a matcher that matches every non-null value. */
    fun <E> isNotNull(): Matcher<E> = NotNullMatcher.getInstance()

    /** Returns a matcher that matches non-null, non-empty strings. */
    fun nonNullAndNonEmptyString(): Matcher<String?> =
        NonNullAndNonEmptyStringMatcher.getInstance()

    /** Matches beans whose named property equals [expectedValue]. */
    @Suppress("UNCHECKED_CAST")
    fun <E> beanPropertyMatcher(
        beanClass: Class<E>,
        propertyName: String,
        expectedValue: Any?,
    ): Matcher<E> = BeanPropertyMatcher(beanClass, propertyName, expectedValue) as Matcher<E>

    /** Matches comparable values within the inclusive range from [start] to [end]. */
    fun <D, E> rangeMatcher(start: D?, end: D?): Matcher<E> where D : Comparable<D> =
        RangeMatcher(start, end)

    /**
     * Matches when [filterator] extracts at least one comparable value within the inclusive
     * range from [start] to [end].
     */
    fun <D, E> rangeMatcher(
        start: D?,
        end: D?,
        filterator: Filterator<D, E>,
    ): Matcher<E> where D : Comparable<D> = RangeMatcher(start, end, filterator)

    /** Matches or excludes property-change events by their property names. */
    fun propertyEventNameMatcher(
        matchPropertyNames: Boolean,
        vararg propertyNames: String,
    ): Matcher<PropertyChangeEvent> = PropertyEventNameMatcher(matchPropertyNames, *propertyNames)

    /** Counts the elements in [collection] accepted by [matcher]. */
    fun <E> count(
        collection: Collection<E>,
        matcher: Matcher<in E>,
    ): Int {
        var count = 0
        for (element in collection) {
            if (matcher.matches(element)) count++
        }
        return count
    }

    /** Removes every element from [collection] that is rejected by [matcher]. */
    fun <E> filter(collection: MutableCollection<E>, matcher: Matcher<in E>): Boolean {
        var changed = false
        val iterator = collection.iterator()
        while (iterator.hasNext()) {
            if (!matcher.matches(iterator.next())) {
                iterator.remove()
                changed = true
            }
        }
        return changed
    }

    /** Returns a new array containing the elements in [items] accepted by [matcher]. */
    fun <E> select(items: Array<E>, matcher: Matcher<in E>): Array<E> {
        val selections = ArrayList<E>()
        select(items.asList(), matcher, selections)
        @Suppress("UNCHECKED_CAST")
        val result = ReflectArray.newInstance(items.javaClass.componentType, selections.size) as Array<E>
        for (index in selections.indices) result[index] = selections[index]
        return result
    }

    /** Returns a new collection containing the elements accepted by [matcher]. */
    fun <E> select(
        collection: Collection<E>,
        matcher: Matcher<in E>,
    ): MutableCollection<in E> = select(collection, matcher, ArrayList())

    /** Adds the elements accepted by [matcher] to [results] and returns [results]. */
    fun <E> select(
        collection: Collection<E>,
        matcher: Matcher<in E>,
        results: MutableCollection<in E>,
    ): MutableCollection<in E> {
        for (element in collection) {
            if (matcher.matches(element)) results += element
        }
        return results
    }

    /** Returns whether [collection] contains an element accepted by [matcher]. */
    fun <E> contains(
        collection: Collection<E>,
        matcher: Matcher<in E>,
    ): Boolean {
        for (element in collection) {
            if (matcher.matches(element)) return true
        }
        return false
    }

    /** Returns the index of the first element accepted by [matcher], or `-1`. */
    fun <E> indexOf(
        list: List<E>,
        matcher: Matcher<in E>,
    ): Int {
        for (index in list.indices) {
            if (matcher.matches(list[index])) return index
        }
        return -1
    }

    /** Returns a matcher that accepts a value when any [matchers] accepts it. */
    fun <E> or(vararg matchers: Matcher<in E>): Matcher<E> = OrMatcher(*matchers)

    /** Returns a matcher that accepts a value when every matcher in [matchers] accepts it. */
    fun <E> and(vararg matchers: Matcher<in E>): Matcher<E> = AndMatcher(*matchers)

    /** Returns a matcher that accepts non-null instances of any of [classes]. */
    fun <E> types(vararg classes: Class<*>): Matcher<E> = TypeMatcher(*classes)
}
