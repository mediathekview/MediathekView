/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.Filterator
import ca.odell.glazedlists.matchers.Matcher

/** Matches when at least one extracted comparable lies within the inclusive range. */
internal open class RangeMatcher<D, E>(
    private val start: D?,
    private val end: D?,
    private val filterator: Filterator<D, E>?,
) : Matcher<E> where D : Comparable<D> {
    private val filterComparables = ArrayList<D?>()

    constructor(start: D?, end: D?) : this(start, end, null)

    @Suppress("UNCHECKED_CAST")
    override fun matches(item: E): Boolean {
        filterComparables.clear()

        if (filterator == null) {
            filterComparables += item as D?
        } else {
            filterator.getFilterValues(filterComparables as MutableList<D>, item)
        }

        return filterComparables.any { filterComparable ->
            filterComparable == null ||
                (start == null || start <= filterComparable) &&
                (end == null || end >= filterComparable)
        }
    }

    override fun toString(): String = "[RangeMatcher between $start and $end]"
}
