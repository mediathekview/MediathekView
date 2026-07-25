/* Glazed Lists                                                 (c) 2003-2014 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.Filterator
import ca.odell.glazedlists.impl.matchers.RangeMatcher

/**
 * Produces matchers that accept elements with comparable values inside an
 * inclusive range. A [Filterator] can extract those values from each element;
 * without one, the elements themselves must be comparable values.
 */
class RangeMatcherEditor<D, E>(
    filterator: Filterator<D, E>? = null,
) : AbstractMatcherEditor<E>() where D : Comparable<D> {
    private val rangeFilterator = filterator
    private var currentRangeStart: D? = null
    private var currentRangeEnd: D? = null

    /** The filterator used to extract comparable values from matched elements. */
    val filterator: Filterator<D, E>?
        get() = rangeFilterator

    /**
     * Changes the inclusive range. Null bounds are unbounded, and reversed
     * non-null bounds are normalized into their natural order.
     */
    fun setRange(newStart: D?, newEnd: D?) {
        var normalizedStart = newStart
        var normalizedEnd = newEnd
        if (normalizedStart != null && normalizedEnd != null && normalizedStart > normalizedEnd) {
            val previousStart = normalizedStart
            normalizedStart = normalizedEnd
            normalizedEnd = previousStart
        }

        try {
            if (normalizedStart == null && normalizedEnd == null) {
                if (currentRangeStart != null || currentRangeEnd != null) {
                    fireMatchAll()
                }
                return
            }

            val newStartVsOldStart = compare(normalizedStart, currentRangeStart, nullsBeforeAll = true)
            var isRelaxed = newStartVsOldStart < 0
            var isConstrained = newStartVsOldStart > 0

            val newEndVsOldEnd = compare(normalizedEnd, currentRangeEnd, nullsBeforeAll = false)
            isRelaxed = isRelaxed || newEndVsOldEnd > 0
            isConstrained = isConstrained || newEndVsOldEnd < 0

            val newMatcher = RangeMatcher(normalizedStart, normalizedEnd, rangeFilterator)
            when {
                isRelaxed && isConstrained -> fireChanged(newMatcher)
                isRelaxed -> fireRelaxed(newMatcher)
                isConstrained -> fireConstrained(newMatcher)
            }
        } finally {
            currentRangeStart = normalizedStart
            currentRangeEnd = normalizedEnd
        }
    }

    private fun compare(first: D?, second: D?, nullsBeforeAll: Boolean): Int = when {
        first == null && second == null -> 0
        first == null -> if (nullsBeforeAll) -1 else 1
        second == null -> if (nullsBeforeAll) 1 else -1
        else -> first.compareTo(second)
    }
}
