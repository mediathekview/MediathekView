/* Glazed Lists                                                 (c) 2003-2014 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.GlazedLists

/**
 * Produces matchers that compare values with a threshold. A function
 * can extract the value to compare when the list element and threshold types differ.
 */
class ThresholdMatcherEditor<E, T>(
    threshold: T? = null,
    operation: MatchOperation<*, *>? = null,
    comparator: Comparator<T>? = null,
    function: ((E) -> T)? = null,
) : AbstractMatcherEditor<E>() {
    private var currentMatcher: MatchOperation<E, T>
    private var activeComparator: Comparator<T> = comparator ?: defaultComparator()
    private var activeOperation: MatchOperation<E, T> = typedOperation(operation ?: GREATER_THAN)
    private var activeThreshold: T? = threshold
    private val extractionFunction: (E) -> T = function ?: identityFunction()

    /** The threshold to compare extracted values with, or `null` for no threshold. */
    var threshold: T?
        get() = activeThreshold
        set(value) {
            activeThreshold = value
            rebuildMatcher()
        }

    /** The comparison operation used to decide whether an element matches. */
    var matchOperation: MatchOperation<*, *>?
        get() = activeOperation
        set(value) {
            require(value != null) { "Operation cannot be null" }
            activeOperation = typedOperation(value)
            rebuildMatcher()
        }

    /** The comparator used for extracted values; assigning `null` restores natural ordering. */
    var comparator: Comparator<T>?
        get() = activeComparator
        set(value) {
            activeComparator = value ?: defaultComparator()
            rebuildMatcher()
        }

    init {
        currentMatcher = activeOperation.instance(activeComparator, activeThreshold, extractionFunction)
        fireChanged(currentMatcher)
    }

    private fun rebuildMatcher() {
        val newMatcher = activeOperation.instance(activeComparator, activeThreshold, extractionFunction)
        val moreStrict = newMatcher.isMoreStrict(currentMatcher)
        val lessStrict = currentMatcher.isMoreStrict(newMatcher)

        if (!moreStrict && !lessStrict) return

        currentMatcher = newMatcher
        when {
            moreStrict && lessStrict -> fireChanged(currentMatcher)
            moreStrict -> fireConstrained(currentMatcher)
            else -> fireRelaxed(currentMatcher)
        }
    }

    @Suppress("UNCHECKED_CAST")
    private fun typedOperation(operation: MatchOperation<*, *>): MatchOperation<E, T> =
        operation as MatchOperation<E, T>

    @Suppress("UNCHECKED_CAST")
    private fun defaultComparator(): Comparator<T> =
        GlazedLists.comparableComparator<Comparable<Any?>>() as Comparator<T>

    @Suppress("UNCHECKED_CAST")
    private fun identityFunction(): (E) -> T = { value -> value as T }

    /** An immutable comparison operation and the matcher created from it. */
    @ConsistentCopyVisibility
    data class MatchOperation<E, T> internal constructor(
        private val comparator: Comparator<T>?,
        private val threshold: T?,
        private val polarity: Int,
        private val inclusive: Boolean,
        private val function: (E) -> T,
    ) : Matcher<E> {
        internal constructor(polarity: Int, inclusive: Boolean) : this(
            comparator = null,
            threshold = null,
            polarity = polarity,
            inclusive = inclusive,
            function = identityFunction(),
        )

        internal fun instance(
            comparator: Comparator<T>,
            threshold: T?,
            function: (E) -> T,
        ): MatchOperation<E, T> = MatchOperation(comparator, threshold, polarity, inclusive, function)

        internal fun isMoreStrict(other: MatchOperation<E, T>): Boolean {
            if (other.polarity != polarity || other.comparator !== comparator) return true
            if (other.threshold === threshold) {
                return if (polarity == 0) {
                    other.inclusive != inclusive
                } else {
                    other.inclusive && !inclusive
                }
            }
            return polarity == 0 || !matchesThreshold(other.threshold)
        }

        override fun matches(item: E): Boolean = matchesThreshold(function(item))

        fun matchesThreshold(value: T?): Boolean {
            val compareResult = compare(value, threshold)
            if (compareResult == 0) return inclusive
            if (polarity == 0) return !inclusive
            return (compareResult < 0) == (polarity < 0)
        }

        @Suppress("UNCHECKED_CAST")
        private fun compare(first: T?, second: T?): Int =
            requireNotNull(comparator).compare(first as T, second as T)

        companion object {
            @Suppress("UNCHECKED_CAST")
            private fun <E, T> identityFunction(): (E) -> T = { value -> value as T }
        }
    }

    companion object {
        @JvmField
        val GREATER_THAN: MatchOperation<Any?, Any?> = MatchOperation(1, false)

        @JvmField
        val GREATER_THAN_OR_EQUAL: MatchOperation<Any?, Any?> = MatchOperation(1, true)

        @JvmField
        val LESS_THAN: MatchOperation<Any?, Any?> = MatchOperation(-1, false)

        @JvmField
        val LESS_THAN_OR_EQUAL: MatchOperation<Any?, Any?> = MatchOperation(-1, true)

        @JvmField
        val EQUAL: MatchOperation<Any?, Any?> = MatchOperation(0, true)

        @JvmField
        val NOT_EQUAL: MatchOperation<Any?, Any?> = MatchOperation(0, false)
    }
}
