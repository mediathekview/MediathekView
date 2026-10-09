/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.TextFilterator
import ca.odell.glazedlists.impl.filter.*
import ca.odell.glazedlists.impl.text.LatinDiacriticsStripper

/**
 * Creates matchers that search text extracted from elements without coupling the filtering logic
 * to a UI component.
 */
open class TextMatcherEditor<E>(
    filterator: TextFilterator<in E>? = null,
) : AbstractMatcherEditor<E>() {
    /** The extractor used to obtain searchable strings from matched elements. */
    open var filterator: TextFilterator<in E>? = filterator
        set(value) {
            if (value === field) return
            field = value

            val currentMatcher = currentTextMatcher ?: return
            fireChanged(currentMatcher.newFilterator(value))
        }

    /** The location within searchable strings where filter text must match. */
    open var mode: Int = CONTAINS
        set(value) {
            require(value == CONTAINS || value == STARTS_WITH || value == REGULAR_EXPRESSION || value == EXACT) {
                "mode must be one of: TextMatcherEditor.CONTAINS, STARTS_WITH, REGULAR_EXPRESSION or EXACT"
            }
            if (value == field) return

            val oldMode = field
            field = value
            val currentMatcher = currentTextMatcher ?: return
            val newMatcher = currentMatcher.newMode(value)

            when (oldMode) {
                CONTAINS ->
                    if (value == STARTS_WITH) fireConstrained(newMatcher) else fireChanged(newMatcher)

                STARTS_WITH ->
                    if (value == CONTAINS) fireRelaxed(newMatcher) else fireChanged(newMatcher)

                else -> fireChanged(newMatcher)
            }
        }

    private var strategyFactory = IDENTICAL_STRATEGY as TextSearchStrategy.Factory

    /** The character-comparison strategy used by newly created text matchers. */
    open var strategy: Any?
        get() = strategyFactory
        set(value) {
            if (value === strategyFactory) return
            require(value is TextSearchStrategy.Factory)

            strategyFactory = value
            val currentMatcher = currentTextMatcher ?: return
            fireChanged(currentMatcher.newStrategy(value))
        }

    /** The current matcher when it is text-based, otherwise `null`. */
    @Suppress("UNCHECKED_CAST")
    protected open val currentTextMatcher: TextMatcher<E>?
        get() = matcher as? TextMatcher<E>

    /** Replaces the current filter strings and emits the most specific safe matcher event. */
    open fun setFilterText(newFilters: Array<String>) {
        val searchTerms = Array(newFilters.size) { index -> SearchTerm<E>(newFilters[index]) }
        setTextMatcher(TextMatcher(searchTerms, filterator, mode, strategy))
    }

    /** Applies [newMatcher], classifying its relationship to the current matcher when possible. */
    protected open fun setTextMatcher(newMatcher: TextMatcher<E>) {
        val oldMatcher = currentTextMatcher
        if (newMatcher == oldMatcher) return

        if (newMatcher.searchTerms.isEmpty()) {
            if (!isCurrentlyMatchingAll) fireMatchAll()
            return
        }

        when {
            isCurrentlyMatchingAll -> fireConstrained(newMatcher)
            TextMatchers.isMatcherRelaxed(oldMatcher, newMatcher) -> fireRelaxed(newMatcher)
            TextMatchers.isMatcherConstrained(oldMatcher, newMatcher) -> fireConstrained(newMatcher)
            else -> fireChanged(newMatcher)
        }
    }

    private open class IdenticalStrategyFactory : TextSearchStrategy.Factory {
        override fun create(mode: Int, filter: String): TextSearchStrategy {
            when (mode) {
                CONTAINS -> {
                    if (filter.length == 1) return SingleCharacterCaseInsensitiveTextSearchStrategy()
                    return BoyerMooreCaseInsensitiveTextSearchStrategy()
                }

                STARTS_WITH -> return StartsWithCaseInsensitiveTextSearchStrategy()
                REGULAR_EXPRESSION -> return RegularExpressionTextSearchStrategy()
                EXACT -> return ExactCaseInsensitiveTextSearchStrategy()
                else -> throw IllegalArgumentException("unrecognized mode: $mode")
            }
        }
    }

    private class NormalizedStrategyFactory : IdenticalStrategyFactory() {
        override fun create(mode: Int, filter: String): TextSearchStrategy =
            super.create(mode, filter).apply {
                setCharacterMap(LatinDiacriticsStripper.sharedMapper())
            }
    }

    companion object {
        /** Matches filter text anywhere within an extracted string. */
        const val CONTAINS = 0

        /** Matches filter text only at the beginning of an extracted string. */
        const val STARTS_WITH = 1

        /** Interprets filter text as a regular expression. */
        const val REGULAR_EXPRESSION = 2

        /** Requires an exact extracted-string match. */
        const val EXACT = 3

        /** Compares characters directly without normalization. */
        @JvmField
        val IDENTICAL_STRATEGY: Any = IdenticalStrategyFactory()

        /** Normalizes Latin diacritics before comparing characters. */
        @JvmField
        val NORMALIZED_STRATEGY: Any = NormalizedStrategyFactory()
    }
}
