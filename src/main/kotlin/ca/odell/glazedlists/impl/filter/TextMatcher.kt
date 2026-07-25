/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

import ca.odell.glazedlists.TextFilterator
import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.TextMatcherEditor

/** Matches elements against a normalized collection of search terms. */
open class TextMatcher<E>(
    searchTerms: Array<SearchTerm<E>>,
    private val filterator: TextFilterator<in E>?,
    mode: Int,
    strategy: Any?,
) : Matcher<E> {
    private val matcherMode = mode
    private val matcherStrategy: Any
    private val normalizedSearchTerms: Array<SearchTerm<E>>
    private val filterStrategies: Array<TextSearchStrategy>
    private val filterStrings = ArrayList<String>()

    init {
        if (mode == TextMatcherEditor.REGULAR_EXPRESSION && strategy === TextMatcherEditor.NORMALIZED_STRATEGY) {
            throw IllegalArgumentException(
                "TextMatcher does not support normalized character matching with Regular Expressions",
            )
        }

        matcherStrategy = strategy!!
        val strategyFactory = matcherStrategy as TextSearchStrategy.Factory
        normalizedSearchTerms = TextMatchers.normalizeSearchTerms(searchTerms, strategyFactory)
        filterStrategies = Array(normalizedSearchTerms.size) { index ->
            val term = normalizedSearchTerms[index]
            strategyFactory.create(mode, term.text).apply { setSubtext(term.text) }
        }
    }

    /** The matching mode used for this matcher. */
    open val mode: Int
        get() = matcherMode

    /** The character-comparison strategy used for this matcher. */
    open val strategy: Any
        get() = matcherStrategy

    /** The normalized search terms evaluated by this matcher. */
    open val searchTerms: Array<SearchTerm<E>>
        get() = normalizedSearchTerms

    /** The normalized search-term text values. */
    open val searchTermStrings: Array<String>
        get() = Array(normalizedSearchTerms.size) { index -> normalizedSearchTerms[index].text }

    override fun matches(item: E): Boolean =
        TextMatchers.matches(filterStrings, filterator, normalizedSearchTerms, filterStrategies, item)

    /** Returns an equivalent matcher using [mode]. */
    open fun newMode(mode: Int): TextMatcher<E> =
        TextMatcher(normalizedSearchTerms, filterator, mode, matcherStrategy)

    /** Returns an equivalent matcher using [filterator]. */
    open fun newFilterator(filterator: TextFilterator<in E>?): TextMatcher<E> =
        TextMatcher(normalizedSearchTerms, filterator, matcherMode, matcherStrategy)

    /** Returns an equivalent matcher using [strategy]. */
    open fun newStrategy(strategy: Any?): TextMatcher<E> =
        TextMatcher(normalizedSearchTerms, filterator, matcherMode, strategy)

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || javaClass != other.javaClass) return false
        other as TextMatcher<*>

        return matcherMode == other.matcherMode &&
            normalizedSearchTerms.toHashSet() == other.normalizedSearchTerms.toHashSet() &&
            matcherStrategy == other.matcherStrategy
    }

    override fun hashCode(): Int {
        var result = matcherMode
        result = 31 * result + matcherStrategy.hashCode()
        result = 31 * result + normalizedSearchTerms.toHashSet().hashCode()
        return result
    }
}
