/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

import ca.odell.glazedlists.GlazedLists
import ca.odell.glazedlists.TextFilterable
import ca.odell.glazedlists.TextFilterator
import ca.odell.glazedlists.impl.text.LatinDiacriticsStripper
import ca.odell.glazedlists.matchers.SearchEngineTextMatcherEditor
import ca.odell.glazedlists.matchers.TextMatcherEditor

/** Common parsing, normalization, matching, and comparison services for text matchers. */
internal object TextMatchers {
    private class SearchTermLengthComparator : Comparator<SearchTerm<*>> {
        override fun compare(first: SearchTerm<*>, second: SearchTerm<*>): Int =
            second.text.length - first.text.length
    }

    private val SEARCHTERM_LENGTH_COMPARATOR: Comparator<SearchTerm<*>> =
        SearchTermLengthComparator()

    /** Evaluates every prepared search strategy against text extracted from [element]. */
    fun <E> matches(
        filterStrings: MutableList<String>,
        filterator: TextFilterator<in E>?,
        searchTerms: Array<SearchTerm<E>>,
        filterStrategies: Array<TextSearchStrategy>,
        element: E,
    ): Boolean {
        var filterStringsPopulated = false

        filterLoop@ for (index in filterStrategies.indices) {
            val textSearchStrategy = filterStrategies[index]
            val searchTerm = searchTerms[index]
            val searchTermField = searchTerm.field

            val strings: List<*> = if (searchTermField != null) {
                searchTerm.fieldFilterStrings.apply {
                    clear()
                    searchTermField.textFilterator.getFilterStrings(this, element)
                }
            } else {
                if (!filterStringsPopulated) {
                    filterStrings.clear()
                    if (filterator == null) {
                        (element as TextFilterable).getFilterStrings(filterStrings)
                    } else {
                        filterator.getFilterStrings(filterStrings, element)
                    }
                    filterStringsPopulated = true
                }
                filterStrings
            }

            if (searchTerm.isNegated) {
                for (filterString in strings) {
                    if (filterString != null && textSearchStrategy.indexOf(filterString.toString()) != -1) {
                        return false
                    }
                }
            } else {
                for (filterString in strings) {
                    if (filterString != null && textSearchStrategy.indexOf(filterString.toString()) != -1) {
                        continue@filterLoop
                    }
                }
                return false
            }
        }

        return true
    }

    private fun <E> normalizeSearchTerms(
        searchTerms: List<SearchTerm<E>>,
        negated: Boolean,
    ): List<SearchTerm<E>> {
        val result = searchTerms.filterTo(ArrayList(searchTerms.size)) { it.text.isNotEmpty() }

        for (index in result.lastIndex downTo 0) {
            val candidate = result[index]
            if (candidate.isRequired) continue

            for (otherIndex in result.indices) {
                if (index == otherIndex) continue
                val other = result[otherIndex]
                val redundant = if (negated) {
                    other.text in candidate.text
                } else {
                    candidate.text in other.text
                }
                if (!redundant) continue

                result.removeAt(index)
                break
            }
        }

        val comparator = if (negated) {
            GlazedLists.reverseComparator(SEARCHTERM_LENGTH_COMPARATOR)
        } else {
            SEARCHTERM_LENGTH_COMPARATOR
        }
        result.sortWith(comparator)
        return result
    }

    /** Normalizes text, removes redundant terms, and orders terms for efficient matching. */
    fun <E> normalizeSearchTerms(
        filters: Array<SearchTerm<E>>,
        strategy: TextSearchStrategy.Factory,
    ): Array<SearchTerm<E>> {
        val normalizedFilters = if (strategy === TextMatcherEditor.NORMALIZED_STRATEGY) {
            val characterMap = LatinDiacriticsStripper.sharedMapper()
            Array(filters.size) { index ->
                val term = filters[index]
                val mappedText = CharArray(term.text.length) { characterIndex ->
                    val character = term.text[characterIndex]
                    if (character.code < characterMap.size) characterMap[character.code] else character
                }.concatToString()
                term.newSearchTerm(mappedText)
            }
        } else {
            filters
        }

        val fieldSearchTerms = ArrayList<SearchTerm<E>>()
        val negatedSearchTerms = ArrayList<SearchTerm<E>>()
        val nonNegatedSearchTerms = ArrayList<SearchTerm<E>>()
        for (term in normalizedFilters) {
            when {
                term.field != null -> fieldSearchTerms += term
                term.isNegated -> negatedSearchTerms += term
                else -> nonNegatedSearchTerms += term
            }
        }

        val allSearchTerms = ArrayList<SearchTerm<E>>(normalizedFilters.size)
        allSearchTerms += fieldSearchTerms
        allSearchTerms += normalizeSearchTerms(negatedSearchTerms, true)
        allSearchTerms += normalizeSearchTerms(nonNegatedSearchTerms, false)
        return allSearchTerms.toTypedArray()
    }

    /** Parses [text] without any named fields. */
    fun <E> parse(text: String): Array<SearchTerm<E>> = parse(text, emptySet())

    /** Parses [text], recognizing the supplied named [fields]. */
    fun <E> parse(
        text: String,
        fields: Set<SearchEngineTextMatcherEditor.Field<E>>,
    ): Array<SearchTerm<E>> {
        val searchTerms = ArrayList<SearchTerm<E>>()
        val fieldMap = fields.associateBy { it.name }

        var searchTermText = StringBuilder()
        var field: SearchEngineTextMatcherEditor.Field<E>? = null
        var negated = false
        var required = false
        var insideTerm = false
        var insideQuotedTerm = false

        for (character in text) {
            if (insideTerm) {
                val endOfTerm = character == '"' || !insideQuotedTerm && character.isWhitespace()
                if (endOfTerm) {
                    if (searchTermText.isNotEmpty()) {
                        searchTerms += SearchTerm(searchTermText.toString(), negated, required, field)
                    }

                    searchTermText = StringBuilder()
                    field = null
                    negated = false
                    required = false
                    insideTerm = false
                    insideQuotedTerm = false
                } else {
                    if (character == ':' && field == null && !insideQuotedTerm) {
                        field = fieldMap[searchTermText.toString()]
                        if (field != null) {
                            searchTermText = StringBuilder()
                            negated = false
                            required = false
                            insideTerm = false
                            insideQuotedTerm = false
                            continue
                        }
                    }
                    searchTermText.append(character)
                }
            } else {
                if (character.isWhitespace()) {
                    field = null
                    negated = false
                    required = false
                    insideTerm = false
                    insideQuotedTerm = false
                    continue
                }

                when (character) {
                    '"' -> {
                        insideTerm = true
                        insideQuotedTerm = true
                    }

                    '+' -> required = true
                    '-' -> negated = true
                    else -> {
                        searchTermText.append(character)
                        insideTerm = true
                    }
                }
            }
        }

        if (searchTermText.isNotEmpty()) {
            searchTerms += SearchTerm(searchTermText.toString(), negated, required, field)
        }
        return searchTerms.toTypedArray()
    }

    /** Returns whether [newMatcher] is guaranteed to match no more elements than [oldMatcher]. */
    fun isMatcherConstrained(oldMatcher: TextMatcher<*>?, newMatcher: TextMatcher<*>?): Boolean {
        val existingMatcher = oldMatcher!!
        if (existingMatcher == newMatcher) return false
        val replacementMatcher = newMatcher!!
        if (existingMatcher.strategy !== replacementMatcher.strategy) return false
        if (existingMatcher.mode == TextMatcherEditor.STARTS_WITH &&
            replacementMatcher.mode == TextMatcherEditor.CONTAINS
        ) {
            return false
        }
        if (existingMatcher.mode == TextMatcherEditor.REGULAR_EXPRESSION ||
            replacementMatcher.mode == TextMatcherEditor.REGULAR_EXPRESSION
        ) {
            return false
        }
        if (existingMatcher.mode == TextMatcherEditor.EXACT || replacementMatcher.mode == TextMatcherEditor.EXACT) {
            return false
        }

        oldTerms@ for (oldTerm in existingMatcher.searchTerms) {
            for (newTerm in replacementMatcher.searchTerms) {
                if (newTerm == oldTerm || newTerm.isConstrainment(oldTerm)) continue@oldTerms
            }
            return false
        }
        return true
    }

    /** Returns whether [newMatcher] is guaranteed to match no fewer elements than [oldMatcher]. */
    fun isMatcherRelaxed(oldMatcher: TextMatcher<*>?, newMatcher: TextMatcher<*>?): Boolean =
        isMatcherConstrained(newMatcher, oldMatcher)
}
