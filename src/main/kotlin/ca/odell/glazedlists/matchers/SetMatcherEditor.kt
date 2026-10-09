package ca.odell.glazedlists.matchers

import java.util.logging.Logger

/**
 * A [MatcherEditor] with blacklist or whitelist matching functionality.
 *
 * The [mode] determines whether the supplied match set excludes or includes
 * values extracted from list elements by [function].
 */
class SetMatcherEditor<E, O> private constructor(
    private val mode: Mode,
    private val function: (E) -> O,
) : AbstractMatcherEditor<E>() {

    init {
        check(isCurrentlyMatchingAll)
        if (mode == Mode.WHITELIST_EMPTY_MATCH_NONE) {
            fireMatchNone()
        }
    }

    /** Sets a new match set and reports how the resulting matcher changed. */
    @Suppress("UNCHECKED_CAST")
    fun setMatchSet(newSet: Set<O>) {
        val oldSet = (matcher as? SetMatcher<E, O>)?.matchSet.orEmpty()

        when {
            oldSet == newSet -> logger.fine("new set equals old -> no change to filter")

            newSet.isEmpty() -> {
                if (mode == Mode.WHITELIST_EMPTY_MATCH_NONE) {
                    logger.fine("empty set ($mode) -> firing matchNone")
                    fireMatchNone()
                } else {
                    logger.fine("empty set ($mode) -> firing matchAll")
                    fireMatchAll()
                }
            }

            oldSet.isEmpty() -> {
                logger.fine("old set was empty, new set is not -> firing change")
                fireChanged(SetMatcher(newSet, mode, function))
            }

            oldSet.containsAll(newSet) -> {
                val newMatcher = SetMatcher(newSet, mode, function)
                if (mode == Mode.BLACKLIST) {
                    logger.fine("old set contains new set (blacklist) -> firing relaxed")
                    fireRelaxed(newMatcher)
                } else {
                    logger.fine("old set contains new set (whitelist) -> firing constrained")
                    fireConstrained(newMatcher)
                }
            }

            newSet.containsAll(oldSet) -> {
                val newMatcher = SetMatcher(newSet, mode, function)
                if (mode == Mode.BLACKLIST) {
                    logger.fine("new set contains old set (blacklist) -> firing constrained")
                    fireConstrained(newMatcher)
                } else {
                    logger.fine("new set contains old set (whitelist) -> firing relaxed")
                    fireRelaxed(newMatcher)
                }
            }

            else -> {
                logger.fine("old and new set differ -> firing change")
                fireChanged(SetMatcher(newSet, mode, function))
            }
        }
    }

    /** Supported modes of operation. */
    enum class Mode {
        /** The match set specifies values to exclude. */
        BLACKLIST,

        /** The match set specifies values to include; an empty set matches none. */
        WHITELIST_EMPTY_MATCH_NONE,

        /** The match set specifies values to include; an empty set matches all. */
        WHITELIST_EMPTY_MATCH_ALL,
    }

    private class SetMatcher<E, O>(
        matchSet: Set<O>,
        private val mode: Mode,
        private val function: (E) -> O,
    ) : Matcher<E> {
        val matchSet: Set<O> = matchSet.toHashSet()

        override fun matches(item: E): Boolean {
            val contained = matchSet.contains(function(item))
            return if (mode == Mode.BLACKLIST) !contained else contained
        }
    }

    companion object {
        private val logger = Logger.getLogger(SetMatcherEditor::class.java.toString())

        /** Creates an editor using [mode] and [function] to extract matched values. */
        fun <E, O> create(mode: Mode, function: (E) -> O): SetMatcherEditor<E, O> =
            SetMatcherEditor(mode, function)
    }
}
