package mediathek.daten.blacklist

import mediathek.daten.DatenFilm
import mediathek.tool.Filter
import java.util.*
import java.util.regex.Pattern

class CompiledBlacklistMatcher(rules: List<BlacklistRule>) {
    private val ruleCount = rules.size
    private val ruleIndex = RuleIndex().apply {
        rules.forEachIndexed { index, rule -> add(CompiledRule.from(index, rule)) }
    }

    fun matches(film: DatenFilm): Boolean =
        ruleIndex.matches(film)

    fun countMatchesByRule(films: Iterable<DatenFilm>): IntArray {
        val counts = IntArray(ruleCount)
        films.forEach { film -> ruleIndex.countMatches(film, counts) }
        return counts
    }

    private data class CompiledRule(
        val index: Int,
        val sender: String,
        val thema: String,
        val titleMatcher: FieldMatcher,
        val themaTitleMatcher: FieldMatcher
    ) {
        fun matches(film: DatenFilm): Boolean =
            titleMatcher.matchesTitle(film) &&
                (themaTitleMatcher.matchesThema(film) || themaTitleMatcher.matchesTitle(film))

        companion object {
            fun from(index: Int, rule: BlacklistRule): CompiledRule =
                CompiledRule(
                    index,
                    rule.sender,
                    rule.thema,
                    FieldMatcher.from(rule.titel),
                    FieldMatcher.from(rule.thema_titel)
                )
        }
    }

    private class RuleIndex {
        private val globalRules = mutableListOf<CompiledRule>()
        private val senderRules = mutableMapOf<String, MutableList<CompiledRule>>()
        private val themaRules = TreeMap<String, MutableList<CompiledRule>>(String.CASE_INSENSITIVE_ORDER)
        private val senderThemaRules = mutableMapOf<String, TreeMap<String, MutableList<CompiledRule>>>()

        fun add(rule: CompiledRule) {
            val hasSender = rule.sender.isNotEmpty()
            val hasThema = rule.thema.isNotEmpty()

            when {
                hasSender && hasThema -> senderThemaRules
                    .getOrPut(rule.sender) { TreeMap(String.CASE_INSENSITIVE_ORDER) }
                    .getOrPut(rule.thema) { mutableListOf() }
                    .add(rule)

                hasSender -> senderRules.getOrPut(rule.sender) { mutableListOf() }.add(rule)
                hasThema -> themaRules.getOrPut(rule.thema) { mutableListOf() }.add(rule)
                else -> globalRules.add(rule)
            }
        }

        fun matches(film: DatenFilm): Boolean =
            matchesAny(globalRules, film) ||
                matchesAny(senderRules[film.sender], film) ||
                matchesAny(themaRules[film.thema], film) ||
                matchesSenderThemaRules(film)

        fun countMatches(film: DatenFilm, counts: IntArray) {
            countMatches(globalRules, film, counts)
            countMatches(senderRules[film.sender], film, counts)
            countMatches(themaRules[film.thema], film, counts)
            countMatches(senderThemaRules[film.sender]?.get(film.thema), film, counts)
        }

        private fun matchesSenderThemaRules(film: DatenFilm): Boolean {
            val rulesByThema = senderThemaRules[film.sender] ?: return false
            return matchesAny(rulesByThema[film.thema], film)
        }

        private fun matchesAny(rules: List<CompiledRule>?, film: DatenFilm): Boolean {
            if (rules == null) {
                return false
            }

            var index = 0
            while (index < rules.size) {
                if (rules[index].matches(film)) {
                    return true
                }
                index++
            }
            return false
        }

        private fun countMatches(rules: List<CompiledRule>?, film: DatenFilm, counts: IntArray) {
            if (rules == null) {
                return
            }

            var index = 0
            while (index < rules.size) {
                val rule = rules[index]
                if (rule.matches(film)) {
                    counts[rule.index]++
                }
                index++
            }
        }
    }

    private class FieldMatcher private constructor(
        private val pattern: Pattern?,
        private val literalTokens: Array<String>,
        private val matchAll: Boolean
    ) {
        fun matchesTitle(film: DatenFilm): Boolean =
            matches(film.title)

        fun matchesThema(film: DatenFilm): Boolean =
            matches(film.thema)

        private fun matches(input: String): Boolean =
            when {
                matchAll -> true
                pattern != null -> pattern.matcher(input).matches()
                else -> Filter.checkContainsIgnoreCase(literalTokens, input)
            }

        companion object {
            private val emptyString = arrayOf("")
            private val matchAll = FieldMatcher(null, emptyString, true)

            fun from(inputString: String): FieldMatcher {
                if (inputString.isEmpty()) {
                    return matchAll
                }

                if (Filter.isPattern(inputString)) {
                    val pattern = Filter.makePattern(inputString)
                    if (pattern != null) {
                        return FieldMatcher(pattern, emptyString, false)
                    }
                }

                val tokens = inputString.lowercase(Locale.getDefault()).split(",").toTypedArray()
                if (tokens.isEmpty() || tokens.any { it.isEmpty() }) {
                    return matchAll
                }
                return FieldMatcher(null, tokens, false)
            }
        }
    }
}
