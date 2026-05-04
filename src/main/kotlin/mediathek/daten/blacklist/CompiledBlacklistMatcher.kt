package mediathek.daten.blacklist

import mediathek.daten.DatenFilm
import mediathek.tool.Filter
import java.util.*
import java.util.regex.Pattern

class CompiledBlacklistMatcher(rules: List<BlacklistRule>) {
    private val ruleIndex = RuleIndex().apply {
        rules.forEach { add(CompiledRule.from(it)) }
    }

    fun matches(film: DatenFilm): Boolean =
        ruleIndex.matches(FilmFields(film))

    private data class CompiledRule(
        val sender: String,
        val thema: String,
        val titleMatcher: FieldMatcher,
        val themaTitleMatcher: FieldMatcher
    ) {
        fun matches(film: FilmFields): Boolean =
            titleMatcher.matchesTitle(film) &&
                (themaTitleMatcher.matchesThema(film) || themaTitleMatcher.matchesTitle(film))

        companion object {
            fun from(rule: BlacklistRule): CompiledRule =
                CompiledRule(
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

        fun matches(film: FilmFields): Boolean =
            matchesAny(globalRules, film) ||
                matchesAny(senderRules[film.sender], film) ||
                matchesAny(themaRules[film.thema], film) ||
                matchesSenderThemaRules(film)

        private fun matchesSenderThemaRules(film: FilmFields): Boolean {
            val rulesByThema = senderThemaRules[film.sender] ?: return false
            return matchesAny(rulesByThema[film.thema], film)
        }

        private fun matchesAny(rules: List<CompiledRule>?, film: FilmFields): Boolean =
            rules?.any { it.matches(film) } == true
    }

    private class FieldMatcher private constructor(
        private val pattern: Pattern?,
        private val literalTokens: Array<String>,
        private val matchAll: Boolean
    ) {
        fun matchesTitle(film: FilmFields): Boolean =
            matches(film.title, film::lowercaseTitle)

        fun matchesThema(film: FilmFields): Boolean =
            matches(film.thema, film::lowercaseThema)

        private fun matches(input: String, lowercaseInput: () -> String): Boolean =
            when {
                matchAll -> true
                pattern != null -> pattern.matcher(input).matches()
                else -> Filter.checkLowercase(literalTokens, lowercaseInput())
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

    private class FilmFields(private val film: DatenFilm) {
        val sender: String
            get() = film.sender

        val thema: String
            get() = film.thema

        val title: String
            get() = film.title

        private var lowercaseThema: String? = null
        private var lowercaseTitle: String? = null

        fun lowercaseThema(): String =
            lowercaseThema ?: thema.lowercase(Locale.getDefault()).also { lowercaseThema = it }

        fun lowercaseTitle(): String =
            lowercaseTitle ?: title.lowercase(Locale.getDefault()).also { lowercaseTitle = it }
    }
}
