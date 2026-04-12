/*
 * Copyright (c) 2025-2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool.episodes

import java.util.*
import java.util.regex.Pattern

class TitleParserManager {
    private val parsers = HashMap<String, RuleBasedTitleParser>()

    init {
        registerRules(
            "3Sat",
            // 1) Parenthesized "(S2025/E15)"
            rule("\\(\\s*[sS](?<season>\\d{4})/[eE](?<episode>\\d{1,2})\\s*\\)", "(", "/"),
            // 2) Un-parenthesized "S2025/E15"
            rule("[sS](?<season>\\d{4})/[eE](?<episode>\\d{1,2})", "/"),
            // 3) Continuous "S2024E81"
            rule("[sS](?<season>\\d{4})[eE](?<episode>\\d{1,2})"),
            // 4) Parenthesized hyphen "(S2025-E15)"
            rule("\\(\\s*[sS](?<season>\\d{4})-[eE](?<episode>\\d{1,2})\\s*\\)", "(", "-"),
        )
        registerRules(
            "ARD",
            // 1) "15. Staffel, 188. Die alte Frau" - dot-number season, comma, dot-number episode
            rule("(?<season>\\d{1,2})\\.\\s*Staffel,\\s*(?<episode>\\d{1,3})\\.", "staffel"),
            // 2) "Sturm der Liebe\", Staffel 1, Folge 1 (2005)" - explicit Staffel...Folge
            rule("Staffel\\s*(?<season>\\d{1,2})[,\\s]*Folge\\s*(?<episode>\\d{1,3})", "staffel", "folge"),
            // 3) "(S03E27352)" - parenthesized SxxExxxxx
            rule("\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,5})\\s*\\)", "("),
            // 4) "(S01/E02)" - parenthesized Sxx/Exx
            rule("\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})\\s*\\)", "(", "/"),
            // 5) "S3/E12" - inline Sx/Exx
            rule("[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})", "/"),
        )
        registerRules(
            "ARD-alpha",
            // 1) Parenthesized "(S01/E04)"
            rule("\\(\\s*[sS](?<season>\\d{2})/[eE](?<episode>\\d{2})\\s*\\)", "(", "/"),
            // 2) Un-parenthesized "S05/E03"
            rule("[sS](?<season>\\d{2})/[eE](?<episode>\\d{2})", "/"),
            // 3) Continuous "S05E03"
            rule("[sS](?<season>\\d{2})[eE](?<episode>\\d{2})"),
        )
        registerRules(
            "BR",
            // 1) Parenthesized "(S03/E05)"
            rule("\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)", "(", "/"),
            // 2) Prefix-colon pattern: "100: ... (S04/E20)"
            rule("^(?:\\d{1,3}):[^\\(]*\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)", ":", "(", "/"),
            // 3) "Folge 5: ... (S02/E05)"
            rule("Folge\\s*\\d{1,3}[:\\-][^\\(]*\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)", "folge", "(", "/"),
        )
        registerRules(
            "HR",
            // 1) Parenthesized slash notation, e.g. (S01/E11)
            rule("\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,3})\\s*\\)", "(", "/"),
            // 2) Staffel X, Folge Y pattern, e.g. Staffel 10, Folge 5
            rule("Staffel\\s*(?<season>\\d{1,2})\\s*,\\s*Folge\\s*(?<episode>\\d{1,3})", "staffel", "folge"),
        )
        registerRules(
            "MDR",
            // 1) Parenthesized slash notation, e.g. (S04/E03)
            rule("\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,3})\\s*\\)", "(", "/"),
            // 2) Un-parenthesized slash notation, e.g. S04/E03
            rule("[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,3})", "/"),
        )
        registerRules(
            "NDR",
            // 1) Parenthesized slash notation: (S03x/E07)
            rule("\\(\\s*[sS](?<season>\\d{1,3})/[eE](?<episode>\\d{1,2})\\s*\\)", "(", "/"),
        )
        registerRules(
            "ONE",
            // 1) "15. Staffel, 188. Die alte Frau" - dot-number season, comma, dot-number episode
            rule("(?<season>\\d{1,2})\\.\\s*Staffel,\\s*(?<episode>\\d{1,3})\\.", "staffel"),
            // 2) "Sturm der Liebe\", Staffel 1, Folge 1 (2005)" - explicit Staffel...Folge
            rule("Staffel\\s*(?<season>\\d{1,2})[,\\s]*Folge\\s*(?<episode>\\d{1,3})", "staffel", "folge"),
            // 3) "(S03E27352)" - parenthesized SxxExxxxx
            rule("\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,5})\\s*\\)", "("),
            // 4) "(S01/E02)" - parenthesized Sxx/Exx
            rule("\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})\\s*\\)", "(", "/"),
            // 5) "S3/E12" - inline Sx/Exx
            rule("[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})", "/"),
        )
        registerRules(
            "RBB",
            // 1) Parenthesized S/E pattern, e.g. (S05/E01)
            rule("\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)", "(", "/"),
            // 2) Parenthesized ratio-only pattern, e.g. (1/4)
            rule("\\(\\s*(?<season>\\d{1,2})/(?<episode>\\d{1,2})\\s*\\)", "(", "/"),
        )
        registerRules(
            "SWR",
            // 1) Parenthesized slash notation: (S03x/E07)
            rule("\\(\\s*[sS](?<season>\\d{1,3})/[eE](?<episode>\\d{1,2})\\s*\\)", "(", "/"),
            // 2) Parenthesized continuous notation: (S03E07)
            rule("\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})\\s*\\)", "("),
            // 3) Inline slash notation: S03/E07
            rule("[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})", "/"),
            // 4) Inline continuous notation: S03E07
            rule("[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})"),
            // 5) German Staffel/Folge: Staffel 2, Folge 5
            rule("Staffel\\s*(?<season>\\d{1,2})\\s*,\\s*Folge\\s*(?<episode>\\d{1,3})", "staffel", "folge"),
        )
        registerRules(
            "WDR",
            // 1) Parenthesized slash notation: (S03x/E07)
            rule("\\(\\s*[sS](?<season>\\d{1,3})/[eE](?<episode>\\d{1,2})\\s*\\)", "(", "/"),
            // 2) Parenthesized continuous notation: (S03E07)
            rule("\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})\\s*\\)", "("),
            // 3) Inline slash notation: S03/E07
            rule("[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})", "/"),
            // 4) Inline continuous notation: S03E07
            rule("[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})"),
            // 5) German Staffel/Folge: Staffel 2, Folge 5
            rule("Staffel\\s*(?<season>\\d{1,2})\\s*,\\s*Folge\\s*(?<episode>\\d{1,3})", "staffel", "folge"),
        )
        registerRules(
            "ZDF",
            // Parenthesized slash notation, e.g. (S2025/E11)
            rule("\\(\\s*[sS](?<season>\\d{1,4})/[eE](?<episode>\\d{1,4})\\s*\\)", "(", "/"),
            // Parenthesized continuous notation, e.g. (S2024E11)
            rule("\\(\\s*[sS](?<season>\\d{1,4})[eE](?<episode>\\d{1,4})\\s*\\)", "("),
            // Un-parenthesized slash notation, e.g. S2025/E102
            rule("[sS](?<season>\\d{1,4})/[eE](?<episode>\\d{1,4})", "/"),
            // Un-parenthesized continuous notation, e.g. S01E16
            rule("[sS](?<season>\\d{1,4})[eE](?<episode>\\d{1,4})"),
        )
        registerRules(
            "ZDFneo",
            // 1) Parenthesized slash notation: (S03x/E07)
            rule("\\(\\s*[sS](?<season>\\d{1,3})/[eE](?<episode>\\d{1,2})\\s*\\)", "(", "/"),
            // 2) Parenthesized continuous notation: (S03E07)
            rule("\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})\\s*\\)", "("),
            // 3) Inline slash notation: S03/E07
            rule("[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})", "/"),
            // 4) Inline continuous notation: S03E07
            rule("[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})"),
            // 5) German Staffel/Folge: Staffel 2, Folge 5
            rule("Staffel\\s*(?<season>\\d{1,2})\\s*,\\s*Folge\\s*(?<episode>\\d{1,3})", "staffel", "folge"),
        )
        registerRules(
            "ZDF-tivi",
            // 1) Parenthesized slash notation: (S03x/E07)
            rule("\\(\\s*[sS](?<season>\\d{1,3})/[eE](?<episode>\\d{1,2})\\s*\\)", "(", "/"),
            // 2) Parenthesized continuous notation: (S03E07)
            rule("\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})\\s*\\)", "("),
            // 3) Inline slash notation: S03/E07
            rule("[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})", "/"),
            // 4) Inline continuous notation: S03E07
            rule("[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})"),
            // 5) German Staffel/Folge: Staffel 2, Folge 5
            rule("Staffel\\s*(?<season>\\d{1,2})\\s*,\\s*Folge\\s*(?<episode>\\d{1,3})", "staffel", "folge"),
        )
    }

    /**
     * Register a sender with its regex patterns.
     * @param sender the key (e.g. "3Sat")
     * @param patterns varargs of regex strings with named groups 'season' and 'episode'
     */
    fun register(sender: String, vararg patterns: String) {
        val compiled = patterns.map { regex -> rule(regex) }
        parsers[sender] = RuleBasedTitleParser(compiled)
    }

    private fun registerRules(sender: String, vararg patterns: RuleBasedTitleParser.RulePattern) {
        parsers[sender] = RuleBasedTitleParser(patterns.toList())
    }

    private fun rule(regex: String, vararg requiredMarkers: String): RuleBasedTitleParser.RulePattern =
        RuleBasedTitleParser.RulePattern(
            Pattern.compile(regex, Pattern.CASE_INSENSITIVE),
            requiredMarkers.copyOf(),
        )

    /**
     * Parse a title for a given sender.
     */
    fun parse(sender: String, title: String): Optional<SeasonEpisode> =
        parsers[sender]?.parse(title) ?: Optional.empty()
}
