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

class RuleBasedTitleParser(
    private val patterns: List<RulePattern>,
) {
    /**
     * Try to match each pattern against the title.
     * @return Optional.of(SeasonEpisode) if a pattern matches; otherwise Optional.empty().
     */
    fun parse(title: String): Optional<SeasonEpisode> =
        Optional.ofNullable(parseOrNull(title))

    fun parseOrNull(title: String): SeasonEpisode? {
        for (rulePattern in patterns) {
            if (!rulePattern.matchesGuards(title)) {
                continue
            }

            val matcher = rulePattern.pattern.matcher(title)
            if (matcher.find()) {
                val season = matcher.group("season").toInt()
                val episode = matcher.group("episode").toInt()
                return SeasonEpisode(season, episode)
            }
        }
        return null
    }

    class RulePattern(
        val pattern: Pattern,
        requiredMarkers: Array<out String>,
    ) {
        private val requiredMarkers = Array(requiredMarkers.size) { index -> RequiredMarker(requiredMarkers[index]) }

        fun matchesGuards(title: String): Boolean {
            var index = 0
            while (index < requiredMarkers.size) {
                if (!requiredMarkers[index].isContainedIn(title)) {
                    return false
                }
                index++
            }
            return true
        }

        private class RequiredMarker(private val value: String) {
            private val ignoreCase = value.any(Char::isLetter) && value == value.lowercase(Locale.ROOT)

            fun isContainedIn(title: String): Boolean =
                if (ignoreCase) {
                    title.contains(value, ignoreCase = true)
                } else {
                    title.contains(value)
                }
        }
    }
}
