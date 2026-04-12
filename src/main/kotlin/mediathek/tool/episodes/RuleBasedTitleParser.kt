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
    fun parse(title: String): Optional<SeasonEpisode> {
        val titleLowercase = title.lowercase(Locale.ROOT)
        for (rulePattern in patterns) {
            if (!rulePattern.matchesGuards(title, titleLowercase)) {
                continue
            }

            val matcher = rulePattern.pattern.matcher(title)
            if (matcher.find()) {
                val season = matcher.group("season").toInt()
                val episode = matcher.group("episode").toInt()
                return Optional.of(SeasonEpisode(season, episode))
            }
        }
        return Optional.empty()
    }

    class RulePattern(
        val pattern: Pattern,
        private val requiredMarkers: Array<out String>,
    ) {
        fun matchesGuards(title: String, titleLowercase: String): Boolean =
            requiredMarkers.all { marker -> containsMarker(title, titleLowercase, marker) }

        private fun containsMarker(title: String, titleLowercase: String, marker: String): Boolean =
            if (marker == marker.lowercase(Locale.ROOT)) {
                titleLowercase.contains(marker)
            } else {
                title.contains(marker)
            }
    }
}
