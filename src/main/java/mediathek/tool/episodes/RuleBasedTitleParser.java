/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.tool.episodes;

import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

public class RuleBasedTitleParser {
    private final List<Pattern> patterns;

    public RuleBasedTitleParser(List<Pattern> patterns) {
        this.patterns = patterns;
    }

    /**
     * Try to match each pattern against the title.
     * @return Optional.of(SeasonEpisode) if a pattern matches; otherwise Optional.empty().
     */
    public Optional<SeasonEpisode> parse(String title) {
        for (var pat : patterns) {
            var m = pat.matcher(title);
            if (m.find()) {
                int season = Integer.parseInt(m.group("season"));
                int episode = Integer.parseInt(m.group("episode"));
                return Optional.of(new SeasonEpisode(season, episode));
            }
        }
        return Optional.empty();
    }
}