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

import java.util.*;
import java.util.regex.Pattern;

public class TitleParserManager {
    private final Map<String, RuleBasedTitleParser> parsers = new HashMap<>();

    public TitleParserManager() {
        register("3Sat",
                // 1) Parenthesized "(S2025/E15)"
                "\\(\\s*[sS](?<season>\\d{4})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 2) Un-parenthesized "S2025/E15"
                "[sS](?<season>\\d{4})/[eE](?<episode>\\d{1,2})",
                // 3) Continuous "S2024E81"
                "[sS](?<season>\\d{4})[eE](?<episode>\\d{1,2})",
                // 4) Parenthesized hyphen "(S2025-E15)"
                "\\(\\s*[sS](?<season>\\d{4})-[eE](?<episode>\\d{1,2})\\s*\\)"
        );
        register("ARD",
                // 1) "15. Staffel, 188. Die alte Frau" – dot-number season, comma, dot-number episode
                "(?<season>\\d{1,2})\\.\\s*Staffel,\\s*(?<episode>\\d{1,3})\\.",
                // 2) "Sturm der Liebe\", Staffel 1, Folge 1 (2005)" – explicit Staffel…Folge
                "Staffel\\s*(?<season>\\d{1,2})[,\\s]*Folge\\s*(?<episode>\\d{1,3})",
                // 3) "(S03E27352)" – parenthesized SxxExxxxx
                "\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,5})\\s*\\)",
                // 4) "(S01/E02)" – parenthesized Sxx/Exx
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})\\s*\\)",
                // 5) "S3/E12" – inline Sx/Exx
                "[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})"
        );
        register("ARD-alpha",
                // 1) Parenthesized "(S01/E04)"
                "\\(\\s*[sS](?<season>\\d{2})/[eE](?<episode>\\d{2})\\s*\\)",
                // 2) Un-parenthesized "S05/E03"
                "[sS](?<season>\\d{2})/[eE](?<episode>\\d{2})",
                // 3) Continuous "S05E03"
                "[sS](?<season>\\d{2})[eE](?<episode>\\d{2})"
        );
        register("BR",
                // 1) Parenthesized "(S03/E05)"
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 2) Prefix-colon pattern: "100: ... (S04/E20)"
                "^(?:\\d{1,3}):[^\\(]*\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 3) "Folge 5: ... (S02/E05)"
                "Folge\\s*\\d{1,3}[:\\-][^\\(]*\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)"
        );
        register("HR",
                // 1) Parenthesized slash notation, e.g. (S01/E11)
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,3})\\s*\\)",
                // 2) Staffel X, Folge Y pattern, e.g. Staffel 10, Folge 5
                "Staffel\\s*(?<season>\\d{1,2})\\s*,\\s*Folge\\s*(?<episode>\\d{1,3})"
        );
        register("MDR",
                // 1) Parenthesized slash notation, e.g. (S04/E03)
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,3})\\s*\\)",
                // 2) Un-parenthesized slash notation, e.g. S04/E03
                "[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,3})"
        );
        register("NDR",
                // 1) Parenthesized slash notation: (S03x/E07)
                "\\(\\s*[sS](?<season>\\d{1,3})/[eE](?<episode>\\d{1,2})\\s*\\)");
        register("ONE",
                // 1) "15. Staffel, 188. Die alte Frau" – dot-number season, comma, dot-number episode
                "(?<season>\\d{1,2})\\.\\s*Staffel,\\s*(?<episode>\\d{1,3})\\.",
                // 2) "Sturm der Liebe\", Staffel 1, Folge 1 (2005)" – explicit Staffel…Folge
                "Staffel\\s*(?<season>\\d{1,2})[,\\s]*Folge\\s*(?<episode>\\d{1,3})",
                // 3) "(S03E27352)" – parenthesized SxxExxxxx
                "\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,5})\\s*\\)",
                // 4) "(S01/E02)" – parenthesized Sxx/Exx
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})\\s*\\)",
                // 5) "S3/E12" – inline Sx/Exx
                "[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,5})"
        );
        register("RBB",
                // 1) Parenthesized S/E pattern, e.g. (S05/E01)
                "\\(\\s*[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 2) Parenthesized ratio-only pattern, e.g. (1/4)
                "\\(\\s*(?<season>\\d{1,2})/(?<episode>\\d{1,2})\\s*\\)"
        );
        register("WDR",
                // 1) Parenthesized slash notation: (S03x/E07)
                "\\(\\s*[sS](?<season>\\d{1,3})/[eE](?<episode>\\d{1,2})\\s*\\)",
                // 2) Parenthesized continuous notation: (S03E07)
                "\\(\\s*[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})\\s*\\)",
                // 3) Inline slash notation: S03/E07
                "[sS](?<season>\\d{1,2})/[eE](?<episode>\\d{1,2})",
                // 4) Inline continuous notation: S03E07
                "[sS](?<season>\\d{1,2})[eE](?<episode>\\d{1,2})",
                // 5) German Staffel/Folge: Staffel 2, Folge 5
                "Staffel\\s*(?<season>\\d{1,2})\\s*,\\s*Folge\\s*(?<episode>\\d{1,3})"
        );
        register("ZDF",
                // Parenthesized slash notation, e.g. (S2025/E11)
                "\\(\\s*[sS](?<season>\\d{1,4})/[eE](?<episode>\\d{1,4})\\s*\\)",
                // Parenthesized continuous notation, e.g. (S2024E11)
                "\\(\\s*[sS](?<season>\\d{1,4})[eE](?<episode>\\d{1,4})\\s*\\)",
                // Un-parenthesized slash notation, e.g. S2025/E102
                "[sS](?<season>\\d{1,4})/[eE](?<episode>\\d{1,4})",
                // Un-parenthesized continuous notation, e.g. S01E16
                "[sS](?<season>\\d{1,4})[eE](?<episode>\\d{1,4})"
        );
    }

    /**
     * Register a sender with its regex patterns.
     * @param sender the key (e.g. "3Sat")
     * @param patterns varargs of regex strings with named groups 'season' and 'episode'
     */
    public void register(String sender, String... patterns) {
        List<Pattern> compiled = new ArrayList<>();
        for (String regex : patterns) {
            // enable named groups, case-insensitive by default
            compiled.add(Pattern.compile(regex, Pattern.CASE_INSENSITIVE));
        }
        parsers.put(sender, new RuleBasedTitleParser(compiled));
    }

    /**
     * Parse a title for a given sender.
     */
    public Optional<SeasonEpisode> parse(String sender, String title) {
        RuleBasedTitleParser parser = parsers.get(sender);
        if (parser == null) {
            return Optional.empty();
        }
        return parser.parse(title);
    }
}