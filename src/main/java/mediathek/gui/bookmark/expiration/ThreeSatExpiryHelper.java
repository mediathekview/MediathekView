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

package mediathek.gui.bookmark.expiration;


import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ThreeSatExpiryHelper {

    // Pattern: "Verfügbar bis: bis DD.MM.YYYY"
    private static final Pattern TEXT_DATE_PATTERN = Pattern.compile(
            "(Verfügbar bis:?)\\s*(?:bis\\s*)?(\\d{2}\\.\\d{2}\\.\\d{4})"
    );

    public static Optional<ExpiryInfo> getExpiryInfo(String url) {
        try {
            Document doc = Jsoup.connect(url)
                    .userAgent("Mozilla/5.0")
                    .get();

            String body = doc.body().text();
            Matcher m = TEXT_DATE_PATTERN.matcher(body);
            if (m.find()) {
                LocalDate date = LocalDate.parse(m.group(2), DateTimeFormatter.ofPattern("dd.MM.yyyy"));
                return Optional.of(new ExpiryInfo(date));
            }
        }
        catch (Exception e) {
            System.err.println("Fehler beim Abruf/Parsing: " + e.getMessage());
        }

        return Optional.empty();
    }
}
