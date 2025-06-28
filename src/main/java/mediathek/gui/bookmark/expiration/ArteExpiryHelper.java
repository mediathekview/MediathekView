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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import mediathek.tool.datum.DateUtil;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ArteExpiryHelper {

    private static final ObjectMapper JSON = new ObjectMapper();

    // Mehrsprachiges Pattern für DE, EN, FR, ES, IT, PL
    private static final Pattern TEXT_DATE_PATTERN = Pattern.compile(
            "(Verfügbar bis zum|Available until|Disponible hasta el|Disponible jusqu'?au|Disponibile fino al|Dostępny do)\\s*(\\d{2}/\\d{2}/\\d{4})"
    );

    public static Optional<ExpiryInfo> getExpiryInfo(String url) {
        try {
            Document doc = Jsoup.connect(url)
                    .userAgent("Mozilla/5.0")
                    .get();

            // 1) JSON-LD durchsuchen
            Elements scripts = doc.select("script[type=application/ld+json]");
            for (var script : scripts) {
                JsonNode root = JSON.readTree(script.html());
                Optional<ExpiryInfo> info = searchRecursiveForDate(root);
                if (info.isPresent())
                    return info;
            }

            // 2) API endpoint via data-configuration
            var video = doc.selectFirst("video[data-configuration], video[data-href]");
            if (video != null) {
                String cfgUrl = video.hasAttr("data-configuration")
                        ? video.attr("data-configuration")
                        : video.attr("data-href");
                if (!cfgUrl.isBlank()) {
                    JsonNode root2 = JSON.readTree(
                            Jsoup.connect(cfgUrl).ignoreContentType(true).execute().body()
                    );
                    JsonNode avail = root2.path("availability").path("availabilityEnds");
                    if (!avail.isMissingNode() && !avail.asText().isBlank()) {
                        Instant expiry = Instant.parse(avail.asText());
                        return Optional.of(buildInfo(expiry));
                    }
                }
            }

            // 3) Sichtbarer Text-Hinweis mehrsprachig
            String bodyText = doc.body().text();
            Matcher m = TEXT_DATE_PATTERN.matcher(bodyText);
            if (m.find()) {
                String rawDate = m.group(2); // DD/MM/YYYY
                LocalDate date = LocalDate.parse(rawDate, DateTimeFormatter.ofPattern("dd/MM/yyyy"));
                ZonedDateTime z = date.atTime(23, 59).atZone(ZoneId.of("Europe/Paris"));
                Instant expiry = z.toInstant();
                return Optional.of(buildInfo(expiry));
            }

        }
        catch (Exception _) {
        }
        return Optional.empty();
    }

    // Rekursive Suche nach "availabilityEnds" oder "expires"
    private static Optional<ExpiryInfo> searchRecursiveForDate(JsonNode node) {
        if (node == null)
            return Optional.empty();

        if (node.has("availabilityEnds")) {
            String text = node.get("availabilityEnds").asText();
            if (!text.isBlank()) {
                return Optional.of(buildInfo(Instant.parse(text)));
            }
        }

        if (node.has("expires")) {
            String text = node.get("expires").asText();
            if (!text.isBlank()) {
                return Optional.of(buildInfo(Instant.parse(text)));
            }
        }

        for (JsonNode child : node) {
            Optional<ExpiryInfo> found = searchRecursiveForDate(child);
            if (found.isPresent())
                return found;
        }
        return Optional.empty();
    }

    private static ExpiryInfo buildInfo(Instant expiry) {
        return new ExpiryInfo(LocalDate.ofInstant(expiry, DateUtil.MV_DEFAULT_TIMEZONE));
    }
}
