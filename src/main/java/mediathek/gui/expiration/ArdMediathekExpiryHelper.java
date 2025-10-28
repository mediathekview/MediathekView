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

package mediathek.gui.expiration;

import mediathek.config.Konstanten;
import mediathek.tool.http.MVHttpClient;
import okhttp3.Request;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jsoup.Jsoup;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ArdMediathekExpiryHelper {

    // Pattern für "Video verfügbar: bis DD.MM.YYYY ∙ HH:MM Uhr"
    private static final Pattern AVAILABLE_UNTIL_PATTERN = Pattern.compile(
            "Video verfügbar:.*?bis\\s*(\\d{2}\\.\\d{2}\\.\\d{4})\\s*∙\\s*(\\d{2}:\\d{2})\\s*Uhr",
            Pattern.DOTALL);
    private static final Logger LOG = LogManager.getLogger();

    public static Optional<ExpiryInfo> getExpiryInfo(String url) {
        final Request request = new Request.Builder().url(url).get()
                .header("User-Agent", Konstanten.JSOUP_USER_AGENT)
                .build();

        try (var response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute()) {
            if (response.isSuccessful()) {
                var doc = Jsoup.parse(response.body().string());
                String body = doc.body().text();
                Matcher m = AVAILABLE_UNTIL_PATTERN.matcher(body);
                if (m.find()) {
                    String date = m.group(1);   // z.B. "28.06.2027"
                    LocalDate ld = LocalDate.parse(date, DateTimeFormatter.ofPattern("dd.MM.yyyy"));
                    return Optional.of(new ExpiryInfo(ld));
                }
            }
            else {
                LOG.error("Could not fetch expiry data from {}", url);
            }
        }
        catch (Exception _) {
        }

        return Optional.empty();
    }
}
