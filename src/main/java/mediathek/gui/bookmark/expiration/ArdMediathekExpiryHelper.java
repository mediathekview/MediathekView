package mediathek.gui.bookmark.expiration;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

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

    public static Optional<ExpiryInfo> getExpiryInfo(String url) {
        try {
            Document doc = Jsoup.connect(url)
                    .userAgent("Mozilla/5.0")
                    .get();

            String body = doc.body().text();
            Matcher m = AVAILABLE_UNTIL_PATTERN.matcher(body);
            if (m.find()) {
                String date = m.group(1);   // z.B. "28.06.2027"
                LocalDate ld = LocalDate.parse(date, DateTimeFormatter.ofPattern("dd.MM.yyyy"));
                return Optional.of(new ExpiryInfo(ld));
            }
        }
        catch (Exception _) {
        }
        return Optional.empty();
    }
}
