package mediathek.tool;

import okhttp3.Request;
import okhttp3.Response;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class FileSize {
    private static final Logger logger = LogManager.getLogger();

    public static String laengeString(String url) {
        // liefert die Dateigröße einer URL in MB!!
        // Anzeige der Größe in MiB und deshalb: Faktor 1000
        String groesseStr = "";

        long l = getFileSizeFromUrl(url);
        if (l > 1_000_000) {
            // größer als 1MiB sonst kann ich mirs sparen
            groesseStr = String.valueOf(l / 1_000_000);
        } else if (l > 0) {
            groesseStr = "1";
        }
        return groesseStr;
    }

    /**
     * Get the content length from Http Header. Used with HEAD requests
     *
     * @param response the response for reading length
     * @return the length if available, -1 otherwise.
     */
    public static long getContentLength(@NotNull Response response) {
        var sizeStr = response.headers().get("Content-Length");
        long respLength = -1;

        if (sizeStr != null) {
            try {
                respLength = Long.parseLong(sizeStr);
            } catch (NumberFormatException ignored) {
            }
        }

        return respLength;
    }

    /**
     * Return the size of a URL in bytes.
     *
     * @param url URL as String to query.
     * @return size in bytes or -1.
     */
    private static long getFileSizeFromUrl(@NotNull String url) {
        final var lUrl = url.toLowerCase();
        if (!lUrl.startsWith("http") || lUrl.endsWith(".m3u8")) {
            return -1;
        }

        logger.trace("getFileSizeFromUrl for: {}", url);
        final Request request = new Request.Builder().url(url).head().build();
        long respLength = -1;
        try (Response response = MVHttpClient.getInstance().getReducedTimeOutClient().newCall(request).execute()) {
            if (response.isSuccessful()) {
                respLength = getContentLength(response);
            }
        } catch (IOException ignored) {
        }

        if (respLength < FileUtils.ONE_MB) {
            // alles unter 1MB sind Playlisten, ORF: Trailer bei im Ausland gesperrten Filmen, ...
            // dann wars nix
            respLength = -1;
        }
        return respLength;
    }

}
