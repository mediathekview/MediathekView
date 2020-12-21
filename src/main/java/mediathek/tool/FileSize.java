package mediathek.tool;

import mediathek.tool.http.MVHttpClient;
import okhttp3.HttpUrl;
import okhttp3.Request;
import okhttp3.Response;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class FileSize {
    public static final int ONE_MiB = 1_000_000;
    private static final Logger logger = LogManager.getLogger();

    /**
     * Get the length of the file specified by url.
     *
     * @param url the url pointing to the file
     * @return file size in MiByte as a String.
     */
    @NotNull
    public static String getFileLengthFromUrl(@NotNull String url) {
        String groesseStr = "";

        HttpUrl okUrl = HttpUrl.parse(url);
        if (okUrl != null) {
            long l = getFileSizeFromUrl(okUrl);
            groesseStr = convertSize(l);
        }

        return groesseStr;
    }

    /**
     * Convert size from bytes to MBytes
     *
     * @param byteLength size in bytes
     * @return size in MBytes as String.
     */
    @NotNull
    public static String convertSize(long byteLength) {
        String ret = "";
        if (byteLength > ONE_MiB) {
            // größer als 1MB sonst kann ich mirs sparen
            ret = String.valueOf(byteLength / ONE_MiB);
        } else if (byteLength > 0) {
            ret = "1";
        }
        return ret;
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
    public static long getFileSizeFromUrl(@NotNull HttpUrl url) {
        if (!url.scheme().startsWith("http") || url.encodedPath().endsWith(".m3u8")) {
            return -1;
        }

        logger.info("Requesting file size for: {}", url);
        final Request request = new Request.Builder().url(url).head().build();
        long respLength = -1;
        try (Response response = MVHttpClient.getInstance().getReducedTimeOutClient().newCall(request).execute()) {
            if (response.isSuccessful()) {
                respLength = getContentLength(response);
            }
        } catch (IOException ignored) {
        }

        if (respLength < ONE_MiB) {
            // alles unter 1MB sind Playlisten, ORF: Trailer bei im Ausland gesperrten Filmen, ...
            // dann wars nix
            respLength = -1;
        }
        return respLength;
    }

}
