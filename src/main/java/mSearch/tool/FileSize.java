package mSearch.tool;

import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

import java.io.IOException;

public class FileSize {
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
     * Return the size of a URL in bytes.
     *
     * @param url URL as String to query.
     * @return size in bytes or -1.
     */
    private static long getFileSizeFromUrl(String url) {
        if (!url.toLowerCase().startsWith("http")) {
            return -1;
        }

        final Request request = new Request.Builder().url(url).get().build();
        long respLength = -1;
        try (Response response = MVHttpClient.getInstance().getReducedTimeOutClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (body != null && response.isSuccessful())
                respLength = body.contentLength();
        } catch (IOException ignored) {
            respLength = -1;
        }

        if (respLength < 1_000_000) {
            // alles unter 1MB sind Playlisten, ORF: Trailer bei im Ausland gesperrten Filmen, ...
            // dann wars nix
            respLength = -1;
        }
        return respLength;
    }

}
