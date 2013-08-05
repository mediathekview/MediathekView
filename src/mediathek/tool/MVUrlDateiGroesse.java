package mediathek.tool;

import java.net.HttpURLConnection;
import java.net.URL;

public class MVUrlDateiGroesse {

    final static int TIMEOUT = 5000; // ms

    public static long laenge(String url) {
        // liefert die Dateigröße eine URL
        // oder -1
        long ret = -1;
        if (!url.toLowerCase().startsWith("http")) {
            return ret;
        }
        try {
            HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();
            //conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            //conn.setRequestProperty("Accept-Encoding", "gzip, deflate");
            conn.setReadTimeout(TIMEOUT);
            conn.setConnectTimeout(TIMEOUT);
            //ret = conn.getContentLengthLong(); gibts erst seit jdk 7
            ret = conn.getContentLength();
            conn.disconnect();
        } catch (Exception ex) {
            ret = -1;
            Log.fehlerMeldung(643298301, Log.FEHLER_ART_PROG, "StarterClass.StartenDownload.laenge", ex);
        }
        if (ret < 100) {
            // dann wars nix
            ret = -1;
        }
        return ret;
    }
}
