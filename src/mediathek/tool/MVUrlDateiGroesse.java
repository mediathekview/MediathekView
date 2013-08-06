package mediathek.tool;

import java.net.HttpURLConnection;
import java.net.URL;

public class MVUrlDateiGroesse {

    final static int TIMEOUT = 5000; // ms

    public static String laengeString(String url) {
        // liefert die Dateigröße einer URL in GBYTE!!
        // Anzeige der Größe in GB und deshalb: Faktor 1000
        String groesseStr = "";
        long l = laenge(url);
        if (l > 1000 * 1000) {
            // größer als 1MB sonst kann ich mirs sparen
            groesseStr = String.valueOf(l / (1000 * 1000));
//            groesseStr = fuellen(4, groesseStr);
            if (groesseStr.length() >= 4) {
                groesseStr = groesseStr.substring(0, groesseStr.length() - 3) + "." + groesseStr.substring(groesseStr.length() - 3);
            }
        } else if (l > 0) {
            groesseStr = "<1";
        }
        return groesseStr;
    }

    public static long laenge(String url) {
        // liefert die Dateigröße einer URL in BYTE!
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
            //ret = conn.getContentLengthLong(); //gibts erst seit jdk 7
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
//    private static String fuellen(int anz, String s) {
//        while (s.length() < anz) {
//            s = "0" + s;
//        }
//        return s;
//    }
}
