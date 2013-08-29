package mediathek.tool;

import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.SocketAddress;
import java.net.URL;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;

public class MVUrlDateiGroesse {

    final static int TIMEOUT = 2500; // ms
    private static int anz = 0;

    public static void resetZaehler() {
        anz = 0;
    }

    public static int getZaehler() {
        return anz;
    }

    public static String laengeString(String url) {
        // liefert die Dateigröße einer URL in MB!!
        // Anzeige der Größe in MB und deshalb: Faktor 1000
        String groesseStr = "";
        long l = laenge(url);
        if (l > 1000 * 1000) {
            // größer als 1MB sonst kann ich mirs sparen
            groesseStr = String.valueOf(l / (1000 * 1000));
        } else if (l > 0) {
            groesseStr = "<1";
        }
        return groesseStr;
    }

    public static long laenge(String url) {
        // liefert die Dateigröße einer URL in BYTE!
        // oder -1
        ++anz;
        long ret = -1;
        if (!url.toLowerCase().startsWith("http")) {
            return ret;
        }
        try {
            HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();
            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            conn.setReadTimeout(TIMEOUT);
            conn.setConnectTimeout(TIMEOUT);
//            if (conn.getResponseCode() < 400) {
                //ret = conn.getContentLengthLong(); //gibts erst seit jdk 7
                ret = conn.getContentLength();
                conn.disconnect();
//            } else {
//                // ein anderer Versuch
//                try {
//                    // wenn möglich, einen Proxy einrichten
//                    Log.fehlerMeldung(316598749, Log.FEHLER_ART_GETURL, MVUrlDateiGroesse.class.getName(), "---Proxy---");
//                    SocketAddress saddr = new InetSocketAddress("localhost", 9050);
//                    Proxy proxy = new Proxy(Proxy.Type.SOCKS, saddr);
//                    conn = (HttpURLConnection) new URL(url).openConnection(proxy);
//                    conn.setRequestProperty("User-Agent", Daten.getUserAgent());
//                    conn.setReadTimeout(TIMEOUT);
//                    conn.setConnectTimeout(TIMEOUT);
//                    //ret = conn.getContentLengthLong(); //gibts erst seit jdk 7
//                    ret = conn.getContentLength();
//                    conn.disconnect();
//                } catch (Exception ex) {
//                    ret = -1;
//                    Log.fehlerMeldung(643298301, Log.FEHLER_ART_PROG, "StarterClass.StartenDownload.laenge", ex);
//                }
//            }
        } catch (Exception ex) {
            ret = -1;
            Log.fehlerMeldung(643298301, Log.FEHLER_ART_PROG, "StarterClass.StartenDownload.laenge", ex);
        }
        if (ret < 300 * 1024) {
            // alles unter 300k sind Playlisten, ...
            // dann wars nix
            ret = -1;
        }
        return ret;
    }
}
