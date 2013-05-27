/*
 *  MediathekView
 *  Copyright (C) 2008 W. Xaver
 *  W.Xaver[at]googlemail.com
 *  http://zdfmediathk.sourceforge.net/
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.Proxy;
import java.net.SocketAddress;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.zip.GZIPInputStream;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;
import mediathek.daten.Daten;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

public class GetUrl {

    public static final int LISTE_SEITEN_ZAEHLER = 1;
    public static final int LISTE_SEITEN_ZAEHLER_FEHlER = 2;
    public static final int LISTE_SEITEN_ZAEHLER_FEHLERVERSUCHE = 3;
    public static final int LISTE_SEITEN_ZAEHLER_WARTEZEIT_FEHLVERSUCHE = 4;
    public static final int LISTE_SUMME_BYTE = 5;
    public static final int LISTE_LADEART = 6;
    private static final long UrlWartenBasis = 500;//ms, Basiswert zu dem dann der Faktor multipliziert wird
    private int faktorWarten = 1;
    private int timeout = 10000;
    private long wartenBasis = UrlWartenBasis;
    private static LinkedList<Seitenzaehler> listeSeitenZaehler = new LinkedList<Seitenzaehler>();
    private static LinkedList<Seitenzaehler> listeSeitenZaehlerFehler = new LinkedList<Seitenzaehler>();
    private static LinkedList<Seitenzaehler> listeSeitenZaehlerFehlerVersuche = new LinkedList<Seitenzaehler>();
    private static LinkedList<Seitenzaehler> listeSeitenZaehlerWartezeitFehlerVersuche = new LinkedList<Seitenzaehler>(); // Wartezeit für Wiederholungen [s]
    private static LinkedList<Seitenzaehler> listeSummeByte = new LinkedList<Seitenzaehler>(); // Summe Daten in Byte für jeden Sender
    private static LinkedList<Seitenzaehler> listeLadeArt = new LinkedList<Seitenzaehler>(); // Summe Daten in Byte für die verschiedenen Ladearten
    private static final int LADE_ART_UNBEKANNT = 0;
    private static final int LADE_ART_NIX = 1;
    private static final int LADE_ART_DEFLATE = 2;
    private static final int LADE_ART_GZIP = 3;

    private class Seitenzaehler {

        String senderName = "";
        long seitenAnzahl = 0;
        long ladeArtUnbekannt = 0;
        long ladeArtNix = 0;
        long ladeArtDeflate = 0;
        long ladeArtGzip = 0;

        public Seitenzaehler(String ssenderName, int sseitenAnzahl) {
            senderName = ssenderName;
            seitenAnzahl = sseitenAnzahl;
        }
    }

    public GetUrl(long wwartenBasis) {
        wartenBasis = wwartenBasis;
    }

    //===================================
    // public
    //===================================
    public StringBuffer getUri_Utf(String sender, String addr, StringBuffer seite, String meldung) {
        return getUri(sender, addr, Konstanten.KODIERUNG_UTF, 1 /* versuche */, seite, meldung);
    }

    public StringBuffer getUri_Iso(String sender, String addr, StringBuffer seite, String meldung) {
        return getUri(sender, addr, Konstanten.KODIERUNG_ISO15, 1 /* versuche */, seite, meldung);
    }

    public synchronized StringBuffer getUri(String sender, String addr, String kodierung, int maxVersuche, StringBuffer seite, String meldung) {
        final int PAUSE = 1000;
//        int aktTimeout = (maxVersuche > 1) ? timeout / 2 : timeout; // wenns mehrere Versuche gibt, dann der erste mit verkürztem Timeout
        int aktTimeout = timeout;
        int aktVer = 0;
        int wartezeit;
        boolean letzterVersuch;
        do {
            ++aktVer;
            try {
//                if (aktVer >= maxVersuche) {
//                    // der letzte Versuch mit dem vollen Timeout
//                    aktTimeout = timeout;
//                }
                if (aktVer > 1) {
                    // und noch eine Pause vor dem nächsten Versuch
                    this.wait(PAUSE);
                }
                letzterVersuch = (aktVer >= maxVersuche) ? true : false;
                seite = getUri(sender, addr, seite, kodierung, aktTimeout, meldung, maxVersuche, letzterVersuch);
                if (seite.length() > 0) {
                    // und nix wie weiter 
                    if (Daten.debug && aktVer > 1) {
                        String text = sender + " [" + aktVer + "/" + maxVersuche + "] ~~~> " + addr;
                        Log.systemMeldung(text);
                    }
                    // nur dann zählen
                    incSeitenZaehler(LISTE_SEITEN_ZAEHLER, sender, 1, LADE_ART_UNBEKANNT);
                    return seite;
                } else {
                    // hat nicht geklappt
                    if (aktVer > 1) {
                        wartezeit = (aktTimeout + PAUSE);
                    } else {
                        wartezeit = (aktTimeout);
                    }
                    incSeitenZaehler(LISTE_SEITEN_ZAEHLER_WARTEZEIT_FEHLVERSUCHE, sender, wartezeit / 1000, LADE_ART_UNBEKANNT);
                    incSeitenZaehler(LISTE_SEITEN_ZAEHLER_FEHLERVERSUCHE, sender, 1, LADE_ART_UNBEKANNT);
                    if (letzterVersuch) {
                        // dann wars leider nichts
                        incSeitenZaehler(LISTE_SEITEN_ZAEHLER_FEHlER, sender, 1, LADE_ART_UNBEKANNT);
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(698963200, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri", ex, sender);
            }
        } while (!Daten.filmeLaden.getStop() && aktVer < maxVersuche);
        return seite;
    }

    public synchronized void getDummy(String sender) {
        // Dummy zum hochzählen des Seitenzählers
        incSeitenZaehler(LISTE_SEITEN_ZAEHLER, sender, 1, LADE_ART_UNBEKANNT);
    }

    public void setTimeout(int ttimeout) {
        timeout = ttimeout;
    }

    public int getTimeout() {
        return timeout;
    }

    public static long getSeitenZaehler(int art, String sender) {
        long ret = 0;
        LinkedList<Seitenzaehler> liste = getListe(art);
        Iterator<Seitenzaehler> it = liste.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                ret = sz.seitenAnzahl;
            }
        }
        if (art == LISTE_SUMME_BYTE) {
            // Byte in MByte
            ret = ret / 1024 / 1024;
        }
        return ret;
    }

    public static synchronized long getSeitenZaehler(int art) {
        long ret = 0;
        LinkedList<Seitenzaehler> liste = getListe(art);
        Iterator<Seitenzaehler> it = liste.iterator();
        while (it.hasNext()) {
            ret += it.next().seitenAnzahl;
        }
        if (art == LISTE_SUMME_BYTE) {
            // Byte in MByte
            ret = ret / 1024 / 1024;
        }
        return ret;
    }

    public static synchronized String getSeitenZaehlerLadeArt(String sender) {
        String ret = "";
        LinkedList<Seitenzaehler> liste = getListe(LISTE_SUMME_BYTE);
        Iterator<Seitenzaehler> it = liste.iterator();
        while (it.hasNext()) {
            Seitenzaehler sz = it.next();
            if (sz.senderName.equals(sender)) {
                ret = "Unbekannt: " + (sz.ladeArtUnbekannt == 0 ? "0" : ((sz.ladeArtUnbekannt / 1024 / 1024) == 0 ? "<1" : String.valueOf(sz.ladeArtUnbekannt / 1024 / 1024)))
                        + "   Nix: " + (sz.ladeArtNix == 0 ? "0" : ((sz.ladeArtNix / 1024 / 1024) == 0 ? "<1" : String.valueOf(sz.ladeArtNix / 1024 / 1024)))
                        + "   Deflaet: " + (sz.ladeArtDeflate == 0 ? "0" : ((sz.ladeArtDeflate / 1024 / 1024) == 0 ? "<1" : String.valueOf(sz.ladeArtDeflate / 1024 / 1024)))
                        + "   Gzip: " + (sz.ladeArtGzip == 0 ? "0" : ((sz.ladeArtGzip / 1024 / 1024) == 0 ? "<1" : String.valueOf(sz.ladeArtGzip / 1024 / 1024)));
            }
        }
        return ret;
    }

    public static synchronized void resetZaehler() {
        listeSeitenZaehler.clear();
        listeSeitenZaehlerFehler.clear();
        listeSeitenZaehlerFehlerVersuche.clear();
        listeSeitenZaehlerWartezeitFehlerVersuche.clear();
        listeSummeByte.clear();
    }

    //===================================
    // private
    //===================================
    private static LinkedList<Seitenzaehler> getListe(int art) {
        switch (art) {
            case LISTE_SEITEN_ZAEHLER:
                return listeSeitenZaehler;
            case LISTE_SEITEN_ZAEHLER_FEHLERVERSUCHE:
                return listeSeitenZaehlerFehlerVersuche;
            case LISTE_SEITEN_ZAEHLER_FEHlER:
                return listeSeitenZaehlerFehler;
            case LISTE_SEITEN_ZAEHLER_WARTEZEIT_FEHLVERSUCHE:
                return listeSeitenZaehlerWartezeitFehlerVersuche;
            case LISTE_SUMME_BYTE:
                return listeSummeByte;
            default:
                return null;
        }
    }

    private synchronized void incSeitenZaehler(int art, String sender, int inc, int ladeArt) {
        boolean gefunden = false;
        LinkedList<Seitenzaehler> liste = getListe(art);
        Iterator<Seitenzaehler> it = liste.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                sz.seitenAnzahl += inc;
                addLadeArt(sz, ladeArt, inc);
                gefunden = true;
                break;
            }
        }
        if (!gefunden) {
            sz = new Seitenzaehler(sender, inc);
            addLadeArt(sz, ladeArt, inc);
            liste.add(sz);
        }
    }

    private void addLadeArt(Seitenzaehler sz, int ladeArt, int inc) {
        switch (ladeArt) {
            case LADE_ART_NIX:
                sz.ladeArtNix += inc;
                break;
            case LADE_ART_DEFLATE:
                sz.ladeArtDeflate += inc;
                break;
            case LADE_ART_GZIP:
                sz.ladeArtGzip += inc;
                break;
            default:
                sz.ladeArtUnbekannt += inc;
        }
    }

    private synchronized StringBuffer getUri(String sender, String addr, StringBuffer seite, String kodierung, int timeout, String meldung, int versuch, boolean lVersuch) {
        int timeo = timeout;
        boolean proxyB = false;
        char[] zeichen = new char[1];
        seite.setLength(0);
        HttpURLConnection conn = null;
        int code = 0;
        InputStream in = null;
        InputStreamReader inReader = null;
        Proxy proxy = null;
        SocketAddress saddr = null;
        int ladeArt = LADE_ART_UNBEKANNT;
        // immer etwas bremsen
        try {
            long w = wartenBasis * faktorWarten;
            this.wait(w);
        } catch (Exception ex) {
            Log.fehlerMeldung(976120379, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri", ex, sender);
        }
        try {
            // conn = url.openConnection(Proxy.NO_PROXY);
            conn = (HttpURLConnection) new URL(addr).openConnection();
            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            System.setProperty("http.maxConnections", "600");
            System.setProperty("http.keepAlive", "false");
            conn.setRequestProperty("Accept-Encoding", "gzip, deflate");
            if (timeout > 0) {
                conn.setReadTimeout(timeout);
                conn.setConnectTimeout(timeout);
            }
            // the encoding returned by the server
            String encoding = conn.getContentEncoding();
//            if (conn instanceof HttpURLConnection) {
//                HttpURLConnection httpConnection = (HttpURLConnection) conn;
//                code = httpConnection.getResponseCode();
//                if (code >= 400) {
//                    if (true) {
//                        // wenn möglich, einen Proxy einrichten
//                        Log.fehlerMeldung(236597125, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri", "---Proxy---");
//                        proxyB = true;
//                        saddr = new InetSocketAddress("localhost", 9050);
//                        proxy = new Proxy(Proxy.Type.SOCKS, saddr);
//                        conn = url.openConnection(proxy);
//                        conn.setRequestProperty("User-Agent", Daten.getUserAgent());
//                        if (timeout > 0) {
//                            conn.setReadTimeout(timeout);
//                            conn.setConnectTimeout(timeout);
//                        }
//                    } else {
//                        Log.fehlerMeldung(864123698, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri", "returncode >= 400");
//                    }
//                }
//            } else {
//                Log.fehlerMeldung(949697315, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri", "keine HTTPcon");
//            }
            //in = conn.getInputStream();

            if (encoding != null && encoding.equalsIgnoreCase("gzip")) {
                ladeArt = LADE_ART_GZIP;
                in = new GZIPInputStream(conn.getInputStream());
            } else if (encoding != null && encoding.equalsIgnoreCase("deflate")) {
                ladeArt = LADE_ART_DEFLATE;
                in = new InflaterInputStream(conn.getInputStream(), new Inflater(true));
            } else {
                ladeArt = LADE_ART_NIX;
                in = conn.getInputStream();
            }
            inReader = new InputStreamReader(in, kodierung);
////            while (!Daten.filmeLaden.getStop() && inReader.read(zeichen) != -1) {
////                seite.append(zeichen);
////                incSeitenZaehler(LISTE_SUMME_BYTE, sender, 1);
////            }
            char[] buff = new char[256];
            int cnt;
            while (!Daten.filmeLaden.getStop() && (cnt = inReader.read(buff)) > 0) {
                seite.append(buff, 0, cnt);
                incSeitenZaehler(LISTE_SUMME_BYTE, sender, cnt, ladeArt);
            }
        } catch (IOException ex) {
            // consume error stream, otherwise, connection won't be reused
            if (conn != null) {
                try {
                    InputStream i = conn.getErrorStream();
                    if (i != null) {
                        i.close();
                    }
                    if (inReader != null) {
                        inReader.close();
                    }
                } catch (Exception e) {
                    Log.fehlerMeldung(645105987, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri", e, "");
                }
            }
            if (lVersuch) {
                String[] text;
                if (meldung.equals("")) {
                    text = new String[]{sender + " - timout: " + timeo + " Versuche: " + versuch, addr /*, (proxyB ? "Porxy - " : "")*/};
                } else {
                    text = new String[]{sender + " - timout: " + timeo + " Versuche: " + versuch, addr, meldung/*, (proxyB ? "Porxy - " : "")*/};
                }
                if (ex.getMessage().equals("Read timed out")) {
                    Log.fehlerMeldung(502739817, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri: TimeOut", text);
                } else {
                    Log.fehlerMeldung(502739817, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri", ex, text);
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(973969801, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri", ex, "");
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(696321478, Log.FEHLER_ART_GETURL, GetUrl.class.getName() + ".getUri", ex, "");
            }
        }
        return seite;
    }
}
