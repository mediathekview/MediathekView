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
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;
import java.util.LinkedList;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;

public class GetUrl {

    public static final long UrlWartenBasis = 500;//ms, Basiswert zu dem dann der Faktor multipliziert wird
    private int faktorWarten = 1;
    private int timeout = 10000;
    private long wartenBasis = UrlWartenBasis;
    private static long sumByte = 0;
    private static LinkedList<Seitenzaehler> listeSeitenZaehler = new LinkedList<Seitenzaehler>();
    private static LinkedList<Seitenzaehler> listeSeitenZaehlerFehler = new LinkedList<Seitenzaehler>();
    private static LinkedList<Seitenzaehler> listeSeitenZaehlerFehlerVersuche = new LinkedList<Seitenzaehler>();
    private Exception exeption = null;

    private class Seitenzaehler {

        String senderName = "";
        int seitenAnzahl = 0;

        public Seitenzaehler(String ssenderName) {
            senderName = ssenderName;
            seitenAnzahl = 1;
        }

        public Seitenzaehler(String ssenderName, int sseitenAnzahl) {
            senderName = ssenderName;
            seitenAnzahl = sseitenAnzahl;
        }
    }

//    public GetUrl() {
//    }
    public GetUrl(int ttimeout, long wwartenBasis) {
        timeout = ttimeout;
        wartenBasis = wwartenBasis;
    }

    public GetUrl(long wwartenBasis) {
        wartenBasis = wwartenBasis;
    }

    //===================================
    // public
    //===================================
    public StringBuffer getUri_Utf(String sender, String addr, StringBuffer seite, String meldung) {
        return getUri(sender, addr, Konstanten.KODIERUNG_UTF, timeout, 1 /* versuche */, seite, meldung);
    }

    public StringBuffer getUri_Iso(String sender, String addr, StringBuffer seite, String meldung) {
        return getUri(sender, addr, Konstanten.KODIERUNG_ISO15, timeout, 1 /* versuche */, seite, meldung);
    }

    public StringBuffer getUri(String sender, String addr, String kodierung, int ttimeout, int versuche, StringBuffer seite, String meldung) {
        int timeo = ttimeout;
        for (int i = 0; i < versuche; ++i) {
            if (Daten.filmeLaden.getStop()) {
                break;
            }
            // wäre doch gelacht, wenns nicht irgendwann geht!
            seite = getUri(sender, addr, seite, kodierung, timeo, meldung);
            if (seite.length() == 0) {
                // Timeout um 5 Sekunden verlängern
                timeo += 5000;
            } else {
                break;
            }
        }
        if (exeption != null) {
            incSeitenZaehlerFehler(sender);
            incSeitenZaehlerFehlerVersuche(sender, versuche);
            Log.fehlerMeldung(894120069, "GetUrl.getUri, Versuche: " + versuche + ", ", exeption, addr);
            exeption = null;
        }
        return seite;
    }

    public void setTimeout(int ttimeout) {
        timeout = ttimeout;
    }

    public int getTimeout() {
        return timeout;
    }

    public static long getSumByte() {
        return sumByte;
    }

    public static int getSeitenZaehler(String sender) {
        Iterator<Seitenzaehler> it = listeSeitenZaehler.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                return sz.seitenAnzahl;
            }
        }
        return 0;
    }

    public static synchronized int getSeitenZaehler() {
        int ret = 0;
        Iterator<Seitenzaehler> it = listeSeitenZaehler.iterator();
        while (it.hasNext()) {
            ret += it.next().seitenAnzahl;
        }
        return ret;
    }

    public static int getSeitenZaehlerFehler(String sender) {
        Iterator<Seitenzaehler> it = listeSeitenZaehlerFehler.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                return sz.seitenAnzahl;
            }
        }
        return 0;
    }

    public static int getSeitenZaehlerFehlerVersuche(String sender) {
        Iterator<Seitenzaehler> it = listeSeitenZaehlerFehlerVersuche.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                return sz.seitenAnzahl;
            }
        }
        return 0;
    }

    public static synchronized void resetZaehler() {
        listeSeitenZaehler.clear();
        listeSeitenZaehlerFehler.clear();
        listeSeitenZaehlerFehlerVersuche.clear();
        sumByte = 0;
    }

    //===================================
    // private
    //===================================
    private synchronized void incSeitenZaehler(String sender) {
        boolean gefunden = false;
        Iterator<Seitenzaehler> it = listeSeitenZaehler.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                ++sz.seitenAnzahl;
                gefunden = true;
                break;
            }
        }
        if (!gefunden) {
            listeSeitenZaehler.add(new Seitenzaehler(sender));
        }
    }

    private synchronized void incSeitenZaehlerFehler(String sender) {
        boolean gefunden = false;
        Iterator<Seitenzaehler> it = listeSeitenZaehlerFehler.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                ++sz.seitenAnzahl;
                gefunden = true;
                break;
            }
        }
        if (!gefunden) {
            listeSeitenZaehlerFehler.add(new Seitenzaehler(sender));
        }
    }

    private synchronized void incSeitenZaehlerFehlerVersuche(String sender, int v) {
        boolean gefunden = false;
        Iterator<Seitenzaehler> it = listeSeitenZaehlerFehlerVersuche.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                sz.seitenAnzahl += v;
                gefunden = true;
                break;
            }
        }
        if (!gefunden) {
            listeSeitenZaehlerFehlerVersuche.add(new Seitenzaehler(sender, v));
        }
    }

    private synchronized StringBuffer getUri(String sender, String addr, StringBuffer seite, String kodierung, int timeout, String meldung) {
        exeption = null;
        char[] zeichen = new char[1];
        try {
            long w = wartenBasis * faktorWarten;
            this.wait(w);
        } catch (Exception ex) {
            Log.fehlerMeldung(462800147, "GetUrl.getUri", ex);
        }
        seite.setLength(0);
        URLConnection conn;
        InputStream in = null;
        InputStreamReader inReader = null;
        try {
            URL url = new URL(addr);
            conn = url.openConnection();
            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            if (timeout > 0) {
                conn.setReadTimeout(timeout);
                conn.setConnectTimeout(timeout);
            }
            in = conn.getInputStream();
            inReader = new InputStreamReader(in, kodierung);
            while (!Daten.filmeLaden.getStop() && inReader.read(zeichen) != -1) {
                seite.append(zeichen);
                ++sumByte;
            }
            // nur dann zählen
            incSeitenZaehler(sender);
        } catch (Exception ex) {
            if (!meldung.equals("")) {
                Log.fehlerMeldung(642069083, "GetUrl.getUri für: ", meldung);
            }
            exeption = ex;
        } finally {
            try {
                if (in != null) {
                    inReader.close();
                }
            } catch (IOException ex) {
            }
        }
        return seite;
    }
}
