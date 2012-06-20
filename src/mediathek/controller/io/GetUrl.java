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
    private static LinkedList<Seitenzaehler> listeSeitenZaehlerWartezeitFehlerVersuche = new LinkedList<Seitenzaehler>(); // Wartezeit für Wiederholungen [s]

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
        int aktTimeout = (maxVersuche > 1) ? timeout / 2 : timeout; // wenns mehrere Versuche gibt, dann der erste mit verkürztem Timeout
//        int aktTimeout = timeout;
        int aktVer = 0;
        int wartezeit;
        boolean letzterVersuch;
        do {
            ++aktVer;
            try {
                if (aktVer >= maxVersuche) {
                    // der letzte Versuch mit dem vollen Timeout
                    aktTimeout = timeout;
                }
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
                    incSeitenZaehler(sender);
                    return seite;
                } else {
                    if (aktVer > 1) {
                        wartezeit = (aktTimeout + PAUSE);
                    } else {
                        wartezeit = (aktTimeout);
                    }
                    incSeitenZaehlerWartezeitFehlerVersuche(sender, wartezeit / 1000);
                    if (letzterVersuch) {
                        // dann wars leider nichts
                        incSeitenZaehlerFehler(sender);
                        incSeitenZaehlerFehlerVersuche(sender, maxVersuche);
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldungGetUrl(698963200, ex, sender, new String[]{"GetUrl.getUri"});
            }
        } while (!Daten.filmeLaden.getStop() && aktVer < maxVersuche);
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

    public static int getSeitenZaehlerWartezeitFehlerVersuche(String sender) {
        Iterator<Seitenzaehler> it = listeSeitenZaehlerWartezeitFehlerVersuche.iterator();
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
        listeSeitenZaehlerWartezeitFehlerVersuche.clear();
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

    private synchronized void incSeitenZaehlerFehlerVersuche(String sender, int versuche) {
        boolean gefunden = false;
        Iterator<Seitenzaehler> it = listeSeitenZaehlerFehlerVersuche.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                sz.seitenAnzahl += versuche;
                gefunden = true;
                break;
            }
        }
        if (!gefunden) {
            listeSeitenZaehlerFehlerVersuche.add(new Seitenzaehler(sender, versuche));
        }
    }

    private synchronized void incSeitenZaehlerWartezeitFehlerVersuche(String sender, int versuche) {
        boolean gefunden = false;
        Iterator<Seitenzaehler> it = listeSeitenZaehlerWartezeitFehlerVersuche.iterator();
        Seitenzaehler sz;
        while (it.hasNext()) {
            sz = it.next();
            if (sz.senderName.equals(sender)) {
                sz.seitenAnzahl += versuche;
                gefunden = true;
                break;
            }
        }
        if (!gefunden) {
            listeSeitenZaehlerWartezeitFehlerVersuche.add(new Seitenzaehler(sender, versuche));
        }
    }

    private synchronized StringBuffer getUri(String sender, String addr, StringBuffer seite, String kodierung, int timeout, String meldung, int versuch, boolean lVersuch) {
        int timeo = timeout;
        char[] zeichen = new char[1];
        seite.setLength(0);
        URLConnection conn;
        InputStream in = null;
        InputStreamReader inReader = null;
        // immer etwas bremsen
        try {
            long w = wartenBasis * faktorWarten;
            this.wait(w);
        } catch (Exception ex) {
            Log.fehlerMeldungGetUrl(462800147, ex, sender, new String[]{"GetUrl.getUri"});
        }
        // ab hier Laden
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
        } catch (Exception ex) {
            if (lVersuch) {
                String[] text;
                if (meldung.equals("")) {
                    text = new String[]{"timout: " + timeo + " Versuche: " + versuch, addr};
                } else {
                    text = new String[]{"timout: " + timeo + " Versuche: " + versuch, addr, meldung};
                }
                Log.fehlerMeldungGetUrl(894120069, ex, sender, text);
            }
        } finally {
            try {
                if (in != null) {
                    inReader.close();
                }
            } catch (IOException ex) {
            }
        }
        // ende Laden
        // -----------------------------------------------------------
        // alle Ladeversuche sind durch
        // ####################################################
        return seite;
    }
}
