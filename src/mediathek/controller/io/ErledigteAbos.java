/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.tool.DatumZeit;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class ErledigteAbos {

    private DDaten ddaten;
    private final String TRENNER = "  |###|  ";
    private final String PAUSE = " |#| ";
    private HashSet<String> listeErledigteAbos;
    private LinkedList<String> listeErledigteAbos_ = new LinkedList<String>();

    public ErledigteAbos(DDaten d) {
        ddaten = d;
        listeErledigteAbos = new HashSet<String>() {
            @Override
            public boolean add(String e) {
                listeErledigteAbos_.add(e);
                return super.add(e);
            }

            @Override
            public void clear() {
                listeErledigteAbos_.clear();
                super.clear();
            }
        };
        listeBauen();
    }

    public synchronized boolean alleLoeschen() {
        boolean ret = false;
        listeErledigteAbos.clear();
        File f = new File(Daten.getBasisVerzeichnis(true) + Konstanten.LOG_DATEI_DOWNLOAD_ABOS);
        if (f != null) {
            if (f.exists()) {
                ret = f.delete();
            } else {
                ret = true;
            }
        }
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ERLEDIGTE_ABOS, ErledigteAbos.class.getSimpleName());
        return ret;
    }

    public synchronized boolean urlAusLogfileLoeschen(String urlFilm) {
        //Logfile einlesen, entsprechende Zeile Filtern und dann Logfile überschreiben
        //wenn die URL im Logfiel ist, dann true zurück
        String zeile;
        boolean gefunden = false;
        LinkedList<String> liste = new LinkedList<String>();
        LineNumberReader in = null;
        try {
            File file = new File(Daten.getBasisVerzeichnis(false) + Konstanten.LOG_DATEI_DOWNLOAD_ABOS);
            if (!file.exists()) {
                // beim Programmstart ist die Datei noch nicht da
                gefunden = false;
                return false;
            }
            in = new LineNumberReader(new InputStreamReader(new FileInputStream(file)));
            while ((zeile = in.readLine()) != null) {
                if (getUrlAusZeile(zeile).equals(urlFilm)) {
                    gefunden = true; //nur dann muss das Logfile auch geschrieben werden
                } else {
                    liste.add(zeile);
                }
            }
            in.close();
        } catch (Exception ex) {
            Log.fehlerMeldung(281006874, Log.FEHLER_ART_PROG, "LogDownload.urlAusLogfileLoeschen-1", ex);
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (Exception ex) {
            }
        }
        //und jetzt wieder schreiben, wenn nötig
        if (gefunden) {
            File f = new File(Daten.getBasisVerzeichnis(true) + Konstanten.LOG_DATEI_DOWNLOAD_ABOS);
            if (f != null) {
                OutputStreamWriter writer = null;
                try {
                    writer = new OutputStreamWriter(new FileOutputStream(f, false));
                    Iterator<String> it = liste.iterator();
                    while (it.hasNext()) {
                        writer.write(it.next() + "\n");
                    }
                    writer.close();
                } catch (Exception ex) {
                    Log.fehlerMeldung(566277080, Log.FEHLER_ART_PROG, "LogDownload.urlAusLogfileLoeschen-3", ex);
                } finally {
                    try {
                        writer.close();
                    } catch (Exception ex) {
                        Log.fehlerMeldung(256648801, Log.FEHLER_ART_PROG, "LogDownload.urlAusLogfileLoeschen-4", ex);
                    }
                }
            }
            listeErledigteAbos.clear();
            listeBauen();
        }
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ERLEDIGTE_ABOS, ErledigteAbos.class.getSimpleName());
        return gefunden;
    }

    public synchronized boolean zeileSchreiben(String thema, String titel, String url) {
        boolean ret = false;
        String text;
        listeErledigteAbos.add(url);
        File f = new File(Daten.getBasisVerzeichnis(true) + Konstanten.LOG_DATEI_DOWNLOAD_ABOS);
        if (f != null) {
            OutputStreamWriter writer = null;
            try {
                writer = new OutputStreamWriter(new FileOutputStream(f, true));
                thema = GuiFunktionen.textLaenge(25, putzen(thema), false /* mitte */, false /*addVorne*/);
                titel = GuiFunktionen.textLaenge(30, putzen(titel), false /* mitte */, false /*addVorne*/);
                text = DatumZeit.getHeute_dd_MM_yyyy() + PAUSE + thema + PAUSE + titel + TRENNER + url + "\n";
                writer.write(text);
                writer.close();
                ret = true;
            } catch (Exception ex) {
                Log.fehlerMeldung(945258023, Log.FEHLER_ART_PROG, "LogDownload.zeileSchreiben-1", ex);
            } finally {
                try {
                    writer.close();
                } catch (Exception ex) {
                }
            }
        }
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ERLEDIGTE_ABOS, ErledigteAbos.class.getSimpleName());
        return ret;
    }

    public synchronized boolean zeileSchreiben(ArrayList<String[]> list) {
        boolean ret = false;
        String text;
        String zeit = DatumZeit.getHeute_dd_MM_yyyy();
        String thema, titel, url;
        OutputStreamWriter writer = null;
        File f = new File(Daten.getBasisVerzeichnis(true) + Konstanten.LOG_DATEI_DOWNLOAD_ABOS);
        try {
            writer = new OutputStreamWriter(new FileOutputStream(f, true));
            for (String[] a : list) {
                thema = a[0];
                titel = a[1];
                url = a[2];
                listeErledigteAbos.add(url);
                thema = GuiFunktionen.textLaenge(25, putzen(thema), false /* mitte */, false /*addVorne*/);
                titel = GuiFunktionen.textLaenge(30, putzen(titel), false /* mitte */, false /*addVorne*/);
                text = zeit + PAUSE + thema + PAUSE + titel + TRENNER + url + "\n";
                writer.write(text);
                ret = true;
            }
//            writer.close();
        } catch (Exception ex) {
            ret = false;
            Log.fehlerMeldung(945258023, Log.FEHLER_ART_PROG, "LogDownload.zeileSchreiben-1", ex);
        } finally {
            try {
                writer.close();
            } catch (Exception ex) {
            }
        }
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ERLEDIGTE_ABOS, ErledigteAbos.class.getSimpleName());
        return ret;
    }

    public synchronized boolean urlPruefen(String urlFilm) {
        //wenn url gefunden, dann true zurück
        return listeErledigteAbos.contains(urlFilm);
    }

    public synchronized Object[][] getObjectData() {
        Object[][] object;
        int i = 0;
        Iterator<String> iterator = listeErledigteAbos_.iterator();
        object = new Object[listeErledigteAbos_.size()][1];
        while (iterator.hasNext()) {
            object[i][0] = iterator.next();
            ++i;
        }
        return object;
    }

    // ==============================
    // private
    // ==============================
    private void listeBauen() {
        //LinkedList mit den URLs aus dem Logfile bauen
        LineNumberReader in = null;
        File datei = null;
        try {
            datei = new File(Daten.getBasisVerzeichnis(false) + Konstanten.LOG_DATEI_DOWNLOAD_ABOS);
            if (datei.exists()) {
                in = new LineNumberReader(new InputStreamReader(new FileInputStream(datei)));
                String zeile;
                while ((zeile = in.readLine()) != null) {
                    listeErledigteAbos.add(getUrlAusZeile(zeile));
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(203632125, Log.FEHLER_ART_PROG, ErledigteAbos.class.getName(), ex);
        } finally {
            try {
                if (datei.exists()) {
                    in.close();
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(898743697, Log.FEHLER_ART_PROG, ErledigteAbos.class.getName(), ex);
            }
        }
    }

    private String putzen(String s) {
        s = s.replace("\n", "");
        s = s.replace("|", "");
        s = s.replace(TRENNER, "");
        return s;
    }

    private String getUrlAusZeile(String zeile) {
        String url = "";
        int a1 = 0;
        try {
            if (zeile.contains(TRENNER)) {
                //neues Logfile-Format
                a1 = zeile.lastIndexOf(TRENNER);
                a1 += TRENNER.length();
                url = zeile.substring(a1);
            } else {
                url = zeile;
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(398853224, Log.FEHLER_ART_PROG, "ErledigteAbos.getUrlAusZeile: " + zeile, ex);
        }
        return url;
    }
}
