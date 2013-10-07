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

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import mediathek.daten.Daten;
import mediathek.tool.DatumZeit;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class ErledigteAbos {
    private final String TRENNER = "  |###|  ";
    private final String PAUSE = " |#| ";
    private HashSet<String> listeErledigteAbos;
    private LinkedList<String> listeErledigteAbos_ = new LinkedList<String>();

    public ErledigteAbos() {
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

    public synchronized void alleLoeschen() {
        listeErledigteAbos.clear();
        try {
            Path downloadAboFilePath = Daten.getDownloadAboFilePath();
            if (Files.exists(downloadAboFilePath))
                Files.delete(downloadAboFilePath);
        } catch (IOException ignored) {
        }

        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ERLEDIGTE_ABOS, ErledigteAbos.class.getSimpleName());
    }

    public synchronized boolean urlAusLogfileLoeschen(String urlFilm) {
        //Logfile einlesen, entsprechende Zeile Filtern und dann Logfile überschreiben
        //wenn die URL im Logfiel ist, dann true zurück
        String zeile;
        boolean gefunden = false;
        LinkedList<String> liste = new LinkedList<>();

        //Use Automatic Resource Management
        try (LineNumberReader in = new LineNumberReader(new InputStreamReader(Files.newInputStream(Daten.getDownloadAboFilePath())))){
            while ((zeile = in.readLine()) != null) {
                if (getUrlAusZeile(zeile).equals(urlFilm)) {
                    gefunden = true; //nur dann muss das Logfile auch geschrieben werden
                } else
                    liste.add(zeile);
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(281006874, Log.FEHLER_ART_PROG, "LogDownload.urlAusLogfileLoeschen-1", ex);
        }

        //und jetzt wieder schreiben, wenn nötig
        if (gefunden) {
                try (OutputStreamWriter writer = new OutputStreamWriter(Files.newOutputStream(Daten.getDownloadAboFilePath()))) {
                    for (String entry : liste)
                        writer.write(entry + "\n");
                } catch (Exception ex) {
                    Log.fehlerMeldung(566277080, Log.FEHLER_ART_PROG, "LogDownload.urlAusLogfileLoeschen-3", ex);
                }
            }
            listeErledigteAbos.clear();
            listeBauen();

        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ERLEDIGTE_ABOS, ErledigteAbos.class.getSimpleName());
        return gefunden;
    }

    public synchronized boolean zeileSchreiben(String thema, String titel, String url) {
        boolean ret = false;
        String text;
        listeErledigteAbos.add(url);

        //Automatic Resource Management
        try (OutputStreamWriter writer = new OutputStreamWriter(Files.newOutputStream(Daten.getDownloadAboFilePath()))) {
            thema = GuiFunktionen.textLaenge(25, putzen(thema), false /* mitte */, false /*addVorne*/);
            titel = GuiFunktionen.textLaenge(30, putzen(titel), false /* mitte */, false /*addVorne*/);
            text = DatumZeit.getHeute_dd_MM_yyyy() + PAUSE + thema + PAUSE + titel + TRENNER + url + "\n";
            writer.write(text);

            ret = true;
        } catch (Exception ex) {
            Log.fehlerMeldung(945258023, Log.FEHLER_ART_PROG, "LogDownload.zeileSchreiben-1", ex);
        }

        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ERLEDIGTE_ABOS, ErledigteAbos.class.getSimpleName());
        return ret;
    }

    public synchronized boolean zeileSchreiben(ArrayList<String[]> list) {
        boolean ret = false;
        String text;
        String zeit = DatumZeit.getHeute_dd_MM_yyyy();
        String thema, titel, url;

        try (OutputStreamWriter writer = new OutputStreamWriter(Files.newOutputStream(Daten.getDownloadAboFilePath()))) {
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
        } catch (Exception ex) {
            ret = false;
            Log.fehlerMeldung(945258023, Log.FEHLER_ART_PROG, "LogDownload.zeileSchreiben-1", ex);
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
        Path downloadAboFilePath = Daten.getDownloadAboFilePath();
        //use Automatic Resource Management
        try (LineNumberReader in = new LineNumberReader(new InputStreamReader(Files.newInputStream(downloadAboFilePath))))
        {
            String zeile;
            while ((zeile = in.readLine()) != null)
                listeErledigteAbos.add(getUrlAusZeile(zeile));
        } catch (Exception ex) {
            //FIXME assign new error code!
            Log.fehlerMeldung(203632125, Log.FEHLER_ART_PROG, ErledigteAbos.class.getName(), ex);
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
        int a1;

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
