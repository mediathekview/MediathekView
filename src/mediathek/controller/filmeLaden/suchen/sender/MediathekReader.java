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
package mediathek.controller.filmeLaden.suchen.sender;

import java.util.Iterator;
import java.util.LinkedList;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.Log;

public class MediathekReader implements Runnable {

    String nameSenderMReader = ""; // ist der Name, den der Mediathekreader hat, der ist eindeutig
    int maxThreadLaufen = 4;
    long wartenSeiteLaden = 500;//ms, Basiswert zu dem dann der Faktor multipliziert wird
    //boolean senderOn = true;
    boolean updateOn = false;
    int threads = 0;
    int max = 0;
    int progress = 0;
    int startPrio = 1; // es gibt die Werte: 0->startet sofort, 1->später und 2->zuletzt
    LinkedListUrl listeThemen = new LinkedListUrl();
    GetUrl getUrlIo;
    FilmeSuchenSender filmeSuchenSender;

    public MediathekReader(FilmeSuchenSender ssearch, String nameMreader, int ssenderMaxThread, int ssenderWartenSeiteLaden, int sstartPrio) {
        filmeSuchenSender = ssearch;
        wartenSeiteLaden = ssenderWartenSeiteLaden;
        getUrlIo = new GetUrl(ssenderWartenSeiteLaden);
        nameSenderMReader = nameMreader;
        maxThreadLaufen = ssenderMaxThread;
        startPrio = sstartPrio;
    }
    //===================================
    // public 
    //===================================

    class LinkedListUrl extends LinkedList<String[]> {

        synchronized boolean addUrl(String[] e) {
            // e[0] ist immer die URL
            if (!istInListe(this, e[0], 0)) {
                return super.add(e);
            }
            return false;
        }

        synchronized String[] getListeThemen() {
            return this.pollFirst();
        }
    };

    public int getStartPrio() {
        return startPrio;
    }

    public boolean checkNameSenderFilmliste(String name) {
        // ist der Name der in der Tabelle Filme angezeigt wird
        return nameSenderMReader.equalsIgnoreCase(name);
    }

    public String getNameSender() {
        return nameSenderMReader;
    }

    public void delSenderInAlterListe(String sender) {
        filmeSuchenSender.senderInAlteListeLoeschen(sender);
    }

    @Override
    public void run() {
        //alles laden
        try {
            updateOn = false;
            threads = 0;
            addToList();
        } catch (Exception ex) {
            Log.fehlerMeldung(-397543600, Log.FEHLER_ART_MREADER, "MediathekReader.run", ex, nameSenderMReader);
        }
    }

    void addToList() {
        //wird überschrieben, hier werden die Filme gesucht
    }

    boolean addFilm(DatenFilm film) {
        if (film.arr[DatenFilm.FILM_GROESSE_NR].isEmpty()) {
            //film.arr[DatenFilm.FILM_GROESSE_NR] = MVUrlDateiGroesse.laengeString(film.arr[DatenFilm.FILM_URL_NR]);
            film.arr[DatenFilm.FILM_GROESSE_NR] = filmeSuchenSender.listeFilmeAlt.getDateiGroesse(film.arr[DatenFilm.FILM_URL_NR], film.arr[DatenFilm.FILM_SENDER_NR]);
        }
        return filmeSuchenSender.listeFilmeNeu.addFilmVomSender(film);
    }

    DatenFilm istInFilmListe(String sender, String thema, String titel) {
        return filmeSuchenSender.listeFilmeNeu.istInFilmListe(sender, thema, titel);
    }

    boolean istInListe(LinkedList<String[]> liste, String str, int nr) {
        boolean ret = false;
        Iterator<String[]> it = liste.listIterator();
        while (it.hasNext()) {
            if (it.next()[nr].equals(str)) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    boolean istInListe(LinkedList<String> liste, String str) {
        boolean ret = false;
        Iterator<String> it = liste.listIterator();
        while (it.hasNext()) {
            if (it.next().equals(str)) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    // Meldungen
    synchronized void meldungStart() {
        max = 0;
        progress = 0;
        filmeSuchenSender.melden(nameSenderMReader, max, progress, "" /* text */);
    }

    synchronized void meldungAddMax(int mmax) {
        max += mmax;
        filmeSuchenSender.melden(nameSenderMReader, max, progress, "" /* text */);
    }

    synchronized void meldungAddThread() {
        ++threads;
        filmeSuchenSender.melden(nameSenderMReader, max, progress, "" /* text */);
    }

    synchronized void meldungProgress(String text) {
        ++progress;
        filmeSuchenSender.melden(nameSenderMReader, max, progress, text);
    }

    synchronized void meldung(String text) {
        filmeSuchenSender.melden(nameSenderMReader, max, progress, text);
    }

    synchronized void meldungThreadUndFertig() {
        --threads;
        if (threads <= 0) {
            //wird erst ausgeführt wenn alle Threads beendet sind
            filmeSuchenSender.meldenFertig(nameSenderMReader);
        } else {
            // läuft noch was
            filmeSuchenSender.melden(nameSenderMReader, max, progress, "" /* text */);
        }
    }

    String addsUrl(String pfad1, String pfad2) {
        String ret = "";
        if (pfad1 != null && pfad2 != null) {
            if (!pfad1.equals("") && !pfad2.equals("")) {
                if (pfad1.charAt(pfad1.length() - 1) == '/') {
                    ret = pfad1.substring(0, pfad1.length() - 1);
                } else {
                    ret = pfad1;
                }
                if (pfad2.charAt(0) == '/') {
                    ret += pfad2;
                } else {
                    ret += '/' + pfad2;
                }
            }
        }
        if (ret.equals("")) {
            Log.fehlerMeldung(-469872800, Log.FEHLER_ART_MREADER, "MediathekReader.addsUrl", pfad1 + " " + pfad2);
        }
        return ret;
    }

    static void listeSort(LinkedList<String[]> liste, int stelle) {
        //Stringliste alphabetisch sortieren
        GermanStringSorter sorter = GermanStringSorter.getInstance();
        if (liste != null) {
            String str1;
            String str2;
            for (int i = 1; i < liste.size(); ++i) {
                for (int k = i; k > 0; --k) {
                    str1 = liste.get(k - 1)[stelle];
                    str2 = liste.get(k)[stelle];
                    // if (str1.compareToIgnoreCase(str2) > 0) {
                    if (sorter.compare(str1, str2) > 0) {
                        liste.add(k - 1, liste.remove(k));
                    } else {
                        break;
                    }
                }
            }
        }
    }

    static long extractDuration(String dauer) {
        long dauerInSeconds = 0;
        if (dauer.isEmpty()) {
            return 0;
        }
        try {
            if (dauer.contains("min")) {
                dauer = dauer.replace("min", "").trim();
                dauerInSeconds = Long.parseLong(dauer) * 60;
            } else {
                String[] parts = dauer.split(":");
                long power = 1;
                for (int i = parts.length - 1; i >= 0; i--) {
                    dauerInSeconds += Long.parseLong(parts[i]) * power;
                    power *= 60;
                }
            }
        } catch (Exception ex) {
            return 0;
        }
        return dauerInSeconds;
    }
}
