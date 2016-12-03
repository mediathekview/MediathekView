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
package mediathek.controller;

import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.tool.Listener;
import mSearch.tool.Log;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.text.SimpleDateFormat;
import java.util.*;

@SuppressWarnings("serial")
public class MVUsedUrls {

    private final HashSet<String> listeUrls;
    private final LinkedList<MVUsedUrl> listeUrlsSortDate;
    private final String fileName;
    private final String settingsDir;
    private final int notifyEvent;

    public MVUsedUrls(String fileName, String settingsDir, int notifyEvent) {
        this.fileName = fileName;
        this.settingsDir = settingsDir;
        this.notifyEvent = notifyEvent;
        listeUrlsSortDate = new LinkedList<>();
        listeUrls = new HashSet<String>() {
            @Override
            public void clear() {
                listeUrlsSortDate.clear();
                super.clear();
            }
        };
        listeBauen();
    }

    public synchronized void setGesehen(boolean gesehen, ArrayList<DatenFilm> arrayFilms, ListeFilme listeFilmeHistory) {
        if (arrayFilms.isEmpty()) {
            return;
        }
        if (!gesehen) {
            urlAusLogfileLoeschen(arrayFilms);
            arrayFilms.forEach(listeFilmeHistory::remove);
        } else {
            ArrayList<DatenFilm> neueFilme = new ArrayList<>();
            arrayFilms.stream().filter(film -> !urlPruefen(film.getUrlHistory()))
                    .forEach(film -> {
                        neueFilme.add(film);
                        listeFilmeHistory.add(film);
                    });
            zeileSchreiben(neueFilme);
        }
    }

    public synchronized void alleLoeschen() {
        listeUrls.clear();
        Path urlPath = getUrlFilePath();
        try {
            Files.deleteIfExists(urlPath);
        } catch (IOException ignored) {
        }

        Listener.notify(notifyEvent, MVUsedUrls.class.getSimpleName());
    }

    public synchronized boolean urlPruefen(String urlFilm) {
        //wenn url gefunden, dann true zurück
        return listeUrls.contains(urlFilm);
    }

    public synchronized Object[][] getObjectData() {
        int i = 0;
        Iterator<MVUsedUrl> iterator = listeUrlsSortDate.iterator();
        final Object[][] object = new Object[listeUrlsSortDate.size()][];
        while (iterator.hasNext()) {
            object[i] = iterator.next().uUrl;
            ++i;
        }
        return object;
    }

    public synchronized LinkedList<MVUsedUrl> getSortList() {
        LinkedList<MVUsedUrl> ret = new LinkedList<>();
        ret.addAll(listeUrlsSortDate);
        Collections.sort(ret);

        return ret;
    }

    public synchronized boolean urlAusLogfileLoeschen(String urlFilm) {
        //Logfile einlesen, entsprechende Zeile Filtern und dann Logfile überschreiben
        //wenn die URL im Logfiel ist, dann true zurück
        String zeile;
        boolean gefunden = false;
        LinkedList<String> liste = new LinkedList<>();

        //Use Automatic Resource Management
        final Path urlPath = getUrlFilePath();
        if (Files.notExists(urlPath)) {
            return false;
        }

        try (LineNumberReader in = new LineNumberReader(new InputStreamReader(Files.newInputStream(urlPath)))) {
            while ((zeile = in.readLine()) != null) {
                if (MVUsedUrl.getUrlAusZeile(zeile).getUrl().equals(urlFilm)) {
                    gefunden = true; //nur dann muss das Logfile auch geschrieben werden
                } else {
                    liste.add(zeile);
                }
            }
        } catch (Exception ex) {
            Log.errorLog(281006874, ex);
        }

        //und jetzt wieder schreiben, wenn nötig
        if (gefunden) {
            try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(getUrlFilePath())))) {
                for (String entry : liste) {
                    bufferedWriter.write(entry + "\n");
                }
            } catch (Exception ex) {
                Log.errorLog(566277080, ex);
            }
        }
        listeUrls.clear();
        listeBauen();

        Listener.notify(notifyEvent, MVUsedUrls.class.getSimpleName());
        return gefunden;
    }

    public synchronized boolean urlAusLogfileLoeschen(ArrayList<DatenFilm> filme) {
        //Logfile einlesen, entsprechende Zeile Filtern und dann Logfile überschreiben
        //wenn die URL im Logfiel ist, dann true zurück
        String zeile;
        boolean gefunden = false, gef;
        LinkedList<String> newListe = new LinkedList<>();

        //Use Automatic Resource Management
        final Path urlPath = getUrlFilePath();
        if (Files.notExists(urlPath)) {
            return false;
        }

        try (LineNumberReader in = new LineNumberReader(new InputStreamReader(Files.newInputStream(urlPath)))) {
            while ((zeile = in.readLine()) != null) {
                gef = false;
                String url = MVUsedUrl.getUrlAusZeile(zeile).getUrl();

                for (DatenFilm film : filme) {
                    if (url.equals(film.getUrlHistory())) {
                        gefunden = true; //nur dann muss das Logfile auch geschrieben werden
                        gef = true; // und die Zeile wird verworfen
                        break;
                    }
                }
                if (!gef) {
                    newListe.add(zeile);
                }

            }
        } catch (Exception ex) {
            Log.errorLog(401020398, ex);
        }

        //und jetzt wieder schreiben, wenn nötig
        if (gefunden) {
            try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(getUrlFilePath())))) {
                for (String entry : newListe) {
                    bufferedWriter.write(entry + "\n");
                }
            } catch (Exception ex) {
                Log.errorLog(784512067, ex);
            }
        }
        listeUrls.clear();
        listeBauen();

        Listener.notify(notifyEvent, MVUsedUrls.class.getSimpleName());
        return gefunden;
    }

    public synchronized boolean zeileSchreiben(String thema, String titel, String url) {
        boolean ret = false;
        String text;
        String datum = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
        listeUrls.add(url);
        listeUrlsSortDate.add(new MVUsedUrl(datum, thema, titel, url));

        //Automatic Resource Management
        try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(getUrlFilePath(), StandardOpenOption.APPEND)))) {
            text = MVUsedUrl.getUsedUrl(datum, thema, titel, url);
            bufferedWriter.write(text);
            ret = true;
        } catch (Exception ex) {
            Log.errorLog(945258023, ex);
        }

        Listener.notify(notifyEvent, MVUsedUrls.class.getSimpleName());
        return ret;
    }

    public synchronized boolean zeileSchreiben(ArrayList<DatenFilm> arrayFilms) {
        boolean ret = false;
        String text;
        String datum = new SimpleDateFormat("dd.MM.yyyy").format(new Date());

        try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(getUrlFilePath(), StandardOpenOption.APPEND)))) {

            for (DatenFilm film : arrayFilms) {
                // film.arr[DatenFilm.FILM_THEMA_NR], film.arr[DatenFilm.FILM_TITEL_NR], film.getUrlHistory()
                listeUrls.add(film.getUrlHistory());
                listeUrlsSortDate.add(new MVUsedUrl(datum, film.arr[DatenFilm.FILM_THEMA], film.arr[DatenFilm.FILM_TITEL], film.getUrlHistory()));
                text = MVUsedUrl.getUsedUrl(datum, film.arr[DatenFilm.FILM_THEMA], film.arr[DatenFilm.FILM_TITEL], film.getUrlHistory());
                bufferedWriter.write(text);
            }

            //Automatic Resource Management
            ret = true;
        } catch (Exception ex) {
            Log.errorLog(420312459, ex);
        }

        Listener.notify(notifyEvent, MVUsedUrls.class.getSimpleName());
        return ret;
    }

    // eigener Thread!!
    public synchronized void zeilenSchreiben(LinkedList<MVUsedUrl> mvuuList) {
        new Thread(new zeilenSchreiben_(mvuuList)).start();
    }

    private class zeilenSchreiben_ implements Runnable {

        LinkedList<MVUsedUrl> mvuuList;

        public zeilenSchreiben_(LinkedList<MVUsedUrl> mvuuList) {
            this.mvuuList = mvuuList;
        }

        @Override
        public synchronized void run() {
            zeilenSchreiben(mvuuList);
        }

        private synchronized boolean zeilenSchreiben(LinkedList<MVUsedUrl> mvuuList) {

            boolean ret = false;
            String text;
            try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(getUrlFilePath(), StandardOpenOption.APPEND)))) {
                for (MVUsedUrl mvuu : mvuuList) {
                    listeUrls.add(mvuu.getUrl());
                    listeUrlsSortDate.add(mvuu);
                    text = mvuu.getUsedUrl();
                    bufferedWriter.write(text);
                    ret = true;
                }
            } catch (Exception ex) {
                ret = false;
                Log.errorLog(945258023, ex);
            }
            Listener.notify(notifyEvent, MVUsedUrls.class.getSimpleName());
            return ret;
        }
    }

    // ==============================
    // private
    // ==============================
    private Path getUrlFilePath() {
        Path urlPath = null;
        try {
            urlPath = Paths.get(settingsDir).resolve(fileName);
            if (Files.notExists(urlPath)) {
                urlPath = Files.createFile(urlPath);
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return urlPath;
    }

    private void listeBauen() {
        //LinkedList mit den URLs aus dem Logfile bauen
        Path urlPath = getUrlFilePath();
        //use Automatic Resource Management
        try (LineNumberReader in = new LineNumberReader(new InputStreamReader(Files.newInputStream(urlPath)))) {
            String zeile;
            while ((zeile = in.readLine()) != null) {
                MVUsedUrl mvuu = MVUsedUrl.getUrlAusZeile(zeile);
                listeUrls.add(mvuu.getUrl());
                listeUrlsSortDate.add(mvuu);
            }
        } catch (Exception ex) {
            Log.errorLog(926362547, ex);
        }
    }

}
