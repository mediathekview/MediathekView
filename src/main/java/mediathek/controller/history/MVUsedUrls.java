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
package mediathek.controller.history;

import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mediathek.config.Daten;
import mediathek.gui.messages.history.HistoryChangedEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.*;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.ConcurrentSkipListSet;

@SuppressWarnings("serial")
public class MVUsedUrls<T extends HistoryChangedEvent> {

    private static final Logger logger = LogManager.getLogger(MVUsedUrls.class);
    private final SimpleDateFormat SDF = new SimpleDateFormat("dd.MM.yyyy");
    private final Set<String> listeUrls = new ConcurrentSkipListSet<>();
    private final List<MVUsedUrl> listeUrlsSortDate = new LinkedList<>();
    private final Class<T> clazz;
    private Path urlPath;
    public MVUsedUrls(String fileName, String settingsDir, Class<T> clazz) {
        this.clazz = clazz;

        try {
            urlPath = Paths.get(settingsDir).resolve(fileName);
        } catch (InvalidPathException e) {
            logger.error("Path resolve failed for {},{}", settingsDir, fileName);
            urlPath = null;
        }

        listeBauen();
    }

    public List<MVUsedUrl> getListeUrlsSortDate() {
        return listeUrlsSortDate;
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

    private void sendChangeMessage() {
        try {
            final T msg = clazz.getDeclaredConstructor().newInstance();
            Daten.getInstance().getMessageBus().publishAsync(msg);
        } catch (IllegalAccessException | InstantiationException | NoSuchMethodException | InvocationTargetException e) {
            logger.error("sendChangeMessage()", e);
        }
    }

    public synchronized void alleLoeschen() {
        listeUrls.clear();
        listeUrlsSortDate.clear();

        //TODO this code is sort of useless as the file is always created here
        checkUrlFilePath();
        try {
            Files.deleteIfExists(urlPath);
        } catch (IOException ignored) {
        }

        sendChangeMessage();
    }

    public boolean urlPruefen(String urlFilm) {
        //wenn url gefunden, dann true zurück
        return listeUrls.contains(urlFilm);
    }

    public synchronized List<MVUsedUrl> getSortedList() {
        ArrayList<MVUsedUrl> ret = new ArrayList<>(listeUrlsSortDate);
        Collections.sort(ret);

        return ret;
    }

    public synchronized void urlAusLogfileLoeschen(String urlFilm) {
        //Logfile einlesen, entsprechende Zeile Filtern und dann Logfile überschreiben
        //wenn die URL im Logfiel ist, dann true zurück
        boolean gefunden = false;

        checkUrlFilePath();

        final List<String> liste = new ArrayList<>();
        try (InputStream is = Files.newInputStream(urlPath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
            String zeile;
            while ((zeile = in.readLine()) != null) {
                if (MVUsedUrl.getUrlAusZeile(zeile).getUrl().equals(urlFilm)) {
                    gefunden = true; //nur dann muss das Logfile auch geschrieben werden
                } else {
                    liste.add(zeile);
                }
            }
        } catch (Exception ex) {
            logger.error("urlAusLogfileLoeschen(String)", ex);
        }

        //und jetzt wieder schreiben, wenn nötig
        if (gefunden) {
            try (OutputStream os = Files.newOutputStream(urlPath);
                 OutputStreamWriter osw = new OutputStreamWriter(os);
                 BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
                for (String entry : liste)
                    bufferedWriter.write(entry + '\n');
            } catch (Exception ex) {
                logger.error("urlAusLogfileLoeschen(String)", ex);
            }
        }

        listeUrls.clear();
        listeUrlsSortDate.clear();

        listeBauen();

        sendChangeMessage();
    }

    public synchronized void urlAusLogfileLoeschen(ArrayList<DatenFilm> filme) {
        //Logfile einlesen, entsprechende Zeile Filtern und dann Logfile überschreiben
        //wenn die URL im Logfiel ist, dann true zurück
        String zeile;
        boolean gefunden = false, gef;

        checkUrlFilePath();

        List<String> newListe = new ArrayList<>();
        try (InputStream is = Files.newInputStream(urlPath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
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
            logger.error("urlAusLogfileLoeschen(ArrayList)", ex);
        }

        //und jetzt wieder schreiben, wenn nötig
        if (gefunden) {
            try (OutputStream os = Files.newOutputStream(urlPath);
                 OutputStreamWriter osw = new OutputStreamWriter(os);
                 BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
                for (String entry : newListe) {
                    bufferedWriter.write(entry + '\n');
                }
            } catch (Exception ex) {
                logger.error("urlAusLogfileLoeschen(ArrayList)", ex);
            }
        }

        listeUrls.clear();
        listeUrlsSortDate.clear();

        listeBauen();

        sendChangeMessage();
    }

    public synchronized void zeileSchreiben(String thema, String titel, String url) {
        String datum = SDF.format(new Date());
        listeUrls.add(url);
        listeUrlsSortDate.add(new MVUsedUrl(datum, thema, titel, url));

        checkUrlFilePath();

        try (OutputStream os = Files.newOutputStream(urlPath, StandardOpenOption.APPEND);
             OutputStreamWriter osw = new OutputStreamWriter(os);
             BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
            final MVUsedUrl usedUrl = new MVUsedUrl(datum, thema, titel, url);
            bufferedWriter.write(usedUrl.getUsedUrl());
        } catch (Exception ex) {
            logger.error("zeileSchreiben(...)", ex);
        }

        sendChangeMessage();
    }

    public synchronized void zeileSchreiben(ArrayList<DatenFilm> arrayFilms) {
        final String datum = SDF.format(new Date());

        checkUrlFilePath();

        try (OutputStream os = Files.newOutputStream(urlPath, StandardOpenOption.APPEND);
             OutputStreamWriter osw = new OutputStreamWriter(os);
             BufferedWriter bufferedWriter = new BufferedWriter(osw)) {

            for (DatenFilm film : arrayFilms) {
                // film.arr[DatenFilm.FILM_THEMA_NR], film.arr[DatenFilm.FILM_TITEL_NR], film.getUrlHistory()
                listeUrls.add(film.getUrlHistory());

                final MVUsedUrl usedUrl = new MVUsedUrl(datum, film.getThema(), film.getTitle(), film.getUrlHistory());
                listeUrlsSortDate.add(usedUrl);

                bufferedWriter.write(usedUrl.getUsedUrl());
            }
        } catch (Exception ex) {
            logger.error("zeileSchreiben(ArrayList)", ex);
        }

        sendChangeMessage();
    }

    // eigener Thread!!
    public synchronized void createLineWriterThread(LinkedList<MVUsedUrl> mvuuList) {
        Thread t = new LineWriterThread(mvuuList);
        t.start();
    }

    private void checkUrlFilePath() {
        try {
            if (Files.notExists(urlPath))
                Files.createFile(urlPath);
        } catch (IOException ex) {
            logger.error("checkUrlFilePath()", ex);
        }
    }

    private void listeBauen() {
        //LinkedList mit den URLs aus dem Logfile bauen
        checkUrlFilePath();

        try (InputStream is = Files.newInputStream(urlPath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
            String zeile;
            while ((zeile = in.readLine()) != null) {
                MVUsedUrl mvuu = MVUsedUrl.getUrlAusZeile(zeile);
                listeUrls.add(mvuu.getUrl());
                listeUrlsSortDate.add(mvuu);
            }
        } catch (Exception ex) {
            logger.error("listeBauen()", ex);
        }
    }

    class LineWriterThread extends Thread {

        private final List<MVUsedUrl> mvuuList;

        public LineWriterThread(LinkedList<MVUsedUrl> mvuuList) {
            this.mvuuList = mvuuList;
            setName(LineWriterThread.class.getName());
        }

        @Override
        public void run() {
            zeilenSchreiben();
        }

        private synchronized void zeilenSchreiben() {
            checkUrlFilePath();

            try (OutputStream os = Files.newOutputStream(urlPath, StandardOpenOption.APPEND);
                 OutputStreamWriter osw = new OutputStreamWriter(os);
                 BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
                for (MVUsedUrl mvuu : mvuuList) {
                    listeUrls.add(mvuu.getUrl());
                    listeUrlsSortDate.add(mvuu);

                    bufferedWriter.write(mvuu.getUsedUrl());
                }
            } catch (Exception ex) {
                logger.error("zeilenSchreiben()", ex);
            }
            sendChangeMessage();
        }
    }

}
