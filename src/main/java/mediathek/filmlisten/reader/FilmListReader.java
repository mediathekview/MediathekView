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
package mediathek.filmlisten.reader;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import javafx.application.Platform;
import javafx.collections.ObservableList;
import mSearch.Config;
import mSearch.Const;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.InputStreamProgressMonitor;
import mSearch.tool.MVHttpClient;
import mSearch.tool.ProgressMonitorInputStream;
import mediathek.config.Daten;
import mediathek.daten.LiveStreamItem;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.commons.lang3.time.FastDateFormat;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.tukaani.xz.XZInputStream;

import javax.swing.event.EventListenerList;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Paths;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class FilmListReader implements AutoCloseable {
    private static final int PROGRESS_MAX = 100;
    /**
     * Memory limit for the xz decompressor. No limit by default.
     */
    protected int DECOMPRESSOR_MEMORY_LIMIT = -1;
    private final EventListenerList listeners = new EventListenerList();
    private final ListenerFilmeLadenEvent progressEvent = new ListenerFilmeLadenEvent("", "Download", 0, 0, 0, false);
    private final int max;
    private int progress = 0;
    private long milliseconds = 0;
    private String sender = "";
    private String thema = "";

    public FilmListReader() {
        max = PROGRESS_MAX;
    }

    public void addAdListener(ListenerFilmeLaden listener) {
        listeners.add(ListenerFilmeLaden.class, listener);
    }

    /**
     * Remove all registered listeners when we do not need them anymore.
     */
    private void removeRegisteredListeners() {
        ListenerFilmeLaden list[] = listeners.getListeners(ListenerFilmeLaden.class);
        for (ListenerFilmeLaden lst : list) {
            listeners.remove(ListenerFilmeLaden.class, lst);
        }
    }

    private InputStream selectDecompressor(String source, InputStream in) throws Exception {
        final InputStream is;

        switch (source.substring(source.lastIndexOf('.'))) {
            case Const.FORMAT_XZ:
                is = new XZInputStream(in, DECOMPRESSOR_MEMORY_LIMIT, false);
                break;

            case ".json":
                is = in;
                break;

            default:
                throw new UnsupportedOperationException("Unbekanntes Dateiformat entdeckt.");
        }

        return is;
    }

    private void parseNeu(JsonParser jp, DatenFilm datenFilm) throws IOException {
        final String value = jp.nextTextValue();
        datenFilm.setNew(Boolean.parseBoolean(value));
    }

    protected void parseWebsiteLink(JsonParser jp, DatenFilm datenFilm) throws IOException {
        final String value = jp.nextTextValue();
        if (value != null && !value.isEmpty()) {
            datenFilm.setWebsiteLink(value);
        }
    }

    private void parseDescription(JsonParser jp, DatenFilm datenFilm) throws IOException {
        final String value = jp.nextTextValue();
        if (value != null && !value.isEmpty())
            datenFilm.setDescription(value);
    }

    protected void parseGeo(JsonParser jp, DatenFilm datenFilm) throws IOException {
        datenFilm.arr[DatenFilm.FILM_GEO] = checkedString(jp);
    }

    private void parseSender(JsonParser jp, DatenFilm datenFilm) throws IOException {
        String parsedSender = checkedString(jp);
        if (parsedSender.isEmpty())
            datenFilm.setSender(sender);
        else {
            datenFilm.setSender(parsedSender.intern());
            //store for future reads
            sender = parsedSender;
        }
    }

    private void parseThema(JsonParser jp, DatenFilm datenFilm) throws IOException {
        String value = checkedString(jp);
        if (value.isEmpty())
            datenFilm.arr[DatenFilm.FILM_THEMA] = thema;
        else {
            datenFilm.arr[DatenFilm.FILM_THEMA] = value;
            thema = datenFilm.arr[DatenFilm.FILM_THEMA];
        }
    }

    private String checkedString(JsonParser jp) throws IOException {
        String value = jp.nextTextValue();
        //only check for null and replace for the default rows...
        if (value == null)
            value = "";

        return value;
    }

    private void parseMetaData(JsonParser jp, ListeFilme listeFilme) throws IOException {
        JsonToken jsonToken;
        while ((jsonToken = jp.nextToken()) != null) {
            if (jsonToken == JsonToken.END_OBJECT) {
                break;
            }
            if (jp.isExpectedStartArrayToken()) {
                for (int k = 0; k < ListeFilme.MAX_ELEM; ++k) {
                    listeFilme.metaDaten[k] = jp.nextTextValue();
                }
                break;
            }
        }
    }

    private void skipFieldDescriptions(JsonParser jp) throws IOException {
        JsonToken jsonToken;
        while ((jsonToken = jp.nextToken()) != null) {
            if (jsonToken == JsonToken.END_OBJECT) {
                break;
            }
            if (jp.isExpectedStartArrayToken()) {
                // sind nur die Feldbeschreibungen, brauch mer nicht
                jp.nextToken();
                break;
            }
        }
    }

    private void parseDefault(JsonParser jp, DatenFilm datenFilm, final int TAG) throws IOException {
        datenFilm.arr[TAG] = checkedString(jp);
    }

    private void parseGroesse(JsonParser jp, DatenFilm datenFilm) throws IOException {
        String value = checkedString(jp);
        datenFilm.arr[DatenFilm.FILM_GROESSE] = value.intern();
    }

    /**
     * Skip over file entry.
     * This is used when fields were deleted in DatenFilm but still exit in filmlist file.
     */
    private void skipToken(JsonParser jp) throws IOException {
        jp.nextToken();
    }

    private void parseTime(JsonParser jp, DatenFilm datenFilm) throws IOException {
        String zeit = checkedString(jp);
        if (!zeit.isEmpty() && zeit.length() < 8) {
            zeit += ":00"; // add seconds
        }
        datenFilm.arr[DatenFilm.FILM_ZEIT] = zeit;
    }

    private void parseTitel(JsonParser jp, DatenFilm datenFilm) throws IOException {
        datenFilm.setTitle(checkedString(jp));
    }

    private void readData(JsonParser jp, ListeFilme listeFilme) throws IOException {
        JsonToken jsonToken;

        if (jp.nextToken() != JsonToken.START_OBJECT) {
            throw new IllegalStateException("Expected data to start with an Object");
        }

        parseMetaData(jp, listeFilme);

        skipFieldDescriptions(jp);

        long start = System.nanoTime();

        while (!Config.getStop() && (jsonToken = jp.nextToken()) != null) {
            if (jsonToken == JsonToken.END_OBJECT) {
                break;
            }
            if (jp.isExpectedStartArrayToken()) {
                DatenFilm datenFilm = new DatenFilm();
                parseSender(jp, datenFilm);
                parseThema(jp, datenFilm);
                parseTitel(jp, datenFilm);
                parseDefault(jp, datenFilm, DatenFilm.FILM_DATUM);
                parseTime(jp, datenFilm);
                parseDefault(jp, datenFilm, DatenFilm.FILM_DAUER);
                parseGroesse(jp, datenFilm);
                parseDescription(jp, datenFilm);
                parseDefault(jp, datenFilm, DatenFilm.FILM_URL);
                parseWebsiteLink(jp, datenFilm);
                parseDefault(jp, datenFilm, DatenFilm.FILM_URL_SUBTITLE);
                skipToken(jp);
                parseDefault(jp, datenFilm, DatenFilm.FILM_URL_KLEIN);
                skipToken(jp);
                parseDefault(jp, datenFilm, DatenFilm.FILM_URL_HD);
                skipToken(jp);
                parseDefault(jp, datenFilm, DatenFilm.FILM_DATUM_LONG);
                parseDefault(jp, datenFilm, DatenFilm.FILM_URL_HISTORY);
                parseGeo(jp, datenFilm);
                parseNeu(jp, datenFilm);

                listeFilme.importFilmliste(datenFilm);

                if (milliseconds > 0) {
                    // muss "rückwärts" laufen, da das Datum sonst 2x gebaut werden muss
                    // wenns drin bleibt, kann mans noch ändern
                    if (!checkDate(datenFilm)) {
                        listeFilme.remove(datenFilm);
                    }
                }
            }
        }
        long end = System.nanoTime();
        logger.debug("Reading filmlist took {} msecs.", TimeUnit.MILLISECONDS.convert(end - start, TimeUnit.NANOSECONDS));
    }

    private void checkDays(long days) {
        if (days > 0) {
            milliseconds = System.currentTimeMillis() - TimeUnit.MILLISECONDS.convert(days, TimeUnit.DAYS);
        } else {
            milliseconds = 0;
        }
    }

    public void readFilmListe(String source, final ListeFilme listeFilme, int days) {
        try {
            logger.info("Liste Filme lesen von: {}", source);
            listeFilme.clear();

            notifyStart(source); // für die Progressanzeige

            checkDays(days);

            if (source.startsWith("http")) {
                final URL sourceUrl = new URL(source);
                processFromWeb(sourceUrl, listeFilme);
            } else
                processFromFile(source, listeFilme);

            if (Config.getStop()) {
                logger.info("--> Abbruch");
                listeFilme.clear();
            }

        } catch (MalformedURLException ex) {
            logger.warn(ex);
        }

        extractLivestreams(listeFilme);

        notifyFertig(source, listeFilme);
    }

    private void extractLivestreams(ListeFilme listeFilme) {
        final Daten daten = Daten.getInstance();
        ObservableList<LiveStreamItem> liveStreamList = daten.getLivestreamList();

        Platform.runLater(liveStreamList::clear);

        List<DatenFilm> tempFilteredFilmList = listeFilme.parallelStream()
                .filter(film -> film.getThema().equalsIgnoreCase(ListeFilme.THEMA_LIVE))
                .collect(Collectors.toList());

        logger.debug("ListeFilme: {}", listeFilme.size());
        for (DatenFilm film : tempFilteredFilmList) {
            try {
                LiveStreamItem item = new LiveStreamItem();
                item.setSender(film.getSender());
                item.setTitel(film.getTitle());
                item.setUrl(new URL(film.getUrl()));
                Platform.runLater(() -> liveStreamList.add(item));

                //TODO wieder aktivieren nachdem speichern und lesen der Livestreams implementiert wurde
                //listeFilme.remove(film);
            } catch (MalformedURLException ex) {
                logger.error(ex);
            }
        }

        tempFilteredFilmList.clear();
        logger.debug("ListeFilme: {}", listeFilme.size());
    }

    /**
     * Read a locally available filmlist.
     *
     * @param source     file path as string
     * @param listeFilme the list to read to
     */
    private void processFromFile(String source, ListeFilme listeFilme) {
        InputStreamProgressMonitor monitor = new InputStreamProgressMonitor() {
            private final String sourceString = source;
            private int oldProgress = 0;

            @Override
            public void progress(final long bytesRead, final long size) {
                final int iProgress = (int) (bytesRead * 100 / size);
                if (iProgress != oldProgress) {
                    oldProgress = iProgress;
                    notifyProgress(sourceString, iProgress);
                }
            }
        };

        try {
            final long fileSize = Files.size(Paths.get(source));

            try (FileInputStream fis = new FileInputStream(source);
                 InputStream input = new ProgressMonitorInputStream(fis, fileSize, monitor);
                 InputStream in = selectDecompressor(source, input);
                 JsonParser jp = new JsonFactory().createParser(in)) {
                readData(jp, listeFilme);
            }
        } catch (FileNotFoundException | NoSuchFileException ex) {
            logger.debug("FilmListe existiert nicht: {}", source);
            listeFilme.clear();
        } catch (Exception ex) {
            logger.error("FilmListe: {}", source, ex);
            listeFilme.clear();
        }
    }

    private static final Logger logger = LogManager.getLogger(FilmListReader.class);

    /**
     * Download and process a filmliste from the web.
     *
     * @param source     source url as string
     * @param listeFilme the list to read to
     */
    private void processFromWeb(URL source, ListeFilme listeFilme) {
        //our progress monitor callback
        InputStreamProgressMonitor monitor = new InputStreamProgressMonitor() {
            private final String sourceString = source.toString();
            private int oldProgress = 0;

            @Override
            public void progress(final long bytesRead, final long size) {
                final int iProgress = (int) (bytesRead * 100 / size);
                if (iProgress != oldProgress) {
                    oldProgress = iProgress;
                    notifyProgress(sourceString, iProgress);
                }
            }
        };

        final Request.Builder builder = new Request.Builder().url(source);
        try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(builder.build()).execute()) {
            if (response.isSuccessful()) {
                try (ResponseBody body = response.body();
                     InputStream input = new ProgressMonitorInputStream(body.byteStream(), body.contentLength(), monitor);
                     InputStream is = new XZInputStream(input);
                     JsonParser jp = new JsonFactory().createParser(is)) {
                    readData(jp, listeFilme);
                }
            }
        } catch (Exception ex) {
            logger.error("FilmListe: {}", source, ex);
            listeFilme.clear();
        }
    }

    private boolean checkDate(DatenFilm film) {
        // true wenn der Film angezeigt werden kann!
        try {
            if (film.datumFilm.getTime() != 0) {
                if (film.datumFilm.getTime() < milliseconds) {
                    return false;
                }
            }
        } catch (Exception ex) {
            logger.error(ex);
        }
        return true;
    }

    private void notifyStart(String url) {
        progress = 0;
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.start(new ListenerFilmeLadenEvent(url, "", max, 0, 0, false));
        }
    }

    private void notifyProgress(String url, int iProgress) {
        progress = iProgress;
        if (progress > max) {
            progress = max;
        }
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            progressEvent.senderUrl = url;
            progressEvent.progress = progress;
            progressEvent.max = max;
            l.progress(progressEvent);
        }
    }

    private void notifyFertig(String url, ListeFilme liste) {
        logger.info("Liste Filme gelesen am: {}", FastDateFormat.getInstance("dd.MM.yyyy, HH:mm").format(new Date()));
        logger.info("  erstellt am: {}", liste.genDate());
        logger.info("  Anzahl Filme: {}", liste.size());
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            progressEvent.senderUrl = url;
            progressEvent.text = "";
            progressEvent.max = max;
            progressEvent.progress = progress;
            l.fertig(progressEvent);
        }
    }

    @Override
    public void close() {
        removeRegisteredListeners();
    }
}
