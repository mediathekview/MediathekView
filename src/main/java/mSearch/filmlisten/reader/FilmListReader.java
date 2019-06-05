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
package mSearch.filmlisten.reader;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.google.common.base.Stopwatch;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.InputStreamProgressMonitor;
import mSearch.tool.MVHttpClient;
import mSearch.tool.ProgressMonitorInputStream;
import mediathek.config.Const;
import mediathek.config.Konstanten;
import mediathek.tool.TrailerTeaserChecker;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.tukaani.xz.XZInputStream;

import javax.swing.event.EventListenerList;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.concurrent.TimeUnit;

public class FilmListReader implements AutoCloseable {
    private static final int PROGRESS_MAX = 100;
    private static final Logger logger = LogManager.getLogger(FilmListReader.class);
    private static final String THEMA_LIVE = "Livestream";
    private final EventListenerList listeners = new EventListenerList();
    private final ListenerFilmeLadenEvent progressEvent = new ListenerFilmeLadenEvent("", "Download", 0, 0, 0, false);
    private final int max;
    private final TrailerTeaserChecker ttc = new TrailerTeaserChecker();
    /**
     * Memory limit for the xz decompressor. No limit by default.
     */
    protected int DECOMPRESSOR_MEMORY_LIMIT = -1;
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
        ListenerFilmeLaden[] list = listeners.getListeners(ListenerFilmeLaden.class);
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
        datenFilm.setGeo(checkedString(jp));
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
            datenFilm.setThema(thema);
        else {
            datenFilm.setThema(value);
            thema = value;
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
                var meta = listeFilme.metaData();
                jp.nextTextValue();
                meta.setDatum(jp.nextTextValue());
                jp.nextTextValue();
                jp.nextTextValue();
                meta.setId(jp.nextTextValue());

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

    /**
     * Check if the title contains keywords which specify an audio version
     */
    private void parseAudioVersion(String title, DatenFilm film) {
        if (title.contains("Hörfassung") || title.contains("Audiodeskription")
                || title.contains("AD |") || title.endsWith("(AD)"))
            film.setAudioVersion(true);
    }

    private void parseSignLanguage(String title, DatenFilm film) {
        if (title.contains("Gebärden"))
            film.setSignLanguage(true);
    }

    private void parseTrailerTeaser(String title, DatenFilm film) {
        if (ttc.check(title))
            film.setTrailerTeaser(true);
    }

    private void parseTitel(JsonParser jp, DatenFilm datenFilm) throws IOException {
        final String title = checkedString(jp);
        datenFilm.setTitle(title);
        //check title if it is audio version
        parseAudioVersion(title, datenFilm);
        //check if it is in sign language
        parseSignLanguage(title, datenFilm);
        parseTrailerTeaser(title, datenFilm);
    }

    private void parseUrl(JsonParser jp, DatenFilm datenFilm) throws IOException {
        datenFilm.setUrl(checkedString(jp));
    }

    private void parseLivestream(DatenFilm datenFilm) {
        if (datenFilm.getThema().equals(THEMA_LIVE))
            datenFilm.setLivestream(true);
    }

    private void readData(JsonParser jp, ListeFilme listeFilme) throws IOException {
        Stopwatch stopwatch = Stopwatch.createStarted();
        JsonToken jsonToken;

        if (jp.nextToken() != JsonToken.START_OBJECT) {
            throw new IllegalStateException("Expected data to start with an Object");
        }

        parseMetaData(jp, listeFilme);

        skipFieldDescriptions(jp);

        final var config = ApplicationConfiguration.getConfiguration();
        final boolean loadTrailer = config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_TRAILER, true);
        final boolean loadAudiodescription = config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_AUDIODESCRIPTION, true);
        final boolean loadSignLanguage = config.getBoolean(ApplicationConfiguration.FILMLIST_LOAD_SIGNLANGUAGE, true);

        while ((jsonToken = jp.nextToken()) != null) {
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
                parseUrl(jp, datenFilm);
                parseWebsiteLink(jp, datenFilm);
                parseDefault(jp, datenFilm, DatenFilm.FILM_URL_SUBTITLE);
                skipToken(jp);
                parseDefault(jp, datenFilm, DatenFilm.FILM_URL_KLEIN);
                skipToken(jp);
                parseDefault(jp, datenFilm, DatenFilm.FILM_URL_HD);
                skipToken(jp);
                parseDefault(jp, datenFilm, DatenFilm.FILM_DATUM_LONG);
                skipToken(jp); //HISTORY_URL
                parseGeo(jp, datenFilm);
                parseNeu(jp, datenFilm);

                //this will check after all data has been read
                parseLivestream(datenFilm);

                if (!loadTrailer) {
                    if (datenFilm.isTrailerTeaser())
                        continue;
                }

                if (!loadAudiodescription) {
                    if (datenFilm.isAudioVersion())
                        continue;
                }

                if (!loadSignLanguage) {
                    if (datenFilm.isSignLanguage())
                        continue;
                }

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

        stopwatch.stop();
        logger.debug("Reading filmlist took {}", stopwatch);
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
            logger.trace("Liste Filme lesen von: {}", source);
            listeFilme.clear();

            notifyStart(source); // für die Progressanzeige

            checkDays(days);

            if (source.startsWith("http")) {
                final URL sourceUrl = new URL(source);
                processFromWeb(sourceUrl, listeFilme);
            } else
                processFromFile(source, listeFilme);

        } catch (MalformedURLException ex) {
            logger.warn(ex);
        }

        DatenFilm.Database.createIndices();

        notifyFertig(source, listeFilme);
    }

    /**
     * Read a locally available filmlist.
     *
     * @param source     file path as string
     * @param listeFilme the list to read to
     */
    private void processFromFile(String source, ListeFilme listeFilme) {
        try {
            final Path filePath = Paths.get(source);
            final long fileSize = Files.size(filePath);
            if (fileSize == 0)
                Files.deleteIfExists(filePath);

            final ProgressMonitor monitor = new ProgressMonitor(source);
            try (FileInputStream fis = new FileInputStream(source);
                 BufferedInputStream bis = new BufferedInputStream(fis, (int) (8 * FileUtils.ONE_MB));
                 InputStream input = new ProgressMonitorInputStream(bis, fileSize, monitor);
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

    /**
     * Download and process a filmliste from the web.
     *
     * @param source     source url as string
     * @param listeFilme the list to read to
     */
    private void processFromWeb(URL source, ListeFilme listeFilme) {
        final String clientId = Konstanten.MVVERSION.toString() + "," + SystemUtils.OS_ARCH + "," + SystemUtils.OS_NAME + "," + SystemUtils.OS_VERSION;

        final Request request = new Request.Builder()
                .url(source)
                .header("MV-Client", clientId)
                .get()
                .build();

        try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (response.isSuccessful() && body != null) {
                final var endRequest = response.request();
                logger.trace("Final Endpoint URL for filmlist: {}", endRequest.url().toString());
                ProgressMonitor monitor = new ProgressMonitor(source.toString());
                try (InputStream input = new ProgressMonitorInputStream(body.byteStream(), body.contentLength(), monitor);
                     InputStream is = selectDecompressor(source.toString(), input);
                     JsonParser jp = new JsonFactory().createParser(is)) {
                    readData(jp, listeFilme);
                }
            } else
                logger.warn("processFromWeb HTTP Response Code: {} for {}", response.code(), response.request().url().url());

        } catch (Exception ex) {
            logger.error("FilmListe: {}", source, ex);
            listeFilme.clear();
        }
    }

    /**
     *
     * @param film film to be checked.
     * @return true if film should be displayed
     */
    private boolean checkDate(@NotNull DatenFilm film) {
        final var time = film.getDatumFilm().getTime();
        return time == 0 || time >= milliseconds;
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

    class ProgressMonitor implements InputStreamProgressMonitor {
        private final String sourceString;
        private int oldProgress = 0;

        public ProgressMonitor(String source) {
            sourceString = source;
        }

        @Override
        public void progress(long bytesRead, long size) {
            final int iProgress = (int) (bytesRead * 100 / size);
            if (iProgress != oldProgress) {
                oldProgress = iProgress;
                notifyProgress(sourceString, iProgress);
            }
        }
    }
}
