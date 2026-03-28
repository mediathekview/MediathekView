/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.filmlisten.reader;

import mediathek.config.Config;
import mediathek.config.Konstanten;
import mediathek.controller.SenderFilmlistLoadApprover;
import mediathek.daten.Country;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.InputStreamProgressMonitor;
import mediathek.tool.ProgressMonitorInputStream;
import mediathek.tool.TrailerTeaserChecker;
import mediathek.tool.datum.DateUtil;
import mediathek.tool.episodes.SeasonEpisode;
import mediathek.tool.episodes.TitleParserManager;
import mediathek.tool.http.MVHttpClient;
import mediathek.tool.time.Stopwatch;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.Okio;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.tukaani.xz.XZInputStream;
import tools.jackson.core.JsonParser;
import tools.jackson.core.JsonToken;
import tools.jackson.core.ObjectReadContext;
import tools.jackson.core.json.JsonFactory;

import javax.swing.event.EventListenerList;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class FilmListReader implements AutoCloseable {
    private static final int PROGRESS_MAX = 100;
    private static final Logger logger = LogManager.getLogger(FilmListReader.class);
    private static final String THEMA_LIVE = "Livestream";
    private static final String PLAYLIST_SUFFIX = ".m3u8";
    private static final String SENDER_RBTV = "rbtv";
    private static final String SENDER_RADIO_BREMEN = "Radio Bremen TV";
    /**
     * Memory limit for the xz decompressor. No limit by default.
     */
    protected final int DECOMPRESSOR_MEMORY_LIMIT = -1;
    private final EventListenerList listeners = new EventListenerList();
    private final ListenerFilmeLadenEvent progressEvent = new ListenerFilmeLadenEvent("", "Download", 0, 0, false);
    private final int max;
    private final TrailerTeaserChecker ttc = new TrailerTeaserChecker();
    private final TitleParserManager manager = new TitleParserManager();
    private int progress;
    private Consumer<DatenFilm> filmSink;
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

        return switch (source.substring(source.lastIndexOf('.'))) {
            case Konstanten.FORMAT_XZ -> new XZInputStream(in, DECOMPRESSOR_MEMORY_LIMIT, false);
            case ".json" -> in;
            default -> throw new UnsupportedOperationException("Unbekanntes Dateiformat entdeckt.");
        };
    }

    private void parseNeu(JsonParser jp, DatenFilm datenFilm) {
        final String value = nextTextValue(jp);
        datenFilm.setNew(Boolean.parseBoolean(value));
    }

    protected void parseWebsiteLink(JsonParser jp, DatenFilm datenFilm) {
        final String value = nextTextValue(jp);
        if (value != null && !value.isEmpty()) {
            datenFilm.setWebsiteUrl(value);
        }
    }

    private void parseDescription(JsonParser jp, DatenFilm datenFilm) {
        final String value = nextTextValue(jp);
        if (value != null && !value.isEmpty())
            datenFilm.setDescription(value);
    }

    protected void parseGeo(JsonParser jp, DatenFilm datenFilm) {
        var geoStr = checkedString(jp);

        if (geoStr.isEmpty())
            datenFilm.clearCountries();
        else {
            /*
            This code is more performant than String.split as we do not allocate arrays on every call.
             */
            int start = 0;
            int length = geoStr.length();
            for (int i = 0; i <= length; i++) {
                if (i < length && geoStr.charAt(i) != '-') {
                    continue;
                }
                String geoItem = geoStr.substring(start, i);
                start = i + 1;
                if (geoItem.isEmpty()) {
                    continue;
                }
                try {
                    datenFilm.addCountry(Country.valueOf(geoItem));
                }
                catch (IllegalArgumentException ex) {
                    logger.error("Unable to parse string {} to Country enum", geoItem);
                }
            }
        }
    }

    private void parseSender(JsonParser jp, DatenFilm datenFilm) {
        String parsedSender = checkedString(jp);
        if (parsedSender.isEmpty())
            datenFilm.setSender(sender);
        else {
            datenFilm.setSender(parsedSender);
            //store for future reads
            sender = parsedSender;
        }

        if (datenFilm.getSender().equalsIgnoreCase(SENDER_RBTV)) {
            datenFilm.setSender(SENDER_RADIO_BREMEN);
        }
    }

    private void parseThema(JsonParser jp, DatenFilm datenFilm) {
        String value = checkedString(jp);
        if (value.isEmpty())
            datenFilm.setThema(thema);
        else {
            datenFilm.setThema(value);
            thema = value;
        }

        //we need to check thema as well as (currently) ARD also puts teaser only into thema...
        if (ttc.check(datenFilm.getThema()))
            datenFilm.setTrailerTeaser(true);
    }

    private String checkedString(JsonParser jp) {
        String value = nextTextValue(jp);
        //only check for null and replace for the default rows...
        if (value == null)
            value = "";

        return value;
    }

    private String nextTextValue(JsonParser jp) {
        JsonToken token = jp.nextToken();
        if (token == null || token == JsonToken.VALUE_NULL)
            return null;
        return jp.getValueAsString();
    }

    private void parseMetaData(JsonParser jp, ListeFilme listeFilme) {
        JsonToken jsonToken;
        while ((jsonToken = jp.nextToken()) != null) {
            if (jsonToken == JsonToken.END_OBJECT) {
                break;
            }
            if (jp.isExpectedStartArrayToken()) {
                var meta = listeFilme.getMetaData();
                nextTextValue(jp);
                meta.setDatum(nextTextValue(jp));
                nextTextValue(jp);
                nextTextValue(jp);
                meta.setId(nextTextValue(jp));
                //update to fire pcs
                listeFilme.setMetaData(meta);

                break;
            }
        }
    }

    private void skipFieldDescriptions(JsonParser jp) {
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

    private void parseUrlSubtitle(JsonParser jp, DatenFilm datenFilm) {
        datenFilm.setSubtitleUrl(checkedString(jp));
    }

    private void parseUrlKlein(JsonParser jp, DatenFilm datenFilm) {
        datenFilm.setLowQualityUrl(checkedString(jp));
    }

    private void parseUrlHd(JsonParser jp, DatenFilm datenFilm) {
        datenFilm.setHighQualityUrl(checkedString(jp));
    }

    private void parseDatumLong(JsonParser jp, DatenFilm datenFilm) {
        var str = checkedString(jp);
        datenFilm.setDatumLong(str);
    }

    private void parseSendedatum(JsonParser jp, DatenFilm datenFilm) {
        datenFilm.setSendeDatum(checkedString(jp));
    }

    private void parseFilmLength(JsonParser jp, DatenFilm datenFilm) {
        datenFilm.setFilmLength(checkedString(jp));
    }

    private void parseGroesse(JsonParser jp, DatenFilm datenFilm) {
        String value = checkedString(jp);
        datenFilm.getFileSize().setSize(value);
    }

    /**
     * Skip over file entry.
     * This is used when fields were deleted in DatenFilm but still exit in filmlist file.
     */
    private void skipToken(JsonParser jp) {
        jp.nextToken();
    }

    private void parseTime(JsonParser jp, DatenFilm datenFilm) {
        String zeit = checkedString(jp);
        if (!zeit.isEmpty() && zeit.length() < 8) {
            zeit += ":00"; // add seconds
        }
        datenFilm.setSendeZeit(zeit);
    }

    /**
     * Check if the title contains keywords which specify an audio version
     */
    private void parseAudioVersion(String title, DatenFilm film) {
        if (title.contains("Hörfassung") || title.contains("Audiodeskription")
                || title.contains("AD |") || title.endsWith("(AD)")
                || title.contains("Hörspiel") || title.contains("Hörfilm")
                || title.contains("mit gesprochenen Untertiteln"))
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

    private void parseTitel(JsonParser jp, DatenFilm datenFilm) {
        final String title = checkedString(jp);
        datenFilm.setTitle(title);
        //check title if it is audio version
        parseAudioVersion(title, datenFilm);
        //check if it is in sign language
        parseSignLanguage(title, datenFilm);
        parseTrailerTeaser(title, datenFilm);
        // check for burned in subtitles
        if (title.contains("(mit Untertitel)"))
            datenFilm.setBurnedInSubtitles(true);
    }

    private void parseUrl(JsonParser jp, DatenFilm datenFilm) {
        datenFilm.setNormalQualityUrl(checkedString(jp));
    }

    private void parseLivestream(DatenFilm datenFilm) {
        if (datenFilm.getThema().equals(THEMA_LIVE))
            datenFilm.setLivestream(true);
    }

    private void readData(JsonParser jp, ListeFilme listeFilme) {
        JsonToken jsonToken;

        if (jp.nextToken() != JsonToken.START_OBJECT) {
            throw new IllegalStateException("Expected data to start with an Object");
        }

        parseMetaData(jp, listeFilme);

        skipFieldDescriptions(jp);

        final var config = ApplicationConfiguration.getConfiguration();
        final boolean loadTrailer = config.getBoolean(ApplicationConfiguration.FilmList.LOAD_TRAILER, true);
        final boolean loadAudiodescription = config.getBoolean(ApplicationConfiguration.FilmList.LOAD_AUDIO_DESCRIPTION, true);
        final boolean loadSignLanguage = config.getBoolean(ApplicationConfiguration.FilmList.LOAD_SIGN_LANGUAGE, true);
        final boolean loadLivestreams = config.getBoolean(ApplicationConfiguration.FilmList.LOAD_LIVESTREAMS, true);

        while ((jsonToken = jp.nextToken()) != null) {
            if (jsonToken == JsonToken.END_OBJECT) {
                break;
            }
            if (jp.isExpectedStartArrayToken()) {
                DatenFilm datenFilm = new DatenFilm();
                parseSender(jp, datenFilm);
                parseThema(jp, datenFilm);
                parseTitel(jp, datenFilm);
                parseSendedatum(jp, datenFilm);
                parseTime(jp, datenFilm);
                parseFilmLength(jp, datenFilm);
                parseGroesse(jp, datenFilm);
                parseDescription(jp, datenFilm);
                parseUrl(jp, datenFilm);
                parseWebsiteLink(jp, datenFilm);
                parseUrlSubtitle(jp, datenFilm);
                skipToken(jp);
                parseUrlKlein(jp, datenFilm);
                skipToken(jp);
                parseUrlHd(jp, datenFilm);
                skipToken(jp);
                parseDatumLong(jp, datenFilm);
                skipToken(jp); //HISTORY_URL
                parseGeo(jp, datenFilm);
                parseNeu(jp, datenFilm);

                //this will check after all data has been read
                parseLivestream(datenFilm);
                checkPlayList(datenFilm);

                //if user specified he doesn´t want to load this sender, skip...
                if (!SenderFilmlistLoadApprover.isApproved(datenFilm.getSender()))
                    continue;

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

                if (!loadLivestreams) {
                    if (datenFilm.isLivestream())
                        continue;
                }

                //just initialize the film object, rest will be done in one of the filters
                datenFilm.init();

                // this will add the film to the filmlist if it passes...
                filmSink.accept(datenFilm);
            }
        }
    }

    /**
     * Check if this film entry is a playlist entry, ends with .m3u8
     *
     * @param datenFilm the film to check.
     */
    private void checkPlayList(@NotNull DatenFilm datenFilm) {
        if (datenFilm.getUrlNormalQuality().endsWith(PLAYLIST_SUFFIX))
            datenFilm.setPlayList(true);
    }

    public void readFilmListe(String source, @NotNull ListeFilme listeFilme, int days) {
        try {
            logger.trace("Liste Filme lesen von: {}", source);
            listeFilme.clear();

            if (days == 0)
                filmSink = listeFilme::add;
            else {
                final LocalDate cutoffDate = LocalDate.now().minusDays(days);
                filmSink = film -> {
                    // do not filter livestreams
                    if (film.isLivestream()) {
                        listeFilme.add(film);
                        return;
                    }

                    final LocalDate filmDate = DateUtil.convertToLocalDate(film.getDatumFilm());
                    if (!cutoffDate.isAfter(filmDate)) {
                        listeFilme.add(film);
                    }
                };
            }

            notifyStart(source); // für die Progressanzeige

            if (source.startsWith("http")) {
                final var sourceUrl = new URI(source);
                processFromWeb(sourceUrl.toURL(), listeFilme);
            }
            else
                processFromFile(source, listeFilme);

            parseSeasonAndEpisode(listeFilme);
        }
        catch (URISyntaxException | IOException ex) {
            logger.warn(ex);
        }

        notifyFertig(source, listeFilme);
    }

    private void parseSeasonAndEpisode(@NotNull ListeFilme listeFilme) {
        AtomicInteger counter = new AtomicInteger(0);
        Stopwatch stopwatch = Stopwatch.createStarted();
        listeFilme.parallelStream()
                .forEach(film -> {
                    Optional<SeasonEpisode> result = manager.parse(film.getSender(), film.getTitle());
                    result.ifPresent(sea -> {
                        film.setSeasonEpisode(sea);
                        counter.incrementAndGet();
                    });
                });

        stopwatch.stop();
        logger.info("Season and episode detection took: {}", stopwatch);
        logger.info("Number of detected seasons and episodes: {}", counter.get());
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

            //windows doesn´t like mem-mapped files...causes FileSystemExceptions :(
            try (var sourceFile = Okio.source(filePath);
                 var bufferedSource = Okio.buffer(sourceFile);
                 var is = bufferedSource.inputStream();
                 InputStream input = new ProgressMonitorInputStream(is, fileSize, monitor);
                 InputStream in = selectDecompressor(source, input);
                 JsonParser jp = new JsonFactory().createParser(ObjectReadContext.empty(), in)) {
                readData(jp, listeFilme);
            }
        }
        catch (FileNotFoundException | NoSuchFileException ex) {
            logger.debug("FilmListe existiert nicht: {}", source);
            listeFilme.clear();
        }
        catch (Exception ex) {
            logger.error("FilmListe: {}", source, ex);
            listeFilme.clear();
        }
    }

    private String buildClientInfo() {
        List<Object> clientData = Arrays.asList(Konstanten.PROGRAMMNAME, Konstanten.MVVERSION, SystemUtils.OS_ARCH,
                SystemUtils.OS_NAME, SystemUtils.OS_VERSION);
        return clientData.stream().map(Object::toString).collect(Collectors.joining(","));
    }

    /**
     * Download and process a filmliste from the web.
     *
     * @param source     source url as string
     * @param listeFilme the list to read to
     */
    @SuppressWarnings("UastIncorrectHttpHeaderInspection")
    private void processFromWeb(URL source, ListeFilme listeFilme) {
        final Request request = new Request.Builder()
                .url(source)
                .header("MV-Client", buildClientInfo())
                .get()
                .build();

        try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (response.isSuccessful()) {
                final var endRequest = response.request();
                if (Config.isEnhancedLoggingEnabled()) {
                    logger.trace("Final Endpoint URL for filmlist: {}", endRequest.url().toString());
                }
                ProgressMonitor monitor = new ProgressMonitor(source.toString());
                try (InputStream input = new ProgressMonitorInputStream(body.byteStream(), body.contentLength(), monitor);
                     InputStream is = selectDecompressor(source.toString(), input);
                     JsonParser jp = new JsonFactory().createParser(ObjectReadContext.empty(), is)) {
                    readData(jp, listeFilme);
                }
            }
            else
                logger.warn("processFromWeb HTTP Response Code: {} for {}", response.code(), response.request().url().url());

        }
        catch (Exception ex) {
            logger.error("FilmListe: {}", source, ex);
            listeFilme.clear();
        }
    }

    private void notifyStart(String url) {
        progress = 0;
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.start(new ListenerFilmeLadenEvent(url, "", max, 0, false));
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
        logger.info("Liste Filme gelesen am: {}", DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm")
                .format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault())));
        logger.info("  erstellt am: {}", liste.getMetaData().getGenerationDateTimeAsString());
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
        private static final long MIN_TIME_BETWEEN_UPDATES_MS = 500;
        private final String sourceString;
        private int oldProgress;
        private long lastUpdate;

        public ProgressMonitor(String source) {
            sourceString = source;
        }

        @Override
        public void progress(long bytesRead, long size) {
            if (size <= 0) {
                return;
            }

            int iProgress = (int) (bytesRead * 100 / size);
            long now = System.currentTimeMillis();

            if (iProgress >= oldProgress + 1 || now - lastUpdate > MIN_TIME_BETWEEN_UPDATES_MS) {
                oldProgress = iProgress;
                lastUpdate = now;
                notifyProgress(sourceString, iProgress);
            }
        }
    }
}
