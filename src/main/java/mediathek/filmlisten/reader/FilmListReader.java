package mediathek.filmlisten.reader;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.google.common.base.Stopwatch;
import mediathek.config.Config;
import mediathek.config.Konstanten;
import mediathek.controller.SenderFilmlistLoadApprover;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.tool.*;
import mediathek.tool.http.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.Okio;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.tukaani.xz.XZInputStream;

import javax.swing.event.EventListenerList;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public class FilmListReader implements AutoCloseable {
    private static final int PROGRESS_MAX = 100;
    private static final Logger logger = LogManager.getLogger(FilmListReader.class);
    private static final String THEMA_LIVE = "Livestream";
    private final EventListenerList listeners = new EventListenerList();
    private final ListenerFilmeLadenEvent progressEvent = new ListenerFilmeLadenEvent("", "Download", 0, 0, 0, false);
    private final int max;
    private final TrailerTeaserChecker ttc = new TrailerTeaserChecker();
    private final SenderFilmlistLoadApprover senderApprover = SenderFilmlistLoadApprover.INSTANCE;
    /**
     * Memory limit for the xz decompressor. No limit by default.
     */
    protected int DECOMPRESSOR_MEMORY_LIMIT = -1;
    private int progress;
    private long milliseconds;
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
        var geoStr = checkedString(jp);

        Optional<String> geo;
        if (geoStr.isEmpty())
            geo = Optional.empty();
        else
            geo = Optional.of(geoStr);

        datenFilm.setGeo(geo);
    }

    private void parseSender(JsonParser jp, DatenFilm datenFilm) throws IOException {
        String parsedSender = checkedString(jp);
        if (parsedSender.isEmpty())
            datenFilm.setSender(sender);
        else {
            datenFilm.setSender(parsedSender);
            //store for future reads
            sender = parsedSender;
        }

        if (datenFilm.getSender().equalsIgnoreCase("rbtv")) {
            datenFilm.setSender("Radio Bremen TV");
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

        //we need to check thema as well as (currently) ARD also puts teaser only into thema...
        if (ttc.check(datenFilm.getThema()))
            datenFilm.setTrailerTeaser(true);
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

    private void parseUrlSubtitle(JsonParser jp, DatenFilm datenFilm) throws IOException {
        datenFilm.setUrlSubtitle(checkedString(jp));
    }

    private void parseUrlKlein(JsonParser jp, DatenFilm datenFilm) throws IOException {
        datenFilm.setUrlKlein(checkedString(jp));
    }

    private void parseUrlHd(JsonParser jp, DatenFilm datenFilm) throws IOException {
        datenFilm.setUrlHighQuality(checkedString(jp));
    }

    private void parseDatumLong(JsonParser jp, DatenFilm datenFilm) throws IOException {
        datenFilm.setDatumLong(checkedString(jp));
    }

    private void parseSendedatum(JsonParser jp, DatenFilm datenFilm) throws IOException {
        datenFilm.setSendeDatum(checkedString(jp));
    }

    private void parseDauer(JsonParser jp, DatenFilm datenFilm) throws IOException {
        datenFilm.setDauer(checkedString(jp));
    }

    private void parseGroesse(JsonParser jp, DatenFilm datenFilm) throws IOException {
        String value = checkedString(jp);
        datenFilm.setSize(value);
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

    private void parseTitel(JsonParser jp, DatenFilm datenFilm) throws IOException {
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
                parseDauer(jp, datenFilm);
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
                if (!senderApprover.isApproved(datenFilm.getSender()))
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

    /**
     * Check if this film entry is a playlist entry, ends with .m3u8
     *
     * @param datenFilm the film to check.
     */
    private void checkPlayList(@NotNull DatenFilm datenFilm) {
        if (datenFilm.getUrl().endsWith(".m3u8"))
            datenFilm.setPlayList(true);
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

        if (MemoryUtils.isLowMemoryEnvironment())
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

            //windows doesn´t like mem-mapped files...causes FileSystemExceptions :(
            try (var sourceFile = Okio.source(filePath);
                 var bufferedSource = Okio.buffer(sourceFile);
                 var is = bufferedSource.inputStream();
                 InputStream input = new ProgressMonitorInputStream(is, fileSize, monitor);
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
                if (Config.isEnhancedLoggingEnabled()) {
                    logger.trace("Final Endpoint URL for filmlist: {}", endRequest.url().toString());
                }
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
        logger.info("Liste Filme gelesen am: {}", DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm")
                .format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault())));
        logger.info("  erstellt am: {}", liste.metaData().getGenerationDateTimeAsString());
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
        private int oldProgress;

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
