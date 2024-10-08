package mediathek.filmlisten.writer;

import com.fasterxml.jackson.core.JsonEncoding;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import mediathek.daten.Country;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.gui.messages.FilmListWriteStartEvent;
import mediathek.gui.messages.FilmListWriteStopEvent;
import mediathek.tool.MessageBus;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class FilmListWriter {

    private static final Logger logger = LogManager.getLogger(FilmListWriter.class);
    private static final String TAG_JSON_LIST = "X";
    private final boolean readable;
    private String sender = "";
    private String thema = "";
    private boolean compressSenderTag = true;
    private boolean compressThemaTag = true;

    public FilmListWriter(boolean readable) {
        this.readable = readable;
    }

    private JsonGenerator getJsonGenerator(OutputStream os) throws IOException {
        final JsonFactory jsonF = new JsonFactory();
        JsonGenerator jg = jsonF.createGenerator(os, JsonEncoding.UTF8);
        if (readable)
            jg = jg.useDefaultPrettyPrinter();

        return jg;
    }

    private void checkOsxCacheDirectory() {
        final Path filePath = Paths.get(SystemUtils.USER_HOME + File.separator + "Library/Caches/MediathekView");
        if (Files.notExists(filePath)) {
            try {
                Files.createDirectories(filePath);
            } catch (IOException e) {
                logger.error("checkOsxCacheDirectory", e);
            }
        }
    }

    private void writeFormatHeader(JsonGenerator jg, ListeFilme listeFilme) throws IOException {
        final var meta = listeFilme.getMetaData();

        jg.writeArrayFieldStart(ListeFilme.FILMLISTE);
        jg.writeString(""); //ListeFilme.FILMLISTE_DATUM_NR unused in newer versions
        jg.writeString(meta.getDatum());
        jg.writeString(meta.getVersion());
        jg.writeString("");
        jg.writeString(meta.getId());
        jg.writeEndArray();
    }

    public void writeFilmList(String datei, ListeFilme listeFilme, IProgressListener listener) {
        MessageBus.getMessageBus().publishAsync(new FilmListWriteStartEvent());

        try {
            logger.info("Filme schreiben ({} Filme) :", listeFilme.size());
            logger.info("   --> Start Schreiben nach: {}", datei);

            sender = "";
            thema = "";

            //Check if Cache directory exists on OSX
            if (SystemUtils.IS_OS_MAC_OSX) {
                checkOsxCacheDirectory();
            }


            Path filePath = Paths.get(datei);
            try {
                Files.deleteIfExists(filePath);
            }
            catch (Exception e) {
                logger.warn("error trying to delete file", e);
                logger.trace("Waiting two seconds...");
                TimeUnit.SECONDS.sleep(2);
            }
            long start = System.nanoTime();

            try (OutputStream fos = Files.newOutputStream(filePath);
                 BufferedOutputStream bos = new BufferedOutputStream(fos, 64 * 1024);
                 JsonGenerator jg = getJsonGenerator(bos)) {

                jg.writeStartObject();

                writeFormatHeader(jg, listeFilme);
                writeFormatDescription(jg);

                final long filmEntries = listeFilme.size();
                float curEntry = 0f;

                for (DatenFilm datenFilm : listeFilme) {
                    writeEntry(datenFilm, jg);
                    if (listener != null) {
                        listener.progress(curEntry / filmEntries);
                        curEntry++;
                    }
                }
                jg.writeEndObject();

                if (listener != null)
                    listener.progress(1d);

                long end = System.nanoTime();

                logger.info("   --> geschrieben!");
                logger.trace("Write duration: {} ms", TimeUnit.MILLISECONDS.convert(end - start, TimeUnit.NANOSECONDS));
            }
        } catch (Exception ex) {
            logger.error("nach: {}", datei, ex);
        }

        MessageBus.getMessageBus().publishAsync(new FilmListWriteStopEvent());
    }

    private void writeEntry(DatenFilm datenFilm, JsonGenerator jg) throws IOException {
        jg.writeArrayFieldStart(TAG_JSON_LIST);

        writeSender(jg, datenFilm);
        writeThema(jg, datenFilm);
        writeTitel(jg, datenFilm);
        jg.writeString(datenFilm.getSendeDatum());
        writeZeit(jg, datenFilm);
        jg.writeString(datenFilm.getFilmLengthAsString());
        jg.writeString(datenFilm.getFileSize().toString());
        jg.writeString(datenFilm.getDescription());
        jg.writeString(datenFilm.getUrlNormalQuality());
        jg.writeString(datenFilm.getWebsiteUrl());
        jg.writeString(datenFilm.getSubtitleUrl());
        skipEntry(jg); //DatenFilm.FILM_URL_RTMP
        writeLowQualityUrl(jg, datenFilm);
        skipEntry(jg); //DatenFilm.URL_RTMP_KLEIN
        writeHighQualityUrl(jg, datenFilm);
        skipEntry(jg); //DatenFilm.FILM_URL_RTMP_HD
        jg.writeString(datenFilm.getDatumLong());
        skipEntry(jg); //DatenFilm.FILM_URL_HISTORY
        if (datenFilm.countrySet.isEmpty())
            jg.writeString("");
        else
            jg.writeString(datenFilm.countrySet.stream().map(Country::toString).collect(Collectors.joining("-")));
        jg.writeString(Boolean.toString(datenFilm.isNew()));

        jg.writeEndArray();
    }

    private void writeLowQualityUrl(@NotNull JsonGenerator jg, @NotNull DatenFilm datenFilm) throws IOException {
        String url = datenFilm.getLowQualityUrl();
        if (decompressUrls) {
            if (DatenFilm.isCompressedUrl(url)) {
                url = datenFilm.decompressUrl(url);
            }
        }

        jg.writeString(url);
    }

    private void writeHighQualityUrl(@NotNull JsonGenerator jg, @NotNull DatenFilm datenFilm) throws IOException {
        String url = datenFilm.getHighQualityUrl();
        if (decompressUrls) {
            if (DatenFilm.isCompressedUrl(url)) {
                url = datenFilm.decompressUrl(url);
            }
        }

        jg.writeString(url);
    }

    public void setDecompressUrls(boolean decompressUrls) {
        this.decompressUrls = decompressUrls;
    }

    private boolean decompressUrls;

    private void skipEntry(JsonGenerator jg) throws IOException {
        jg.writeString("");
    }

    private void writeTitel(JsonGenerator jg, DatenFilm datenFilm) throws IOException {
        jg.writeString(datenFilm.getTitle());
    }

    private void writeSender(JsonGenerator jg, DatenFilm datenFilm) throws IOException {
        String tempSender = datenFilm.getSender();

        if (compressSenderTag) {
            if (tempSender.equals(sender)) {
                jg.writeString("");
            } else {
                sender = tempSender;
                jg.writeString(tempSender);
            }
        }
        else
            jg.writeString(tempSender);
    }

    public void setCompressThemaTag(boolean compressThemaTag) {
        this.compressThemaTag = compressThemaTag;
    }

    public void setCompressSenderTag(boolean compress) {
        compressSenderTag = compress;
    }

    private void writeThema(JsonGenerator jg, DatenFilm datenFilm) throws IOException {
        if (compressThemaTag) {
            if (datenFilm.getThema().equals(thema)) {
                jg.writeString("");
            } else {
                thema = datenFilm.getThema();
                jg.writeString(datenFilm.getThema());
            }
        }
        else
            jg.writeString(datenFilm.getThema());
    }

    private void writeZeit(JsonGenerator jg, DatenFilm datenFilm) throws IOException {
        String strZeit = datenFilm.getSendeZeit();
        final int len = strZeit.length();

        if (strZeit.isEmpty() || len < 8)
            jg.writeString("");
        else {
            strZeit = strZeit.substring(0, len - 3);
            jg.writeString(strZeit);
        }
    }

    /**
     * Write a dummy field description array.
     * Is not used anywhere but necessary for compatibility
     */
    private void writeFormatDescription(JsonGenerator jg) throws IOException {
        jg.writeArrayFieldStart(ListeFilme.FILMLISTE);
        jg.writeString("");
        jg.writeEndArray();
    }

    @FunctionalInterface
    public interface IProgressListener {
        void progress(double current);
    }
}
