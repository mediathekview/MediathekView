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

package mediathek.filmlisten.writer;

import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.gui.messages.FilmListWriteStartEvent;
import mediathek.gui.messages.FilmListWriteStopEvent;
import mediathek.tool.MessageBus;
import mediathek.tool.datum.DatumFilm;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import tools.jackson.core.JsonEncoding;
import tools.jackson.core.JsonGenerator;
import tools.jackson.core.ObjectWriteContext;
import tools.jackson.core.json.JsonFactory;
import tools.jackson.core.util.DefaultPrettyPrinter;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.concurrent.TimeUnit;

public class FilmListWriter {

    private static final String FILMLISTE = "Filmliste";
    private static final Logger logger = LogManager.getLogger();
    private static final String TAG_JSON_LIST = "X";
    private final boolean readable;
    private String sender = "";
    private String thema = "";
    private boolean compressSenderTag = true;
    private boolean compressThemaTag = true;
    private boolean decompressUrls;

    public FilmListWriter(boolean readable) {
        this.readable = readable;
    }

    private JsonGenerator getJsonGenerator(OutputStream os) {
        ObjectWriteContext context = ObjectWriteContext.empty();
        if (readable) {
            context = new PrettyObjectWriteContext();
        }

        final JsonFactory jsonF = new JsonFactory();
        return jsonF.createGenerator(context, os, JsonEncoding.UTF8);
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

    private void writeFormatHeader(JsonGenerator jg, ListeFilme listeFilme) {
        final var meta = listeFilme.getMetaData();

        jg.writeArrayPropertyStart(FILMLISTE);
        jg.writeString(""); //ListeFilme.FILMLISTE_DATUM_NR unused in newer versions
        jg.writeString(meta.getDatum());
        jg.writeString(meta.getVersion());
        jg.writeString("");
        jg.writeString(meta.getId());
        jg.writeEndArray();
    }

    public void writeFilmList(String datei, ListeFilme listeFilme, IProgressListener progressListener) {
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
                long curEntry = 0;
                final long progressStep = Math.max(1L, filmEntries / 500L);

                if (compressSenderTag)
                    listeFilme.sort(Comparator.comparing(DatenFilm::getSender).thenComparing(DatenFilm::getThema));

                for (DatenFilm datenFilm : listeFilme) {
                    writeEntry(datenFilm, jg);
                    curEntry++;
                    if (progressListener != null) {
                        if (curEntry % progressStep == 0 || curEntry == filmEntries) {
                            progressListener.progress(curEntry / (double) filmEntries);
                        }
                    }
                }
                jg.writeEndObject();

                if (progressListener != null)
                    progressListener.progress(1d);

                long end = System.nanoTime();

                logger.info("   --> geschrieben!");
                logger.trace("Write duration: {} ms", TimeUnit.MILLISECONDS.convert(end - start, TimeUnit.NANOSECONDS));
            }
        } catch (Exception ex) {
            logger.error("nach: {}", datei, ex);
        }

        MessageBus.getMessageBus().publishAsync(new FilmListWriteStopEvent());
    }

    private void writeDatumLong(DatenFilm datenFilm, JsonGenerator jg) {
        var filmDate = datenFilm.getDatumFilm();
        if (filmDate.equals(DatumFilm.UNDEFINED_FILM_DATE)) {
            jg.writeString("");
        }
        else {
            var time_sec = TimeUnit.SECONDS.convert(filmDate.getTime(), TimeUnit.MILLISECONDS);
            var str = String.valueOf(time_sec);
            jg.writeString(str);
        }
    }

    private void writeFilmLength(DatenFilm datenFilm, JsonGenerator jg) {
        jg.writeString(datenFilm.getFilmLengthAsString());
    }

    private void writeEntry(DatenFilm film, JsonGenerator jg) {
        jg.writeArrayPropertyStart(TAG_JSON_LIST);

        writeSender(jg, film);
        writeThema(jg, film);
        writeTitel(jg, film);
        jg.writeString(film.getSendeDatum());
        writeZeit(jg, film);
        writeFilmLength(film, jg);
        jg.writeString(film.getFileSize().toString());
        jg.writeString(film.getDescription());
        jg.writeString(film.getUrlNormalQuality());
        jg.writeString(film.getWebsiteUrl());
        jg.writeString(film.getSubtitleUrl());
        skipEntry(jg); //DatenFilm.FILM_URL_RTMP
        writeLowQualityUrl(jg, film);
        skipEntry(jg); //DatenFilm.URL_RTMP_KLEIN
        writeHighQualityUrl(jg, film);
        skipEntry(jg); //DatenFilm.FILM_URL_RTMP_HD
        writeDatumLong(film, jg);
        skipEntry(jg); //DatenFilm.FILM_URL_HISTORY
        if (!film.hasCountries())
            jg.writeString("");
        else
            jg.writeString(film.getCountriesAsString());
        jg.writeString(Boolean.toString(film.isNew()));

        jg.writeEndArray();
    }

    private void writeLowQualityUrl(@NotNull JsonGenerator jg, @NotNull DatenFilm datenFilm) {
        String url = datenFilm.getLowQualityUrl();
        if (decompressUrls) {
            if (DatenFilm.isCompressedUrl(url)) {
                url = datenFilm.decompressUrl(url);
            }
        }

        jg.writeString(url);
    }

    private void writeHighQualityUrl(@NotNull JsonGenerator jg, @NotNull DatenFilm datenFilm) {
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

    private void skipEntry(JsonGenerator jg) {
        jg.writeString("");
    }

    private void writeTitel(JsonGenerator jg, DatenFilm datenFilm) {
        jg.writeString(datenFilm.getTitle());
    }

    private void writeSender(JsonGenerator jg, DatenFilm datenFilm) {
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

    private void writeThema(JsonGenerator jg, DatenFilm datenFilm) {
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

    private void writeZeit(JsonGenerator jg, DatenFilm datenFilm) {
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
    private void writeFormatDescription(JsonGenerator jg) {
        jg.writeArrayPropertyStart(FILMLISTE);
        jg.writeString("");
        jg.writeEndArray();
    }

    private static class PrettyObjectWriteContext extends ObjectWriteContext.Base {
        @Override
        public boolean hasPrettyPrinter() {
            return true;
        }

        @Override
        public DefaultPrettyPrinter getPrettyPrinter() {
            return new DefaultPrettyPrinter();
        }
    }

    @FunctionalInterface
    public interface IProgressListener {
        void progress(double current);
    }
}
