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
package mSearch.filmlisten.writer;

import com.fasterxml.jackson.core.JsonEncoding;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.jidesoft.utils.SystemInfo;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.tool.ApplicationConfiguration;
import mediathek.config.Daten;
import mediathek.gui.messages.FilmListWriteStartEvent;
import mediathek.gui.messages.FilmListWriteStopEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

public class FilmListWriter {

    private static final Logger logger = LogManager.getLogger(FilmListWriter.class);
    private static final String TAG_JSON_LIST = "X";
    private String sender = "";
    private String thema = "";
    private boolean fireEvents = true;

    private JsonGenerator getJsonGenerator(OutputStream os) throws IOException {
        final JsonFactory jsonF = new JsonFactory();
        JsonGenerator jg = jsonF.createGenerator(os, JsonEncoding.UTF8);

        final boolean readable = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FILMLISTE_SAVE_HUMAN_READABLE, false);
        if (readable)
            jg = jg.useDefaultPrettyPrinter();

        return jg;
    }

    public void disableEvents() {
        fireEvents = false;
    }

    private void checkOsxCacheDirectory() {
        final Path filePath = Paths.get(System.getProperty("user.home") + File.separator + "Library/Caches/MediathekView");
        if (Files.notExists(filePath)) {
            try {
                Files.createDirectories(filePath);
            } catch (IOException ioException) {
                ioException.printStackTrace();
            }
        }
    }

    private void writeFormatHeader(JsonGenerator jg, ListeFilme listeFilme) throws IOException {
        jg.writeArrayFieldStart(ListeFilme.FILMLISTE);
        jg.writeString(""); //ListeFilme.FILMLISTE_DATUM_NR unused in newer versions
        jg.writeString(listeFilme.metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR]);
        jg.writeString(listeFilme.metaDaten[2]); //filmlist version
        jg.writeString(""); //filmlist creator program
        jg.writeString(listeFilme.metaDaten[ListeFilme.FILMLISTE_ID_NR]);
        jg.writeEndArray();
    }

    public void writeFilmList(String datei, ListeFilme listeFilme, IProgressListener listener) {
        if (fireEvents)
            Daten.getInstance().getMessageBus().publishAsync(new FilmListWriteStartEvent());

        try {
            logger.info("Filme schreiben ({} Filme) :", listeFilme.size());
            logger.info("   --> Start Schreiben nach: {}", datei);

            sender = "";
            thema = "";

            //Check if Cache directory exists on OSX
            if (SystemInfo.isMacOSX()) {
                checkOsxCacheDirectory();
            }


            Path filePath = Paths.get(datei);
            Files.deleteIfExists(filePath);
            long start = System.nanoTime();

            try (OutputStream fos = Files.newOutputStream(filePath);
                 BufferedOutputStream bos = new BufferedOutputStream(fos, 64 * 1024);
                 JsonGenerator jg = getJsonGenerator(bos)) {

                jg.writeStartObject();

                writeFormatHeader(jg, listeFilme);
                writeFormatDescription(jg);

                final long filmEntries = listeFilme.size();
                double curEntry = 0d;

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
                logger.info("Write duration: {} ms", TimeUnit.MILLISECONDS.convert(end - start, TimeUnit.NANOSECONDS));
            }
        } catch (Exception ex) {
            logger.error("nach: {}", datei, ex);
        }

        if (fireEvents)
            Daten.getInstance().getMessageBus().publishAsync(new FilmListWriteStopEvent());
    }

    private void writeEntry(DatenFilm datenFilm, JsonGenerator jg) throws IOException {
        jg.writeArrayFieldStart(TAG_JSON_LIST);

        writeSender(jg, datenFilm);
        writeThema(jg, datenFilm);
        writeTitel(jg, datenFilm);
        jg.writeString(datenFilm.arr[DatenFilm.FILM_DATUM]);
        writeZeit(jg, datenFilm);
        jg.writeString(datenFilm.arr[DatenFilm.FILM_DAUER]);
        jg.writeString(datenFilm.arr[DatenFilm.FILM_GROESSE]);
        jg.writeString(datenFilm.getDescription());
        jg.writeString(datenFilm.arr[DatenFilm.FILM_URL]);
        jg.writeString(datenFilm.getWebsiteLink());
        jg.writeString(datenFilm.arr[DatenFilm.FILM_URL_SUBTITLE]);
        skipEntry(jg); //DatenFilm.FILM_URL_RTMP
        jg.writeString(datenFilm.arr[DatenFilm.FILM_URL_KLEIN]);
        skipEntry(jg); //DatenFilm.URL_RTMP_KLEIN
        jg.writeString(datenFilm.arr[DatenFilm.FILM_URL_HD]);
        skipEntry(jg); //DatenFilm.FILM_URL_RTMP_HD
        jg.writeString(datenFilm.arr[DatenFilm.FILM_DATUM_LONG]);
        jg.writeString(datenFilm.arr[DatenFilm.FILM_URL_HISTORY]);
        jg.writeString(datenFilm.arr[DatenFilm.FILM_GEO]);
        jg.writeString(Boolean.toString(datenFilm.isNew()));

        jg.writeEndArray();
    }

    private void skipEntry(JsonGenerator jg) throws IOException {
        jg.writeString("");
    }

    private void writeTitel(JsonGenerator jg, DatenFilm datenFilm) throws IOException {
        jg.writeString(datenFilm.getTitle());
    }

    private void writeSender(JsonGenerator jg, DatenFilm datenFilm) throws IOException {
        String tempSender = datenFilm.getSender();

        if (tempSender.equals(sender)) {
            jg.writeString("");
        } else {
            sender = tempSender;
            jg.writeString(tempSender);
        }
    }

    private void writeThema(JsonGenerator jg, DatenFilm datenFilm) throws IOException {
        if (datenFilm.getThema().equals(thema)) {
            jg.writeString("");
        } else {
            thema = datenFilm.getThema();
            jg.writeString(datenFilm.getThema());
        }
    }

    private void writeZeit(JsonGenerator jg, DatenFilm datenFilm) throws IOException {
        String strZeit = datenFilm.arr[DatenFilm.FILM_ZEIT];
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
