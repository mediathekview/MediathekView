package mediathek.tool;

import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import org.apache.commons.text.WordUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Optional;

public class MVInfoFile {

    private static final Logger logger = LogManager.getLogger(MVInfoFile.class);
    private static final String FILM_GROESSE = "Größe [MB]";
    private static final String FILM_SENDER = "Sender";
    private static final String FILM_THEMA = "Thema";
    private static final String FILM_TITEL = "Titel";
    private static final String FILM_DATUM = "Datum";
    private static final String FILM_ZEIT = "Zeit";
    private static final String FILM_DAUER = "Dauer";
    private static final String FILM_URL = "URL";

    protected static String formatFilmAsString(DatenFilm film, int maxLengthHeaders) {

        if (null == film)
            return "";

        String formatString = String.format("%%-%ds %%s", maxLengthHeaders);

        StringBuilder sb = new StringBuilder();

        sb = appendFormatedTableLine(sb, formatString, FILM_SENDER, film.getSender());
        sb = appendFormatedTableLine(sb, formatString, FILM_THEMA, film.getThema()).append(System.lineSeparator());
        sb = appendFormatedTableLine(sb, formatString, FILM_TITEL, film.getTitle()).append(System.lineSeparator());
        sb = appendFormatedTableLine(sb, formatString, FILM_DATUM, film.getSendeDatum());
        sb = appendFormatedTableLine(sb, formatString, FILM_ZEIT, film.getSendeZeit());
        sb = appendFormatedTableLine(sb, formatString, FILM_DAUER, film.getDauer());
        sb = appendFormatedTableLine(sb, formatString, FILM_GROESSE, film.getSize()).append(System.lineSeparator());

        sb.append("Website");
        sb.append(System.lineSeparator());
        sb.append(film.getWebsiteLink());
        sb.append(System.lineSeparator());
        sb.append(System.lineSeparator());

        sb.append(FILM_URL);
        sb.append(System.lineSeparator());
        sb.append(film.getUrl());
        sb.append(System.lineSeparator());
        sb.append(System.lineSeparator());

        sb.append(splittStringIntoMaxFixedLengthLines(film.getDescription(), 62));
        sb.append(System.lineSeparator());
        sb.append(System.lineSeparator());

        return sb.toString();

    }

    protected static StringBuilder appendFormatedTableLine(StringBuilder sb, String formatString, String keyTitle, String value) {
        return sb.append(String.format(formatString, String.format("%s:", keyTitle), value))
                .append(System.lineSeparator());
    }

    public static String splittStringIntoMaxFixedLengthLines(String input, int lineLength) {
        return Optional.ofNullable(input)
                .map(s -> WordUtils.wrap(s, lineLength))
                .orElse("");
    }

    public static int getMaxLengthFromStringArray(String[] array) {
        return Optional.ofNullable(array).stream().flatMap(Arrays::stream)
                .max(Comparator.comparing(String::length))
                .map(String::length)
                .orElse(0);
    }

    public void writeInfoFile(DatenFilm film, @NotNull Path path) throws IOException {
        logger.info("Infofile schreiben nach: {}", path.toAbsolutePath().toString());
        path.toFile().getParentFile().mkdirs();

        try (OutputStream os = Files.newOutputStream(path);
             DataOutputStream dos = new DataOutputStream(os);
             OutputStreamWriter osw = new OutputStreamWriter(dos);
             BufferedWriter br = new BufferedWriter(osw)) {
            br.write(formatFilmAsString(film, FILM_GROESSE.length() + 2));
            br.flush();
        }

        logger.info("Infodatei geschrieben");
    }

    public void writeInfoFile(@NotNull DatenDownload datenDownload) throws IOException {
        new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]).mkdirs();
        final Path path = Paths.get(datenDownload.getFileNameWithoutSuffix() + ".txt");

        final DatenFilm film = datenDownload.film;
        if (film != null) {
            writeInfoFile(film, path);
        }
    }
}
