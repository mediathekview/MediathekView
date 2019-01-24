/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.tool;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import mSearch.daten.DatenFilm;
import mSearch.tool.FilenameUtils;
import mSearch.tool.javafx.FXErrorDialog;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.dialog.DialogZiel;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Optional;
import java.util.stream.Stream;

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
        return Optional.ofNullable(array)
                .map(Arrays::stream)
                .orElseGet(Stream::empty)
                .max(Comparator.comparing(String::length))
                .map(String::length)
                .orElse(0);
    }

    public void writeInfoFile(DatenFilm film) {
        String titel = FilenameUtils.replaceLeerDateiname(film.getTitle(), false,
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));
        String pfad = "";
        ListePset lp = Daten.listePset.getListeSpeichern();
        if (!lp.isEmpty()) {
            DatenPset p = lp.get(0);
            pfad = p.getZielPfad();
        }
        if (pfad.isEmpty()) {
            pfad = GuiFunktionen.getStandardDownloadPath();
        }

        if (titel.isEmpty()) {
            titel = StringUtils.replace(film.getSender(), " ", "-") + ".txt";
        } else {
            titel += ".txt";
        }

        pfad = GuiFunktionen.addsPfad(pfad, titel);
        DialogZiel dialog = new DialogZiel(null, true, pfad, "Infos speichern");
        dialog.setVisible(true);
        if (!dialog.ok) {
            return;
        }

        final Path path = Paths.get(dialog.ziel);
        path.toFile().getParentFile().mkdirs();

        try (OutputStream os = Files.newOutputStream(path);
             DataOutputStream dos = new DataOutputStream(os);
             OutputStreamWriter osw = new OutputStreamWriter(dos);
             BufferedWriter br = new BufferedWriter(osw)) {
            br.write(formatFilmAsString(film, FILM_GROESSE.length() + 2));
            br.flush();

            showSuccessDialog();
        } catch (IOException ex) {
            FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, "Infodatei schreiben", "Ein unbekannter Fehler ist aufgetreten!", ex);
            logger.error("Ziel: {}", dialog.ziel, ex);
        }
    }

    private void showSuccessDialog() {
        Platform.runLater(() -> {
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle(Konstanten.PROGRAMMNAME);
            alert.setHeaderText("Infodatei schreiben");
            alert.setContentText("Infodatei wurde erfolgreich geschrieben.");
            alert.showAndWait();
        });
    }

    public void writeInfoFile(DatenDownload datenDownload) {
        logger.info("Infofile schreiben nach: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]);

        new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]).mkdirs();
        final Path path = Paths.get(datenDownload.getFileNameWithoutSuffix() + ".txt");

        try (OutputStream os = Files.newOutputStream(path);
             DataOutputStream dos = new DataOutputStream(os);
             OutputStreamWriter osw = new OutputStreamWriter(dos);
             BufferedWriter br = new BufferedWriter(osw)) {
            final DatenFilm film = datenDownload.film;
            if (film != null) {
                br.write(formatFilmAsString(film, FILM_GROESSE.length() + 2));
                br.flush();
            }

            logger.info("Infodatei geschrieben");
        } catch (IOException ex) {
            FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, "Infodatei schreiben", "Ein unbekannter Fehler ist aufgetreten!", ex);
            logger.error("Ziel: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME], ex);
        }
    }

}
