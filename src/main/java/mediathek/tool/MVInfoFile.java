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
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.dialog.DialogZiel;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class MVInfoFile {

    private static final Logger logger = LogManager.getLogger(MVInfoFile.class);

    private void writeSender(BufferedWriter br, DatenFilm film) throws IOException {
        br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_SENDER] + ":      " + film.getSender());
        br.newLine();
    }

    private void writeThema(BufferedWriter br, DatenFilm film) throws IOException {
        br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_THEMA] + ":       " + film.getThema());
        br.newLine();
        br.newLine();
    }

    private void writeTitle(BufferedWriter br, DatenFilm film) throws IOException {
        br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_TITEL] + ":       " + film.getTitle());
        br.newLine();
        br.newLine();
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
        if (pfad.isEmpty())
            pfad = GuiFunktionen.getStandardDownloadPath();

        if (titel.isEmpty())
            titel = StringUtils.replace(film.getSender(), " ", "-") + ".txt";
        else
            titel += ".txt";

        pfad = GuiFunktionen.addsPfad(pfad, titel);
        DialogZiel dialog = new DialogZiel(null, true, pfad, "Infos speichern");
        dialog.setVisible(true);
        if (!dialog.ok)
            return;


        final Path path = Paths.get(dialog.ziel);
        path.toFile().getParentFile().mkdirs();

        try (OutputStream os = Files.newOutputStream(path);
             DataOutputStream dos = new DataOutputStream(os);
             OutputStreamWriter osw = new OutputStreamWriter(dos);
             BufferedWriter br = new BufferedWriter(osw)) {
            writeSender(br, film);
            writeThema(br, film);
            writeTitle(br, film);
            writeDatum(br, film);
            writeZeit(br, film);
            writeDauer(br, film);
            br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_GROESSE] + ":  " + film.arr[DatenFilm.FILM_GROESSE]);
            br.newLine();
            br.newLine();
            writeWebsiteLink(br, film);
            writeUrl(br, film);

            int anz = 0;
            for (String s : film.getDescription().split(" ")) {
                anz += s.length();
                br.write(s + ' ');
                if (anz > 50) {
                    br.newLine();
                    anz = 0;
                }
            }
            br.newLine();
            br.newLine();
            br.flush();

            showSuccessDialog();
        } catch (IOException ex) {
            showErrorDialog();
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

    private void showErrorDialog() {
        Platform.runLater(() -> {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle(Konstanten.PROGRAMMNAME);
            alert.setHeaderText("Infodatei schreiben");
            alert.setContentText("Es trat ein unbekannter Fehler beim Schreiben auf.");
            alert.showAndWait();
        });
    }

    private void writeDatum(BufferedWriter br, DatenFilm film) throws IOException {
        br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DATUM] + ":       " + film.arr[DatenFilm.FILM_DATUM]);
        br.newLine();
    }

    private void writeUrl(BufferedWriter br, DatenFilm film) throws IOException {
        br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_URL]);
        br.newLine();
        br.write(film.arr[DatenFilm.FILM_URL]);
        br.newLine();
        br.newLine();
    }

    private void writeWebsiteLink(BufferedWriter br, DatenFilm film) throws IOException {
        br.write("Website");
        br.newLine();
        br.write(film.getWebsiteLink());
        br.newLine();
        br.newLine();
    }

    private void writeZeit(BufferedWriter br, DatenFilm film) throws IOException {
        br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_ZEIT] + ":        " + film.arr[DatenFilm.FILM_ZEIT]);
        br.newLine();
    }

    private void writeDauer(BufferedWriter br, DatenFilm film) throws IOException {
        br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DAUER] + ":       " + film.arr[DatenFilm.FILM_DAUER]);
        br.newLine();
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
                writeSender(br, film);
                writeThema(br, film);
                writeTitle(br, film);
                writeDatum(br, film);
                writeZeit(br, film);
                writeDauer(br, film);
                br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_GROESSE] + ":  " + datenDownload.mVFilmSize);
                br.newLine();
                br.newLine();
                writeWebsiteLink(br, film);
            }

            br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_URL]);
            br.newLine();
            br.write(datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
            br.newLine();
            br.newLine();

            if (film != null) {
                int anz = 0;
                for (String s : datenDownload.film.getDescription().split(" ")) {
                    anz += s.length();
                    br.write(s + ' ');
                    if (anz > 50) {
                        br.newLine();
                        anz = 0;
                    }
                }
            }
            br.newLine();
            br.newLine();
            br.flush();

            showSuccessDialog();
            logger.info("Infofile geschrieben");
        } catch (IOException ex) {
            showErrorDialog();
            logger.error("Ziel: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
    }

}
