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

import mSearch.daten.DatenFilm;
import mSearch.tool.FilenameUtils;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.dialog.DialogZiel;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class MVInfoFile {

    private static final Logger logger = LogManager.getLogger(MVInfoFile.class);

    public void writeInfoFile(JFrame paFrame, DatenFilm film) {
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
            titel = StringUtils.replace(film.arr[DatenFilm.FILM_SENDER], " ", "-") + ".txt";
        } else {
            titel = titel + ".txt";
        }
        pfad = GuiFunktionen.addsPfad(pfad, titel);
        DialogZiel dialog = new DialogZiel(paFrame, true, pfad, "Infos speichern");
        dialog.setVisible(true);
        if (!dialog.ok) {
            return;
        }

        Path path = Paths.get(dialog.ziel);
        try (OutputStream os = Files.newOutputStream(path);
             DataOutputStream dos = new DataOutputStream(os);
             OutputStreamWriter osw = new OutputStreamWriter(dos);
             BufferedWriter br = new BufferedWriter(osw)) {
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_SENDER] + ":      " + film.arr[DatenFilm.FILM_SENDER]);
            br.write("\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_THEMA] + ":       " + film.arr[DatenFilm.FILM_THEMA]);
            br.write("\n\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_TITEL] + ":       " + film.getTitle());
            br.write("\n\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DATUM] + ":       " + film.arr[DatenFilm.FILM_DATUM]);
            br.write("\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_ZEIT] + ":        " + film.arr[DatenFilm.FILM_ZEIT]);
            br.write("\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DAUER] + ":       " + film.arr[DatenFilm.FILM_DAUER]);
            br.write("\n");
            br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_GROESSE] + ":  " + film.arr[DatenFilm.FILM_GROESSE]);
            br.write("\n\n");

            br.write("Website\n");
            br.write(film.getWebsiteLink());
            br.write("\n\n");

            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_URL] + '\n');
            br.write(film.arr[DatenFilm.FILM_URL]);
            br.write("\n\n");

            int anz = 0;
            for (String s : film.getDescription().split(" ")) {
                anz += s.length();
                br.write(s + ' ');
                if (anz > 50) {
                    br.write("\n");
                    anz = 0;
                }
            }
            br.write("\n\n");
            br.flush();
        } catch (IOException ex) {
            logger.error("Ziel: {}", dialog.ziel, ex);
        }
    }

    public void writeInfoFile(DatenDownload datenDownload) {
        logger.info("Infofile schreiben nach: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]);

        new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]).mkdirs();
        Path path = Paths.get(datenDownload.getFileNameWithoutSuffix() + ".txt");

        try (OutputStream os = Files.newOutputStream(path);
             DataOutputStream dos = new DataOutputStream(os);
             OutputStreamWriter osw = new OutputStreamWriter(dos);
             BufferedWriter br = new BufferedWriter(osw)) {
            if (datenDownload.film != null) {
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_SENDER] + ":      " + datenDownload.film.arr[DatenFilm.FILM_SENDER]);
                br.write("\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_THEMA] + ":       " + datenDownload.film.arr[DatenFilm.FILM_THEMA]);
                br.write("\n\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_TITEL] + ":       " + datenDownload.film.getTitle());
                br.write("\n\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DATUM] + ":       " + datenDownload.film.arr[DatenFilm.FILM_DATUM]);
                br.write("\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_ZEIT] + ":        " + datenDownload.film.arr[DatenFilm.FILM_ZEIT]);
                br.write("\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DAUER] + ":       " + datenDownload.film.arr[DatenFilm.FILM_DAUER]);
                br.write("\n");
                br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_GROESSE] + ":  " + datenDownload.mVFilmSize);
                br.write("\n\n");

                br.write("Website\\n");
                br.write(datenDownload.film.getWebsiteLink());
                br.write("\n\n");
            }

            br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_URL] + '\n');
            br.write(datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
            br.write("\n\n");

            if (datenDownload.film != null) {
                int anz = 0;
                for (String s : datenDownload.film.getDescription().split(" ")) {
                    anz += s.length();
                    br.write(s + ' ');
                    if (anz > 50) {
                        br.write("\n");
                        anz = 0;
                    }
                }
            }
            br.write("\n\n");
            br.flush();

            logger.info("Infofile geschrieben");
        } catch (IOException ex) {
            logger.error("Ziel: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
    }

}
