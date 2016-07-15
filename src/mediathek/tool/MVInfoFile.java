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

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.swing.JFrame;
import mSearch.daten.DatenFilm;
import mSearch.tool.FilenameUtils;
import mSearch.tool.Log;
import mSearch.tool.MVConfig;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.dialog.DialogZiel;

public class MVInfoFile {

    public static void writeInfoFile(JFrame paFrame, Daten daten, DatenFilm film) {
        String titel = film.arr[DatenFilm.FILM_TITEL];
        titel = FilenameUtils.replaceLeerDateiname(titel, false /*pfad*/,
                Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_USE_REPLACETABLE)),
                Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_ONLY_ASCII)));
        String pfad = "";
        ListePset lp = Daten.listePset.getListeSpeichern();
        if (lp.size() > 0) {
            DatenPset p = lp.get(0);
            pfad = p.getZielPfad();
        }
        if (pfad.isEmpty()) {
            pfad = GuiFunktionen.getStandardDownloadPath();
        }
        if (titel.isEmpty()) {
            titel = film.arr[DatenFilm.FILM_SENDER].replace(" ", "-") + ".txt";
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
        try (BufferedWriter br = new BufferedWriter(new OutputStreamWriter(new DataOutputStream(Files.newOutputStream(path))))) {
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_SENDER] + ":      " + film.arr[DatenFilm.FILM_SENDER]);
            br.write("\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_THEMA] + ":       " + film.arr[DatenFilm.FILM_THEMA]);
            br.write("\n\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_TITEL] + ":       " + film.arr[DatenFilm.FILM_TITEL]);
            br.write("\n\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DATUM] + ":       " + film.arr[DatenFilm.FILM_DATUM]);
            br.write("\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_ZEIT] + ":        " + film.arr[DatenFilm.FILM_ZEIT]);
            br.write("\n");
            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DAUER] + ":       " + film.arr[DatenFilm.FILM_DAUER]);
            br.write("\n");
            br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_GROESSE] + ":  " + film.arr[DatenFilm.FILM_GROESSE]);
            br.write("\n\n");

            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_WEBSEITE] + "\n");
            br.write(film.arr[DatenFilm.FILM_WEBSEITE]);
            br.write("\n\n");

            br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_URL] + "\n");
            br.write(film.arr[DatenFilm.FILM_URL]);
            br.write("\n\n");
            if (!film.arr[DatenFilm.FILM_URL_RTMP].isEmpty()) {
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_URL_RTMP] + "\n");
                br.write(film.arr[DatenFilm.FILM_URL_RTMP]);
                br.write("\n\n");
            }

            int anz = 0;
            for (String s : film.arr[DatenFilm.FILM_BESCHREIBUNG].split(" ")) {
                anz += s.length();
                br.write(s + " ");
                if (anz > 50) {
                    br.write("\n");
                    anz = 0;
                }
            }
            br.write("\n\n");
            br.flush();
        } catch (IOException ex) {
            Log.errorLog(632656214, dialog.ziel);
        }
    }

    public static void writeInfoFile(DatenDownload datenDownload) {
        try {
            SysMsg.sysMsg(new String[]{"Infofile schreiben nach: ", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]});

            new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]).mkdirs();
            Path path = Paths.get(datenDownload.getFileNameWithoutSuffix() + ".txt");
            BufferedWriter br = new BufferedWriter(new OutputStreamWriter(new DataOutputStream(Files.newOutputStream(path))));
            if (datenDownload.film != null) {
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_SENDER] + ":      " + datenDownload.film.arr[DatenFilm.FILM_SENDER]);
                br.write("\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_THEMA] + ":       " + datenDownload.film.arr[DatenFilm.FILM_THEMA]);
                br.write("\n\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_TITEL] + ":       " + datenDownload.film.arr[DatenFilm.FILM_TITEL]);
                br.write("\n\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DATUM] + ":       " + datenDownload.film.arr[DatenFilm.FILM_DATUM]);
                br.write("\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_ZEIT] + ":        " + datenDownload.film.arr[DatenFilm.FILM_ZEIT]);
                br.write("\n");
                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_DAUER] + ":       " + datenDownload.film.arr[DatenFilm.FILM_DAUER]);
                br.write("\n");
                br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_GROESSE] + ":  " + datenDownload.mVFilmSize);
                br.write("\n\n");

                br.write(DatenFilm.COLUMN_NAMES[DatenFilm.FILM_WEBSEITE] + "\n");
                br.write(datenDownload.film.arr[DatenFilm.FILM_WEBSEITE]);
                br.write("\n\n");
            }

            br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_URL] + "\n");
            br.write(datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
            br.write("\n\n");
            if (!datenDownload.arr[DatenDownload.DOWNLOAD_URL_RTMP].isEmpty()
                    && !datenDownload.arr[DatenDownload.DOWNLOAD_URL_RTMP].equals(datenDownload.arr[DatenDownload.DOWNLOAD_URL])) {
                br.write(DatenDownload.COLUMN_NAMES[DatenDownload.DOWNLOAD_URL_RTMP] + "\n");
                br.write(datenDownload.arr[DatenDownload.DOWNLOAD_URL_RTMP]);
                br.write("\n\n");
            }

            if (datenDownload.film != null) {
                int anz = 0;
                for (String s : datenDownload.film.arr[DatenFilm.FILM_BESCHREIBUNG].split(" ")) {
                    anz += s.length();
                    br.write(s + " ");
                    if (anz > 50) {
                        br.write("\n");
                        anz = 0;
                    }
                }
            }
            br.write("\n\n");
            br.flush();
            br.close();
            SysMsg.sysMsg(new String[]{"Infofile", "  geschrieben"});
        } catch (IOException ex) {
            Log.errorLog(975410369, datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
    }

}
