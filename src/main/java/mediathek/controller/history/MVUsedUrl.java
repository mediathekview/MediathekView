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
package mediathek.controller.history;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Legacy class used to read entries from old history text files.
 * Should NOT be used in further developments.
 */
public class MVUsedUrl {
    private static final Logger logger = LogManager.getLogger(MVUsedUrl.class);
    private final static String TRENNER = "  |###|  ";
    private final static String PAUSE = " |#| ";
    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    private final LocalDate datum;
    private final String thema;
    private final String titel;
    private final String url;

    public MVUsedUrl(String date, String thema, String title, String url) {
        LocalDate tempDate;
        try {
            tempDate = LocalDate.parse(date, DATE_TIME_FORMATTER);
        }
        catch (DateTimeException ex) {
            logger.error("Failed to parse date: \"{}\" for thema: \"{}\", titel: \"{}\" and URL: \"{}\"", date, thema, title, url);
            logger.error("Resetting date to current date...");
            tempDate = LocalDate.now();
        }
        this.datum = tempDate;
        this.thema = thema;
        this.titel = title;
        this.url = url;
    }

    public MVUsedUrl(String thema, String title, String url) {
        this.datum = LocalDate.now();
        this.thema = thema;
        this.titel = title;
        this.url = url;
    }

    public static MVUsedUrl getUrlAusZeile(String zeile) {
        // 29.05.2014 |#| Abendschau                |#| Patenkind trifft Groß                     |###|  http://cdn-storage.br.de/iLCpbHJGNLT6NK9HsLo6s61luK4C_2rc5U1S/_-OS/5-8y9-NP/5bb33365-038d-46f7-914b-eb83fab91448_E.mp4
        String url = "", thema = "", titel = "", datum = "";
        int a1;
        try {
            if (zeile.contains(TRENNER)) {
                //neues Logfile-Format
                a1 = zeile.lastIndexOf(TRENNER);
                a1 += TRENNER.length();
                url = zeile.substring(a1).trim();
                // titel
                titel = zeile.substring(zeile.lastIndexOf(PAUSE) + PAUSE.length(), zeile.lastIndexOf(TRENNER)).trim();
                datum = zeile.substring(0, zeile.indexOf(PAUSE)).trim();
                thema = zeile.substring(zeile.indexOf(PAUSE) + PAUSE.length(), zeile.lastIndexOf(PAUSE)).trim();
            } else {
                url = zeile;
            }
        } catch (Exception ex) {
            logger.error("getUrlAusZeile", ex);
        }
        return new MVUsedUrl(datum, thema, titel, url);
    }

    protected static String putzen(String s) {
        return s.replace("\n", "")
                .replace("|", "")
                .replace(TRENNER, "");
    }


    public String getDatum() {
        return DATE_TIME_FORMATTER.format(datum);
    }

    public String getThema() {
        return thema;
    }

    public String getTitel() {
        return titel;
    }

    /**
     * Creates the string for one row in the data file.
     *
     * @return one row with data.
     */
    public String getPreparedRowString() {
        return String.format("%s%s%s%s%s%s%s%n",
                getDatum(), PAUSE,
                putzen(thema), PAUSE,
                putzen(titel), TRENNER,
                url);
    }

    public String getUrl() {
        return url;
    }
}
