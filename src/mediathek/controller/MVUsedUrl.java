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
package mediathek.controller;

import mediathek.tool.GuiFunktionen;
import msearch.tool.GermanStringSorter;

public class MVUsedUrl implements Comparable<MVUsedUrl> {

    public static String[] title = {"Datum", "Thema", "Titel", "Url"};
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    private final static String TRENNER = "  |###|  ";
    private final static String PAUSE = " |#| ";

    String[] uUrl;

    public MVUsedUrl(String date, String thema, String title, String url) {
        this.uUrl = new String[]{date, thema, title, url};
    }

    public static String getUsedUrl(String date, String thema, String title, String url) {
        return date + PAUSE
                + GuiFunktionen.textLaenge(25, putzen(thema), false /* mitte */, false /*addVorne*/) + PAUSE
                + GuiFunktionen.textLaenge(40, putzen(title), false /* mitte */, false /*addVorne*/) + TRENNER
                + url + "\n";
    }

    public String getUsedUrl() {
        return uUrl[0] + PAUSE
                + GuiFunktionen.textLaenge(25, putzen(uUrl[1]), false /* mitte */, false /*addVorne*/) + PAUSE
                + GuiFunktionen.textLaenge(40, putzen(uUrl[2]), false /* mitte */, false /*addVorne*/) + TRENNER
                + uUrl[3] + "\n";
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
            Log.fehlerMeldung(398853224, Log.FEHLER_ART_PROG, "ErledigteAbos.getUrlAusZeile: " + zeile, ex);
        }
        return new MVUsedUrl(datum, thema, titel, url);
    }

    public static String getHeaderString() {
        return GuiFunktionen.textLaenge(40, title[2], false /* mitte */, false /*addVorne*/)
                + "    " + GuiFunktionen.textLaenge(25, title[1], false /* mitte */, false /*addVorne*/)
                + "    " + GuiFunktionen.textLaenge(10, title[0], false /* mitte */, false /*addVorne*/)
                + "    " + title[3];
    }

    public String getString() {
        return GuiFunktionen.textLaenge(40, uUrl[2], false /* mitte */, false /*addVorne*/)
                + "    " + GuiFunktionen.textLaenge(25, uUrl[1], false /* mitte */, false /*addVorne*/)
                + "    " + (uUrl[0].isEmpty() ? "          " : uUrl[0])
                + "    " + uUrl[3];
    }

    public String getUrl() {
        return uUrl[3];
    }

    @Override
    public int compareTo(MVUsedUrl arg0) {
        return sorter.compare(uUrl[2], arg0.uUrl[2]);
    }

    private static String putzen(String s) {
        s = s.replace("\n", "");
        s = s.replace("|", "");
        s = s.replace(TRENNER, "");
        return s;
    }
}
