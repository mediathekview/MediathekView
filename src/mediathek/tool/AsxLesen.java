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
package mediathek.tool;

import mediathek.controller.Log;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import mediathek.daten.Daten;

public class AsxLesen {

    public static final int STRING_BUFFER_START_BUFFER = 10 * 1024 * 8; // 10 KiB

    public static String lesen(String datei) {
        if (!datei.toLowerCase().endsWith(".asx")) {
            // dann ist es keine "asx" URL
            return datei;
        }
        String url = datei;
        StringBuilder seite = new StringBuilder(STRING_BUFFER_START_BUFFER);
        String kodierung = Konstanten.KODIERUNG_UTF;
        int timeout = 20000; //10 Sekunden
        char[] zeichen = new char[1];
        URLConnection conn;
        InputStream in = null;
        InputStreamReader inReader = null;
        try {
            conn = new URL(datei).openConnection();
            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            conn.setReadTimeout(timeout);
            conn.setConnectTimeout(timeout);
            in = conn.getInputStream();
            inReader = new InputStreamReader(in, kodierung);
            while (inReader.read(zeichen) != -1) {
                seite.append(zeichen);
            }
            url = getUrl(seite);
            if (url.equals("")) {
                // so wird die Filmurl oder die URL der ASX-Datei zurückgegeben
                url = datei;
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(946201406, Log.FEHLER_ART_PROG, "AsxLesen.lesen", ex, datei);
        } finally {
            try {
                if (in != null) {
                    inReader.close();
                }
            } catch (IOException ex) {
            }
        }
        return url;
    }

    private static String getUrl(StringBuilder buff) {
        String ret = "";
        final String URL = "href=\"";
        int pos1 = 0, pos2;
        while ((pos1 = buff.indexOf(URL, pos1)) != -1) {
            pos1 += URL.length();
            pos2 = buff.indexOf("\"", pos1);
            if (pos1 != -1 && pos2 != -1) {
                ret += buff.substring(pos1, pos2) + " ";
            }
        }
        return ret.trim();
    }
}
//<ASX version ="3.0">
//    <Entry>
//        <Ref href="mms://a1014.v1252931.c125293.g.vm.akamaistream.net/7/1014/125293/v0001/wm.od.origin.zdf.de.gl-systemhaus.de/none/zdf/12/03/120323_seifert_asp_vh.wmv"/>
//    </Entry>
//</ASX>
