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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Path;
import java.util.zip.GZIPInputStream;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;

public class MVSubtitle {

    private static final int timeout = 10000;
    public static final String KODIERUNG_UTF = "UTF-8";
    private static String subFile = null;

    public static void writeSubtitle(DatenDownload datenDownload) {
        String url;
        File file;
        HttpURLConnection conn = null;
        InputStream in = null;
        FileOutputStream fos = null;
        String encoding;

        if (datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE_NR].isEmpty()) {
            return;
        }
        try {
            Log.systemMeldung(new String[]{"Untertitel: ", datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE_NR],
                "schreiben nach: ", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]});
            url = datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE_NR];
            new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]).mkdirs();
            String suff = ".xml"; // txt käme dem Infofile in die Quere
            if (url.contains(".")) {
                suff = url.substring(url.lastIndexOf("."));

                // erstmal putzen
                String newSuff;
                newSuff = suff.replaceAll(FilenameUtils.REGEXP_ILLEGAL_CHARACTERS_WINDOWS, "--");
                newSuff = newSuff.replaceAll(FilenameUtils.REGEXP_ILLEGAL_CHARACTERS_OTHERS, "--");

                if (!newSuff.equals(suff) || suff.length() > 6 /* zur Sicherheit: .ttml, ..*/) {
                    suff = ".xml";
                }
            }
            file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR] + suff);
            subFile = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR] + suff;

            conn = (HttpURLConnection) new URL(url).openConnection();
            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            conn.setRequestProperty("Accept-Encoding", "gzip, deflate");
            conn.setReadTimeout(timeout);
            conn.setConnectTimeout(timeout);
            // the encoding returned by the server
            encoding = conn.getContentEncoding();
            if ((conn.getResponseCode()) < 400) {
                in = conn.getInputStream();
            } else {
                // dann wars das
                Log.fehlerMeldung(752301248, "url: " + url);
            }

            if (in == null) {
                return;
            }

            if (encoding != null) {
                switch (encoding.toLowerCase()) {
                    case "gzip":
                        in = new GZIPInputStream(in);
                        break;
                    case "deflate":
                        in = new InflaterInputStream(in, new Inflater(true));
                        break;
                }
            }

            fos = new FileOutputStream(file);
            final byte[] buffer = new byte[1024];
            int n;
            while ((n = in.read(buffer)) != -1) {
                fos.write(buffer, 0, n);
            }
            Log.systemMeldung(new String[]{"Untertitel", "  geschrieben"});
        } catch (IOException ex) {
            subFile = null;
            if (conn != null) {
                try {
                    InputStream i = conn.getErrorStream();
                    if (i != null) {
                        i.close();
                    }
                    if (in != null) {
                        in.close();
                    }
                } catch (Exception ignored) {
                }
            }
        } catch (Exception ignored) {
            subFile = null;
        } finally {
            try {
                if (fos != null) {
                    fos.close();
                }
                if (in != null) {
                    in.close();
                }
            } catch (Exception ignored) {
            }
        }
        if (subFile != null) {
            if (!subFile.endsWith(".srt")) {
                TimedTextMarkupLanguageParser ttmlp = new TimedTextMarkupLanguageParser();
                Path p = new File(subFile).toPath();
                Path srt = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR] + ".srt").toPath();
                if (ttmlp.parse(p)) {
                    ttmlp.toSrt(srt);
                } else if (ttmlp.parseXmlFlash(p)) {
                    ttmlp.toSrt(srt);
                }
                ttmlp.cleanup();
            }
        }
    }
}
