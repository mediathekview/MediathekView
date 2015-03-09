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
import java.util.zip.GZIPInputStream;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;

public class MVSubtitle {

    private static final int timeout = 10000;
    public static final String KODIERUNG_UTF = "UTF-8";

    public static void writeSubtitle(DatenDownload datenDownload) {
        String url;
        File file;
        HttpURLConnection conn = null;
        InputStream in = null;
        FileOutputStream fos;
        String encoding;

        if (datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE_NR].isEmpty()) {
            return;
        }
        try {
            url = datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE_NR];
            new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]).mkdirs();
            String suff = "txt";
            if (url.contains(".")) {
                suff = url.substring(url.lastIndexOf("."));
            }
            file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR] + suff);

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
                switch(encoding.toLowerCase()) {
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
        } catch (IOException ex) {
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
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (Exception ignored) {
            }
        }
    }
}
