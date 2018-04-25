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

import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mSearch.tool.TimedTextMarkupLanguageParser;
import mediathek.daten.DatenDownload;

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

public class MVSubtitle {
    public static void writeSubtitle( DatenDownload datenDownload) {
        final String SUFFIX_TTML = "ttml";
        final String SUFFIX_SRT = "srt";
        String suffix = SUFFIX_TTML;// txt käme dem Infofile in die Quere
        String urlSubtitle = "";
        String strSubtitelFile = null;
        File subtitelFile;
        HttpURLConnection conn = null;
        InputStream in = null;
        String encoding;

        if (datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE].isEmpty()) {
            return;
        }
        try {
            SysMsg.sysMsg(new String[]{"Untertitel: ", datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE],
                "schreiben nach: ", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]});

            urlSubtitle = datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE];
            suffix = GuiFunktionen.getSuffixFromUrl(urlSubtitle);
            if (!suffix.endsWith(SUFFIX_SRT)) {
                suffix = SUFFIX_TTML;
            }
            strSubtitelFile = datenDownload.getFileNameWithoutSuffix() + '.' + suffix;
            subtitelFile = new File(strSubtitelFile);

            new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]).mkdirs();

            conn = (HttpURLConnection) new URL(urlSubtitle).openConnection();
            conn.setRequestProperty("User-Agent",
                    ApplicationConfiguration.getConfiguration()
                            .getString(ApplicationConfiguration.APPLICATION_USER_AGENT));

            conn.setRequestProperty("Accept-Encoding", "gzip, deflate");
            conn.setReadTimeout(10_000);
            conn.setConnectTimeout(10_000);
            // the encoding returned by the server
            encoding = conn.getContentEncoding();
            if ((conn.getResponseCode()) < 400) {
                in = conn.getInputStream();
            } else {
                // dann wars das
                Log.errorLog(752301248, "url: " + urlSubtitle);
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

            try (FileOutputStream fos = new FileOutputStream(subtitelFile)) {
                final byte[] buffer = new byte[1024];
                int n;
                while ((n = in.read(buffer)) != -1) {
                    fos.write(buffer, 0, n);
                }
                SysMsg.sysMsg(new String[]{"Untertitel", "  geschrieben"});
            }
        } catch (IOException ex) {
            strSubtitelFile = null;
            if (conn != null) {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (Exception ignored) {
                }
            }
        } catch (Exception ignored) {
            strSubtitelFile = null;
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (Exception ignored) {
            }
        }

        try (TimedTextMarkupLanguageParser ttmlp = new TimedTextMarkupLanguageParser()) {
            if (strSubtitelFile != null) {
                if (!strSubtitelFile.endsWith('.' + SUFFIX_SRT)) {
                    Path p = new File(strSubtitelFile).toPath();
                    Path srt = new File(datenDownload.getFileNameWithoutSuffix() + "." + SUFFIX_SRT).toPath();
                    if (ttmlp.parse(p)) {
                        ttmlp.toSrt(srt);
                    } else if (ttmlp.parseXmlFlash(p)) {
                        ttmlp.toSrt(srt);
                    }
                }
            }
        } catch (Exception ignored) {
            Log.errorLog(461203210, ignored, "SubtitelUrl: " + urlSubtitle);
        }
    }
}
