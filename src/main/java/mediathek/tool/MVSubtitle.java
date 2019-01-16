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
import mSearch.tool.TimedTextMarkupLanguageParser;
import mediathek.daten.DatenDownload;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

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

    private static final int TIMEOUT = 10000;
    private static final String SUFFIX_SRT = "srt";
    private static final String SUFFIX_TTML = "ttml";
    private static final String SUFFIX_VTT = "vtt";

    private static final Logger logger = LogManager.getLogger(MVSubtitle.class);

    private HttpURLConnection setupConnection(String urlSubtitle) throws IOException {
        final HttpURLConnection conn = (HttpURLConnection) new URL(urlSubtitle).openConnection();
        conn.setRequestProperty("User-Agent",
                ApplicationConfiguration.getConfiguration()
                        .getString(ApplicationConfiguration.APPLICATION_USER_AGENT));

        conn.setRequestProperty("Accept-Encoding", "gzip, deflate");
        conn.setReadTimeout(TIMEOUT);
        conn.setConnectTimeout(TIMEOUT);

        return conn;
    }

    public void writeSubtitle(@NotNull DatenDownload datenDownload) {
        String strSubtitelFile;
        InputStream in = null;
        final String urlSubtitle = datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE];

        if (urlSubtitle.isEmpty())
            return;

        try {
            logger.info("Untertitel {} schreiben nach {}", urlSubtitle, datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]);


            String suffix = GuiFunktionen.getSuffixFromUrl(urlSubtitle);
            if (!suffix.endsWith(SUFFIX_SRT) && !suffix.endsWith(SUFFIX_VTT)) {
                suffix = SUFFIX_TTML;
            }

            strSubtitelFile = datenDownload.getFileNameWithoutSuffix() + '.' + suffix;

            new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]).mkdirs();

            final HttpURLConnection conn = setupConnection(urlSubtitle);
            // the encoding returned by the server
            final String encoding = conn.getContentEncoding();
            final int responseCode = conn.getResponseCode();
            if (responseCode < 400) {
                in = conn.getInputStream();
            } else {
                // dann wars das
                logger.error("HTTP Response Code {} for URL: {}", responseCode, urlSubtitle);
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

            final File subtitleFile = new File(strSubtitelFile);
            try (FileOutputStream fos = new FileOutputStream(subtitleFile)) {
                final byte[] buffer = new byte[64 * 1024];
                int n;
                while ((n = in.read(buffer)) != -1) {
                    fos.write(buffer, 0, n);
                }
                logger.info("Untertitel wurde geschrieben");
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
                if (!strSubtitelFile.endsWith('.' + SUFFIX_SRT) && !strSubtitelFile.endsWith("." + SUFFIX_VTT)) {

                    Path p = new File(strSubtitelFile).toPath();
                    Path srt = new File(datenDownload.getFileNameWithoutSuffix() + "." + SUFFIX_SRT).toPath();
                    if (ttmlp.parse(p)) {
                        ttmlp.toSrt(srt);
                    } else if (ttmlp.parseXmlFlash(p)) {
                        ttmlp.toSrt(srt);
                    }
                }
            }
        } catch (Exception ex) {
            logger.error("Fehler bei Untertitel schreiben:", ex);
        }
    }
}
