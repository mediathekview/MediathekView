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

import de.mediathekview.mlib.tool.Log;
import de.mediathekview.mlib.tool.SysMsg;
import de.mediathekview.mlib.tool.TimedTextMarkupLanguageParser;
import mediathek.config.Daten;
import mediathek.daten.DatenDownload;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.zip.GZIPInputStream;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;

public class MVSubtitle {

    private static final int TIMEOUT = 10_000;
    private static final String SUFFIX_TTML = "ttml";
    private static final String SUFFIX_SRT = "srt";
    private static final String SRT_FILETYPE = ".srt";

    @SuppressWarnings({"resource", "IOResourceOpenedButNotSafelyClosed"})
    private InputStream getContentDecoder(final String encoding, InputStream in) throws IOException {
        if (encoding != null) {
            InputStream out = null;
            switch (encoding.toLowerCase()) {
                case "gzip":
                    out = new GZIPInputStream(in);
                    break;
                case "deflate":
                    out = new InflaterInputStream(in, new Inflater(true));
                    break;
            }
            return out;
        } else
            return in;
    }

    private void setupConnection(HttpURLConnection conn) {
        conn.setRequestProperty("User-Agent", Daten.getUserAgent());
        conn.setRequestProperty("Accept-Encoding", "gzip, deflate");
        conn.setReadTimeout(TIMEOUT);
        conn.setConnectTimeout(TIMEOUT);
    }

    private void downloadContent(InputStream in, String strSubtitelFile) throws IOException {
        try (FileOutputStream fos = new FileOutputStream(strSubtitelFile)) {
            final byte[] buffer = new byte[65536];
            int n;
            while ((n = in.read(buffer)) != -1) {
                fos.write(buffer, 0, n);
            }
            SysMsg.sysMsg(new String[]{"Untertitel", "  geschrieben"});
        }
    }

    private void writeSrt(String strSubtitelFile, DatenDownload datenDownload) {
        Path p = Paths.get(strSubtitelFile);
        TimedTextMarkupLanguageParser ttmlp = new TimedTextMarkupLanguageParser();
        if (ttmlp.parse(p) || ttmlp.parseXmlFlash(p)) {
            Path srt = Paths.get(datenDownload.getFileNameWithoutSuffix() + SRT_FILETYPE);
            ttmlp.toSrt(srt);
        }
        ttmlp.cleanup();
    }

    public void writeSubtitle(DatenDownload datenDownload) {
        String suffix;
        String urlSubtitle = "";
        InputStream in = null;

        if (datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE].isEmpty())
            return;

        try {
            SysMsg.sysMsg(new String[]{"Untertitel: ", datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE],
                    "schreiben nach: ", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]});

            urlSubtitle = datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE];
            suffix = GuiFunktionen.getSuffixFromUrl(urlSubtitle);
            if (!suffix.endsWith(SUFFIX_SRT))
                suffix = SUFFIX_TTML;

            Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]));

            HttpURLConnection conn = (HttpURLConnection) new URL(urlSubtitle).openConnection();
            setupConnection(conn);
            if ((conn.getResponseCode()) < HttpURLConnection.HTTP_BAD_REQUEST) {
                in = getContentDecoder(conn.getContentEncoding(), conn.getInputStream());

                final String strSubtitelFile = datenDownload.getFileNameWithoutSuffix() + '.' + suffix;
                downloadContent(in, strSubtitelFile);

                if (!strSubtitelFile.endsWith(SRT_FILETYPE))
                    writeSrt(strSubtitelFile, datenDownload);
            } else
                Log.errorLog(752301248, "url: " + urlSubtitle);
        } catch (Exception ignored) {
            Log.errorLog(461203210, ignored, "SubtitelUrl: " + urlSubtitle);
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
