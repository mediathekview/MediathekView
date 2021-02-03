package mediathek.tool;

import mediathek.daten.DatenDownload;
import mediathek.tool.http.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class MVSubtitle {

    private static final String SUFFIX_SRT = "srt";
    private static final String SUFFIX_TTML = "ttml";
    private static final String SUFFIX_VTT = "vtt";
    private static final Logger logger = LogManager.getLogger(MVSubtitle.class);

    private void createDirectory(String targetDirectory) {
        try {
            Files.createDirectory(Paths.get(targetDirectory));
        } catch (IOException ignored) {
        }
    }

    private void writeNetworkData(InputStream is, Path ttmlPath) throws IOException {
        try (OutputStream fos = Files.newOutputStream(ttmlPath)) {
            final byte[] buffer = new byte[64 * 1024];
            int n;
            while ((n = is.read(buffer)) != -1) {
                fos.write(buffer, 0, n);
            }
            logger.info("Untertitel-Datei wurde geschrieben");
        }
    }

    public void writeSubtitle(@NotNull DatenDownload datenDownload) {
        final String urlSubtitle = datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE];
        final String targetDirectory = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD];

        if (urlSubtitle.isEmpty())
            return;

        final Request request = new Request.Builder().url(urlSubtitle).get().build();
        try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (body != null && response.isSuccessful()) {
                logger.info("Untertitel {} schreiben nach {}", urlSubtitle, targetDirectory);

                try (InputStream is = body.byteStream()) {
                    String suffix = GuiFunktionen.getSuffixFromUrl(urlSubtitle);
                    if (!suffix.endsWith(SUFFIX_SRT) && !suffix.endsWith(SUFFIX_VTT)) {
                        suffix = SUFFIX_TTML;
                    }

                    createDirectory(targetDirectory);

                    final String strSubtitleFile = datenDownload.getFileNameWithoutSuffix() + '.' + suffix;
                    final Path ttmlPath = Paths.get(strSubtitleFile);

                    writeNetworkData(is, ttmlPath);

                    convertSubtitle(datenDownload, ttmlPath, strSubtitleFile);
                }
            } else {
                //not successful
                logger.error("HTTP Response Code {} for URL: {}", response.code(), urlSubtitle);
            }
        } catch (IOException ex) {
            logger.error("Subtitle exception occured:", ex);
        }
    }

    private void convertSubtitle(DatenDownload datenDownload, Path ttmlPath, String strSubtitleFile) {
        try (TimedTextMarkupLanguageParser ttmlp = new TimedTextMarkupLanguageParser()) {
            if (!strSubtitleFile.endsWith('.' + SUFFIX_SRT) && !strSubtitleFile.endsWith("." + SUFFIX_VTT)) {
                final Path srt = Paths.get(datenDownload.getFileNameWithoutSuffix() + "." + SUFFIX_SRT);
                if (ttmlp.parse(ttmlPath)) {
                    ttmlp.toSrt(srt);
                } else if (ttmlp.parseXmlFlash(ttmlPath)) {
                    ttmlp.toSrt(srt);
                }
                logger.info("Untertitel-Datei wurde konvertiert.");
            }
        } catch (Exception ex) {
            logger.error("Fehler bei Untertitel schreiben:", ex);
        }
    }
}
