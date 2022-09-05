package mediathek.tool;

import mediathek.daten.DatenDownload;
import mediathek.tool.http.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.BufferedSource;
import okio.Okio;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
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

    private void writeNetworkData(@NotNull BufferedSource bufferedSource, @NotNull Path ttmlPath) throws IOException {
        try (var fileSink = Okio.sink(ttmlPath);
             var bufferedSink = Okio.buffer(fileSink)) {
            bufferedSink.writeAll(bufferedSource);
            logger.trace("Untertitel-Datei wurde geschrieben");
        }

    }

    /**
     * Write a subtitle URL to a local file.
     * The original extension (if applicable) will be removed and a .srt and .ttml file will be created.
     *
     * @param url  The URL to the subtitle file.
     * @param file File location; only filename will be used for processing.
     * @throws IOException when an IO exception occurs.
     */
    public void writeSubtitle(@NotNull String url, @NotNull File file) throws IOException {
        if (url.isEmpty())
            return;

        var plainFileName = GuiFunktionen.getFileNameWithoutSuffix(file.getAbsolutePath());

        final Request request = new Request.Builder().url(url).get().build();
        try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (response.isSuccessful()) {
                logger.trace("Untertitel {} schreiben nach {}", url, plainFileName);

                try (var source = body.source()) {
                    String suffix = GuiFunktionen.getSuffixFromUrl(url);
                    if (!suffix.endsWith(SUFFIX_SRT) && !suffix.endsWith(SUFFIX_VTT)) {
                        suffix = SUFFIX_TTML;
                    }

                    final String strSubtitleFile = plainFileName + '.' + suffix;
                    final Path ttmlPath = Paths.get(strSubtitleFile);

                    writeNetworkData(source, ttmlPath);

                    convertSubtitle(ttmlPath);
                }
            } else {
                //not successful
                logger.error("HTTP Response Code {} for URL: {}", response.code(), url);
            }
        }
    }

    public void writeSubtitle(@NotNull DatenDownload datenDownload) {
        final String urlSubtitle = datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE];
        if (urlSubtitle.isEmpty())
            return;

        final String targetDirectory = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD];

        final Request request = new Request.Builder().url(urlSubtitle).get().build();
        try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (response.isSuccessful()) {
                logger.trace("Untertitel {} schreiben nach {}", urlSubtitle, targetDirectory);

                try (var source = body.source()) {
                    String suffix = GuiFunktionen.getSuffixFromUrl(urlSubtitle);
                    if (!suffix.endsWith(SUFFIX_SRT) && !suffix.endsWith(SUFFIX_VTT)) {
                        suffix = SUFFIX_TTML;
                    }

                    createDirectory(targetDirectory);

                    final String strSubtitleFile = datenDownload.getFileNameWithoutSuffix() + '.' + suffix;
                    final Path ttmlPath = Paths.get(strSubtitleFile);

                    writeNetworkData(source, ttmlPath);

                    convertSubtitle(ttmlPath);
                }
            } else {
                //not successful
                logger.error("HTTP Response Code {} for URL: {}", response.code(), urlSubtitle);
            }
        } catch (IOException ex) {
            logger.error("Subtitle exception occured:", ex);
        }
    }

    /**
     * Convert a TTML subtitle file to SRT format.
     * @param ttmlPath The path to the origin TTML subtitle file.
     */
    private void convertSubtitle(@NotNull Path ttmlPath) {
        var subtitleFileStr = GuiFunktionen.getFileNameWithoutSuffix(ttmlPath.toAbsolutePath().toString());

        try (TimedTextMarkupLanguageParser parser = new TimedTextMarkupLanguageParser()) {
            if (!subtitleFileStr.endsWith('.' + SUFFIX_SRT) && !subtitleFileStr.endsWith("." + SUFFIX_VTT)) {
                final Path srt = Paths.get(subtitleFileStr + "." + SUFFIX_SRT);
                if (parser.parse(ttmlPath)) {
                    parser.toSrt(srt);
                } else if (parser.parseXmlFlash(ttmlPath)) {
                    parser.toSrt(srt);
                }
                logger.info("Untertitel-Datei wurde konvertiert.");
            }
        } catch (Exception ex) {
            logger.error("Fehler bei Untertitel schreiben:", ex);
        }
    }
}
