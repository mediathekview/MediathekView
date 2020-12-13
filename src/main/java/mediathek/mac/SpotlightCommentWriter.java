package mediathek.mac;

import javafx.application.Platform;
import mediathek.config.Konstanten;
import mediathek.daten.DatenDownload;
import mediathek.tool.javafx.FXErrorDialog;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

/**
 * Writes spotlight comments to the downloaded file on OS X.
 */
public class SpotlightCommentWriter {
    private static final Logger logger = LogManager.getLogger(SpotlightCommentWriter.class);

    /**
     * Log that MV wasn´t used via the official mac app.
     * This is relevant to know for bug reports.
     */
    private void logUnofficialMacAppUse() {
        logger.error("MediathekView macOS: OFFICIAL DMG APP IS NOT USED!");
    }

    /**
     * This will write the content of the film description into the OS X Finder Info Comment Field.
     * This enables Spotlight to search for these tags.
     *
     * @param datenDownload The download information object
     */
    public void writeComment(final DatenDownload datenDownload) {
        if (datenDownload.film == null) {
            // kann bei EinmalDownloads nach einem Neuladen der Filmliste/Programmneustart der Fall sein
            return;
        }
        final Path filmPath = Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        if (Files.exists(filmPath)) {
            final String strFilePath = filmPath.toString();
            String strComment = datenDownload.film.getDescription();
            if (strComment != null) {
                //no need to write spotlight data when there is no description...
                if (strComment.isEmpty()) {
                    return;
                }

                //replace quotation marks...
                strComment = StringUtils.replace(strComment, "\"", "\\\"");

                final String script = "tell application \"Finder\"\n"
                        + "set my_file to POSIX file \"" + strFilePath + "\" as alias\n"
                        + "set comment of my_file to \"" + strComment + "\"\n"
                        + "end tell\n";
                try {
                    logger.trace("Writing spotlight comment");
                    final ProcessBuilder builder = new ProcessBuilder("/usr/bin/osascript", "-e");
                    builder.command().add(script);
                    builder.start().waitFor(5, TimeUnit.SECONDS);
                    logger.trace("Spotlight writing finished");
                } catch (Exception ex) {
                    Platform.runLater(() -> FXErrorDialog.showErrorDialog("Fehler",
                            "Fehler beim Schreiben des Spotlight-Kommentars",
                            "Es trat ein Fehler beim Schreiben des Spotlight-Kommentars auf.\n" +
                                    "Sollte dieser häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                            ex));
                    logger.error("Fehler beim Spotlight schreiben: {}", filmPath.toString(), ex);
                    //AppleScript may not be available if user does not use the official MacApp.
                    //We need to log that as well if there are error reports.
                    try {
                        if (!System.getProperty(Konstanten.MACOS_OFFICIAL_APP).equalsIgnoreCase("true")) {
                            logUnofficialMacAppUse();
                        }
                    } catch (NullPointerException ignored) {
                        logUnofficialMacAppUse();
                    }
                }
            }
        }
    }
}
