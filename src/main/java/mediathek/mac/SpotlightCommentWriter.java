package mediathek.mac;

import mSearch.tool.Log;
import mSearch.tool.javafx.FXErrorDialog;
import mediathek.config.Daten;
import mediathek.daten.DatenDownload;
import org.apache.commons.lang3.StringUtils;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Writes spotlight comments to the downloaded file on OS X.
 */
public class SpotlightCommentWriter {
    private final Daten daten;

    public SpotlightCommentWriter() {
        daten = Daten.getInstance();
    }

    /**
     * Log that MV wasn´t used via the official mac app.
     * This is relevant to know for bug reports.
     */
    private void logUnofficialMacAppUse() {
        Log.errorLog(915263987, "MV wird NICHT über die offizielle Mac App genutzt.");
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
                    final ProcessBuilder builder = new ProcessBuilder("/usr/bin/osascript", "-e");
                    builder.command().add(script);
                    builder.start();
                } catch (Exception ex) {
                    if (daten.getMediathekGui() != null) {
                        FXErrorDialog.showErrorDialog("Fehler",
                                "Fehler beim Schreiben des Spotlight-Kommentars",
                                "Es trat ein Fehler beim Schreiben des Spotlight-Kommentars auf.\n" +
                                        "Sollte dieser häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                                ex);
                    }
                    Log.errorLog(915263987, "Fehler beim Spotlight schreiben" + filmPath.toString());
                    //AppleScript may not be available if user does not use the official MacApp.
                    //We need to log that as well if there are error reports.
                    try {
                        if (!System.getProperty("OSX_OFFICIAL_APP").equalsIgnoreCase("true")) {
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
