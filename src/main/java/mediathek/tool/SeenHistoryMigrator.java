package mediathek.tool;

import mediathek.config.Daten;
import mediathek.controller.history.MVUsedUrl;
import okhttp3.HttpUrl;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Migrates the old history.txt into a sqlite database.
 */
public class SeenHistoryMigrator implements AutoCloseable {
    private static final Logger logger = LogManager.getLogger();
    private static final String fileName = "history.txt";
    private final Path historyFilePath;
    private final List<MVUsedUrl> historyEntries = new ArrayList<>();


    public SeenHistoryMigrator() throws InvalidPathException {
        final var settingsDir = Daten.getSettingsDirectory_String();
        historyFilePath = Paths.get(settingsDir).resolve(fileName);
        System.out.println("HISTORY MIGRATOR CTOR");
    }

    /**
     * Do we need history migration?
     * @return true if old text file exists, false otherwise
     */
    public boolean needsMigration() {
        return Files.exists(historyFilePath);
    }

    /**
     * Migrate from old text format to database.
     */
    public void migrate() {
        try {
            logger.info("Start old history migration.");
            readOldEntries();
            if (!historyEntries.isEmpty()) {
                // create database connection
                // drop old tables if existent
                // create tables

                //create entries in database
                for (var entry : historyEntries) {
                    // write each entry into database
                }
            }

            //Files.deleteIfExists(historyFilePath);
            logger.info("Finished old history migration.");
        }
        catch (Exception e) {
            logger.error("Error occured during history migration", e);
        }
    }

    private void readOldEntries() {
        logger.trace("Reading old entries");
        try (InputStream is = Files.newInputStream(historyFilePath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
            String entryLine;
            while ((entryLine = in.readLine()) != null) {
                MVUsedUrl oldHistoryEntry = MVUsedUrl.getUrlAusZeile(entryLine);
                final var url = oldHistoryEntry.getUrl();
                if (url.startsWith("rtmp:")) {
                    continue;
                }

                var okHttpUrl = HttpUrl.parse(url);
                if (okHttpUrl == null) {
                    continue;
                }

                // so far so good, add to lists
                historyEntries.add(oldHistoryEntry);
            }
        } catch (Exception ex) {
            logger.error("listeBauen()", ex);
        }

        logger.trace("historyEntries size: {}", historyEntries.size());
    }

    @Override
    public void close() {
        historyEntries.clear();
        System.out.println("CLOSING HISTORY MIGRATOR");
    }
}
