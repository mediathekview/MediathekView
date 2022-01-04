package mediathek.controller.history;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import mediathek.config.StandardLocations;
import mediathek.gui.messages.history.AboHistoryChangedEvent;
import mediathek.tool.MessageBus;
import okhttp3.HttpUrl;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.*;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class AboHistoryController {
    private static final String FILENAME = "downloadAbos.txt";
    private static final Logger logger = LogManager.getLogger();
    /**
     * Quick lookup set for history checks.
     * Stores only URLs.
     */
    private final Set<String> urlCacheSet = ConcurrentHashMap.newKeySet();
    /**
     * The actual storage for all history data.
     * Will be written to file.
     * MUST BE LOCKED DURING MANIPULATION!
     */
    private final EventList<MVUsedUrl> baseDataEventList = new BasicEventList<>();
    private Path urlPath;

    public AboHistoryController() {
        final var settingsDir = StandardLocations.getSettingsDirectory();
        try {
            urlPath = settingsDir.resolve(FILENAME);
            if (Files.notExists(urlPath))
                Files.createFile(urlPath);
        } catch (IOException e) {
            logger.error("I/O error occured", e);
            urlPath = null;
        }

        listeBauen();
    }

    public EventList<MVUsedUrl> getDataList() {
        return baseDataEventList;
    }

    private void sendChangeMessage() {
        MessageBus.getMessageBus().publishAsync(new AboHistoryChangedEvent());
    }

    private void clearLists() {
        try {
            baseDataEventList.getReadWriteLock().writeLock().lock();
            baseDataEventList.clear();
            urlCacheSet.clear();
        } finally {
            baseDataEventList.getReadWriteLock().writeLock().unlock();
        }
    }

    /**
     * Remove all stored entries.
     * Also deletes the used text file. When supported it will be moved to trash, otherwise deleted.
     */
    public void removeAll() {
        clearLists();

        try (var fc = FileChannel.open(urlPath, StandardOpenOption.WRITE)){
            fc.truncate(0);
        } catch (IOException e) {
            logger.error("Could not empty abo history file", e);
        }

        sendChangeMessage();
    }

    /**
     * Check if a URL exists in the cache set.
     *
     * @param urlFilm a string url
     * @return true if exists in cache, false otherwise.
     */
    public boolean urlExists(@NotNull String urlFilm) {
        return urlCacheSet.contains(urlFilm);
    }

    public synchronized void removeUrl(@NotNull String urlFilm) {
        //Logfile einlesen, entsprechende Zeile Filtern und dann Logfile überschreiben
        //wenn die URL im Logfile ist, dann true zurück
        boolean entryFound = false;

        // if the url is NOT in our list, it won´t be in the file...bail out
        if (!urlExists(urlFilm))
            return;

        final List<String> liste = new ArrayList<>();
        try (InputStream is = Files.newInputStream(urlPath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
            String zeile;
            while ((zeile = in.readLine()) != null) {
                if (MVUsedUrl.getUrlAusZeile(zeile).getUrl().equals(urlFilm)) {
                    entryFound = true; //nur dann muss das Logfile auch geschrieben werden
                } else {
                    liste.add(zeile);
                }
            }
        } catch (Exception ex) {
            logger.error("urlAusLogfileLoeschen(String)", ex);
        }

        //und jetzt wieder schreiben, wenn nötig
        if (entryFound) {
            try (OutputStream os = Files.newOutputStream(urlPath);
                 OutputStreamWriter osw = new OutputStreamWriter(os);
                 BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
                for (String entry : liste)
                    bufferedWriter.write(entry + '\n');
            } catch (Exception ex) {
                logger.error("urlAusLogfileLoeschen(String)", ex);
            }

            clearLists();

            listeBauen();

            sendChangeMessage();
        }
    }

    public void add(@NotNull MVUsedUrl usedUrl) {
        try {
            baseDataEventList.getReadWriteLock().writeLock().lock();
            baseDataEventList.add(usedUrl);
            urlCacheSet.add(usedUrl.getUrl());
        } finally {
            baseDataEventList.getReadWriteLock().writeLock().unlock();
        }

        try (OutputStream os = Files.newOutputStream(urlPath, StandardOpenOption.APPEND);
             OutputStreamWriter osw = new OutputStreamWriter(os);
             BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
            bufferedWriter.write(usedUrl.getPreparedRowString());
        } catch (Exception ex) {
            logger.error("Single add failed", ex);
        }

        sendChangeMessage();
    }

    /**
     * Create the internally used list from text file.
     */
    private void listeBauen() {
        List<String> badEntriesList = new ArrayList<>();

        try (InputStream is = Files.newInputStream(urlPath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
            String zeile;

            baseDataEventList.getReadWriteLock().writeLock().lock();

            while ((zeile = in.readLine()) != null) {
                MVUsedUrl mvuu = MVUsedUrl.getUrlAusZeile(zeile);
                var url = mvuu.getUrl();
                if (url.startsWith("rtmp:")) {
                    //logger.warn("RTMP URL found in file {}, skipping: {}", urlPath, url);
                    badEntriesList.add(zeile);
                    continue;
                }

                var okHttpUrl = HttpUrl.parse(url);
                if (okHttpUrl == null) {
                    //logger.warn("Invalid URL received in {}, skipping: {}", urlPath,url);
                    badEntriesList.add(zeile);
                    continue;
                }

                // so far so good, add to lists
                baseDataEventList.add(mvuu);
                urlCacheSet.add(url);
            }
        } catch (Exception ex) {
            logger.error("listeBauen()", ex);
        } finally {
            baseDataEventList.getReadWriteLock().writeLock().unlock();
        }

        checkBadEntries(badEntriesList);

        printDebugInfo();
    }

    private void printDebugInfo() {
        try {
            baseDataEventList.getReadWriteLock().readLock().lock();
            logger.trace("urlCacheSet size: {} for file {}", urlCacheSet.size(), urlPath);
            logger.trace("dataList size: {} for file {}", baseDataEventList.size(), urlPath);
        } finally {
            baseDataEventList.getReadWriteLock().readLock().unlock();
        }
    }

    private void checkBadEntries(@NotNull List<String> badEntriesList) {
        if (!badEntriesList.isEmpty()) {
            logger.warn("File {} contained {} invalid entries ", urlPath, badEntriesList.size());
            removeIllegalEntries(badEntriesList);
            badEntriesList.clear();
        }
    }

    /**
     * Append multiple urls to history file.
     *
     * @param mvuuList the items to add.
     */
    public void add(@NotNull List<MVUsedUrl> mvuuList) {
        try (OutputStream os = Files.newOutputStream(urlPath, StandardOpenOption.APPEND);
             OutputStreamWriter osw = new OutputStreamWriter(os);
             BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
            baseDataEventList.getReadWriteLock().writeLock().lock();

            for (MVUsedUrl mvuu : mvuuList) {
                baseDataEventList.add(mvuu);
                urlCacheSet.add(mvuu.getUrl());
                bufferedWriter.write(mvuu.getPreparedRowString());
            }
        } catch (Exception ex) {
            logger.error("zeilenSchreiben()", ex);
        } finally {
            baseDataEventList.getReadWriteLock().writeLock().unlock();
        }
        sendChangeMessage();
    }

    private synchronized void removeIllegalEntries(@NotNull List<String> badEntriesList) {
        logger.trace("Cleaning entries for {}", urlPath);

        final List<String> cleanedEntriesList = new ArrayList<>();
        try (InputStream is = Files.newInputStream(urlPath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
            String zeile;
            while ((zeile = in.readLine()) != null) {
                if (!badEntriesList.contains(zeile)) {
                    cleanedEntriesList.add(zeile);
                }
            }
        } catch (Exception ex) {
            logger.error("removeIllegalEntries()", ex);
        }

        try (OutputStream os = Files.newOutputStream(urlPath);
             OutputStreamWriter osw = new OutputStreamWriter(os);
             BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
            for (var entry : cleanedEntriesList)
                bufferedWriter.write(entry + '\n');
        } catch (Exception ex) {
            logger.error("removeIllegalEntries()", ex);
        }

        cleanedEntriesList.clear();
        logger.trace("Finished cleaning entries for {}", urlPath);
    }
}
