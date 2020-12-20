package mediathek.controller.history;

import mediathek.config.Daten;
import mediathek.gui.messages.history.AboHistoryChangedEvent;
import mediathek.tool.FileUtils;
import okhttp3.HttpUrl;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;
import java.nio.file.*;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class AboHistoryController {
    private static final String FILENAME = "downloadAbos.txt";
    private static final Logger logger = LogManager.getLogger(AboHistoryController.class);
    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    /**
     * Quick lookup list for history checks.
     * Stores only URLs
     */
    private final Set<String> listeUrls = Collections.synchronizedSet(new HashSet<>());
    /**
     * The actual storage for all history data.
     * Will be written to file.
     */
    private final List<MVUsedUrl> listeUrlsSortDate = Collections.synchronizedList(new LinkedList<>());
    private Path urlPath;

    public AboHistoryController() {
        final var settingsDir = Daten.getSettingsDirectory_String();
        try {
            urlPath = Paths.get(settingsDir).resolve(FILENAME);
        } catch (InvalidPathException e) {
            logger.error("Path resolve failed for {},{}", settingsDir, FILENAME);
            urlPath = null;
        }

        listeBauen();
    }

    public List<MVUsedUrl> getListeUrlsSortDate() {
        return listeUrlsSortDate;
    }

    private void sendChangeMessage() {
        Daten.getInstance().getMessageBus().publishAsync(new AboHistoryChangedEvent());
    }

    /**
     * Remove all stored entries.
     * Also deletes the used text file. When supported it will be moved to trash, otherwise deleted.
     */
    public synchronized void removeAll() {
        listeUrls.clear();
        listeUrlsSortDate.clear();

        try {
            FileUtils.moveToTrash(urlPath);
        } catch (IOException ignored) {
        }

        sendChangeMessage();
    }

    public boolean urlPruefen(String urlFilm) {
        //wenn url gefunden, dann true zurück
        return listeUrls.contains(urlFilm);
    }

    public synchronized void urlAusLogfileLoeschen(String urlFilm) {
        //Logfile einlesen, entsprechende Zeile Filtern und dann Logfile überschreiben
        //wenn die URL im Logfile ist, dann true zurück
        boolean gefunden = false;

        checkUrlFilePath();

        final List<String> liste = new ArrayList<>();
        try (InputStream is = Files.newInputStream(urlPath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
            String zeile;
            while ((zeile = in.readLine()) != null) {
                if (MVUsedUrl.getUrlAusZeile(zeile).getUrl().equals(urlFilm)) {
                    gefunden = true; //nur dann muss das Logfile auch geschrieben werden
                } else {
                    liste.add(zeile);
                }
            }
        } catch (Exception ex) {
            logger.error("urlAusLogfileLoeschen(String)", ex);
        }

        //und jetzt wieder schreiben, wenn nötig
        if (gefunden) {
            try (OutputStream os = Files.newOutputStream(urlPath);
                 OutputStreamWriter osw = new OutputStreamWriter(os);
                 BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
                for (String entry : liste)
                    bufferedWriter.write(entry + '\n');
            } catch (Exception ex) {
                logger.error("urlAusLogfileLoeschen(String)", ex);
            }
        }

        listeUrls.clear();
        listeUrlsSortDate.clear();

        listeBauen();

        sendChangeMessage();
    }

    public synchronized void zeileSchreiben(String thema, String titel, String url) {
        var datum = DATE_TIME_FORMATTER.format(LocalDate.now());
        listeUrls.add(url);
        listeUrlsSortDate.add(new MVUsedUrl(datum, thema, titel, url));

        checkUrlFilePath();

        try (OutputStream os = Files.newOutputStream(urlPath, StandardOpenOption.APPEND);
             OutputStreamWriter osw = new OutputStreamWriter(os);
             BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
            final MVUsedUrl usedUrl = new MVUsedUrl(datum, thema, titel, url);
            bufferedWriter.write(usedUrl.getUsedUrl());
        } catch (Exception ex) {
            logger.error("zeileSchreiben(...)", ex);
        }

        sendChangeMessage();
    }

    // eigener Thread!!
    public synchronized void createLineWriterThread(LinkedList<MVUsedUrl> mvuuList) {
        Thread t = new LineWriterThread(mvuuList);
        t.start();
    }

    private void checkUrlFilePath() {
        try {
            if (Files.notExists(urlPath))
                Files.createFile(urlPath);
        } catch (IOException ex) {
            logger.error("checkUrlFilePath()", ex);
        }
    }

    private void listeBauen() {
        //LinkedList mit den URLs aus dem Logfile bauen
        checkUrlFilePath();

        List<String> badEntriesList = new ArrayList<>();

        try (InputStream is = Files.newInputStream(urlPath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
            String zeile;
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
                listeUrls.add(url);
                listeUrlsSortDate.add(mvuu);
            }
        } catch (Exception ex) {
            logger.error("listeBauen()", ex);
        }

        if (!badEntriesList.isEmpty()) {
            logger.warn("File {} contains {} invalid entries ", urlPath, badEntriesList.size());
            removeIllegalEntries(badEntriesList);
            badEntriesList.clear();
        }
        logger.debug("listeUrls size: {} for file {}", listeUrls.size(), urlPath);
        logger.debug("listeUrlsSortDate size: {} for file {}", listeUrlsSortDate.size(), urlPath);
    }

    private void removeIllegalEntries(List<String> badEntriesList) {
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

    class LineWriterThread extends Thread {

        private final List<MVUsedUrl> mvuuList;

        public LineWriterThread(List<MVUsedUrl> mvuuList) {
            this.mvuuList = mvuuList;
            setName(LineWriterThread.class.getName());
        }

        @Override
        public void run() {
            zeilenSchreiben();
        }

        private void zeilenSchreiben() {
            checkUrlFilePath();

            try (OutputStream os = Files.newOutputStream(urlPath, StandardOpenOption.APPEND);
                 OutputStreamWriter osw = new OutputStreamWriter(os);
                 BufferedWriter bufferedWriter = new BufferedWriter(osw)) {
                for (MVUsedUrl mvuu : mvuuList) {
                    listeUrls.add(mvuu.getUrl());
                    listeUrlsSortDate.add(mvuu);

                    bufferedWriter.write(mvuu.getUsedUrl());
                }
            } catch (Exception ex) {
                logger.error("zeilenSchreiben()", ex);
            }
            sendChangeMessage();
        }
    }

}
