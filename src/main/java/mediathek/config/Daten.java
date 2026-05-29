package mediathek.config;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.SortedList;
import mediathek.SplashScreenLifecycle;
import mediathek.controller.IoXmlLesen;
import mediathek.controller.IoXmlSchreiben;
import mediathek.controller.history.AboHistoryController;
import mediathek.controller.starter.DownloadStartCoordinator;
import mediathek.daten.*;
import mediathek.daten.blacklist.ListeBlacklist;
import mediathek.filmlisten.FilmeLaden;
import mediathek.gui.bookmark.BookmarkDataList;
import mediathek.gui.duplicates.FilmStatistics;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.ReplaceList;
import mediathek.tool.SenderListBoxModel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Daten {
    private static final Logger logger = LogManager.getLogger(Daten.class);
    private final ListePset listePset;
    private final EventList<FilmStatistics> duplicateStatisticsEventList = new BasicEventList<>();
    private final EventList<FilmStatistics> commonStatisticsEventList = new BasicEventList<>();
    private final FilmeLaden filmeLaden; // erledigt das updaten der Filmliste
    /**
     * "source" list of all entries, contains everything
     */
    private final ListeFilme listeFilme = new ListeFilme();
    private final ListeDownloads listeDownloads; // Filme die als "Download: Tab Download" geladen werden sollen
    private final ListeDownloads listeDownloadsButton; // Filme die über "Tab Filme" als Button/Film abspielen gestartet werden
    private final ListeBlacklist listeBlacklist = new ListeBlacklist();
    private final BookmarkDataList listeBookmarkList;
    private final ListeAbo listeAbo;
    private final DownloadInfos downloadInfos = new DownloadInfos();
    private final DownloadStartCoordinator downloadStartCoordinator; // Klasse zum Ausführen der Programme (für die Downloads): VLC, flvstreamer, ...
    private final ExecutorService decoratedPool = Executors.newVirtualThreadPerTaskExecutor();
    /**
     * "the" final list of films after all filtering is done.
     * Defaults to no lucene index unless changed at startup.
     */
    private ListeFilme listeFilmeNachBlackList = new ListeFilme();
    /**
     * erfolgreich geladene Abos.
     */
    private AboHistoryController erledigteAbos;
    private boolean backupAlreadyHandled;
    private CompletableFuture<AboHistoryController> aboHistoryFuture;
    private EventList<String> allSenderList;

    private Daten() {
        filmeLaden = new FilmeLaden(this);

        listeBookmarkList = new BookmarkDataList(this);

        listePset = new ListePset();

        listeAbo = new ListeAbo();

        listeDownloads = new ListeDownloads();
        listeDownloadsButton = new ListeDownloads();

        downloadStartCoordinator = new DownloadStartCoordinator(this);

        setupAllSendersList();
    }

    public static Daten getInstance() {
        return DatenHolder.INSTANCE;
    }

    /**
     * Return the path to "mediathek.xml_copy_" files which do exist
     *
     * @return all the existing paths to backup file
     */
    private static List<Path> getMediathekXmlCopyFilePath() {
        List<Path> xmlFilePath = new ArrayList<>();

        for (int i = 1; i <= Konstanten.MAX_NUM_BACKUP_FILE_COPIES; ++i) {
            Path path = StandardLocations.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + i);
            if (Files.exists(path)) {
                xmlFilePath.add(path);
            }
        }

        return xmlFilePath;
    }

    public EventList<String> getAllSendersList() {
        return allSenderList;
    }

    private void setupAllSendersList() {
        var sortedSenderList = new SortedList<>(SenderListBoxModel.getProvidedSenderList());
        sortedSenderList.setComparator(GermanStringSorter.INSTANCE);

        allSenderList = sortedSenderList;
    }

    public EventList<FilmStatistics> getDuplicateStatistics() {
        return duplicateStatisticsEventList;
    }

    public EventList<FilmStatistics> getCommonStatistics() {
        return commonStatisticsEventList;
    }

    public ListePset getListePset() {
        return listePset;
    }

    public DownloadStartCoordinator getDownloadStartCoordinator() {
        return downloadStartCoordinator;
    }

    public void setAboHistoryList(AboHistoryController controller) {
        erledigteAbos = controller;
    }

    public AboHistoryController getAboHistoryController() {
        return erledigteAbos;
    }

    public boolean allesLaden() {
        if (!load()) {
            logger.info("Weder Konfig noch Backup konnte geladen werden!");
            // teils geladene Reste entfernen
            clearKonfig();
            return false;
        }
        logger.info("Konfig wurde gelesen!");
        MVColor.load(); // Farben einrichten

        return true;
    }

    public ExecutorService getDecoratedPool() {
        return decoratedPool;
    }

    public void launchHistoryDataLoading() {
        logger.trace("launching async history data loading");
        aboHistoryFuture = launchAboHistoryController(decoratedPool);
    }

    private CompletableFuture<AboHistoryController> launchAboHistoryController(ExecutorService decoratedPool) {
        var aboHistoryFuture = CompletableFuture.supplyAsync(AboHistoryController::new, decoratedPool);
        aboHistoryFuture.whenCompleteAsync((aboHistoryController, throwable) -> {
            if (throwable != null) {
                logger.error("launchAboHistoryController", throwable);
                return;
            }
            setAboHistoryList(aboHistoryController);
        }, decoratedPool);
        return aboHistoryFuture;
    }

    public void waitForHistoryDataLoadingToComplete() throws ExecutionException, InterruptedException {
        aboHistoryFuture.get();
        aboHistoryFuture = null;
    }

    private void clearKonfig() {
        listePset.clear();
        ReplaceList.clear();
        listeAbo.clear();
        listeDownloads.clear();
        listeBlacklist.clear();
        listeBookmarkList.clear();
    }

    private boolean load() {
        boolean ret = false;
        Path xmlFilePath = StandardLocations.getMediathekXmlFile();

        if (Files.exists(xmlFilePath)) {
            final IoXmlLesen configReader = new IoXmlLesen();
            if (configReader.datenLesen(xmlFilePath)) {
                return true;
            } else {
                // dann hat das Laden nicht geklappt
                logger.info("Konfig konnte nicht gelesen werden!");
            }
        } else {
            // dann hat das Laden nicht geklappt
            logger.info("Konfig existiert nicht!");
        }

        // versuchen das Backup zu laden
        if (loadBackup()) {
            ret = true;
        }
        return ret;
    }

    private boolean askForBackupRestore() {
        if (Config.isDownloadAndQuit()) {
            logger.error("CLI download mode does not support interactive backup restore.");
            return false;
        }
        var text = """
                Die Einstellungen sind beschädigt und können nicht geladen werden.
                Soll versucht werden diese aus einem Backup wiederherzustellen?
                """;
        int answer = JOptionPane.showConfirmDialog(null, text,
                Konstanten.PROGRAMMNAME, JOptionPane.YES_NO_OPTION);
        if (answer == JOptionPane.YES_OPTION)
            return true;
        else {
            logger.info("User will kein Backup laden.");
            return false;
        }
    }

    private boolean loadBackup() {
        boolean ret = false;

        var path = Daten.getMediathekXmlCopyFilePath();
        if (path.isEmpty()) {
            logger.info("Es gibt kein Backup");
            return false;
        }

        SplashScreenLifecycle.close();
        // dann gibts ein Backup
        logger.info("Es gibt ein Backup");

        if (askForBackupRestore()) {
            for (Path p : path) {
                // teils geladene Reste entfernen
                clearKonfig();
                logger.info("Versuch Backup zu laden: {}", p.toString());
                final IoXmlLesen configReader = new IoXmlLesen();
                if (configReader.datenLesen(p)) {
                    logger.info("Backup hat geklappt: {}", p.toString());
                    ret = true;
                    break;
                }
            }
        }

        return ret;
    }

    public void allesSpeichern() {
        if (!backupAlreadyHandled) {
            backupAlreadyHandled = ConfigurationBackupService.createConfigurationBackupCopies();
        }

        final IoXmlSchreiben configWriter = new IoXmlSchreiben();
        configWriter.writeConfigurationFile(StandardLocations.getMediathekXmlFile());
    }

    public FilmeLaden getFilmeLaden() {
        return filmeLaden;
    }

    public ListeFilme getListeFilme() {
        return listeFilme;
    }

    public ListeFilme getListeFilmeNachBlackList() {
        return listeFilmeNachBlackList;
    }

    public void setListeFilmeNachBlackList(ListeFilme listeFilmeNachBlackList) {
        this.listeFilmeNachBlackList = listeFilmeNachBlackList;
    }

    public ListeDownloads getListeDownloads() {
        return listeDownloads;
    }

    public ListeDownloads getListeDownloadsButton() {
        return listeDownloadsButton;
    }

    public ListeBlacklist getListeBlacklist() {
        return listeBlacklist;
    }

    public BookmarkDataList getListeBookmarkList() {
        return listeBookmarkList;
    }

    public ListeAbo getListeAbo() {
        return listeAbo;
    }

    public DownloadInfos getDownloadInfos() {
        return downloadInfos;
    }

    /**
     * Part of the Bill Pugh Singleton implementation
     */
    private static class DatenHolder {
        private static final Daten INSTANCE = new Daten();
    }
}
