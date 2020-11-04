package mediathek.config;

import com.google.common.util.concurrent.*;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import mediathek.Main;
import mediathek.SplashScreen;
import mediathek.controller.IoXmlLesen;
import mediathek.controller.IoXmlSchreiben;
import mediathek.controller.history.AboHistoryController;
import mediathek.controller.starter.StarterClass;
import mediathek.daten.*;
import mediathek.daten.blacklist.ListeBlacklist;
import mediathek.filmlisten.FilmeLaden;
import mediathek.gui.messages.BaseEvent;
import mediathek.gui.messages.TimerEvent;
import mediathek.javafx.bookmark.BookmarkDataList;
import mediathek.javafx.tool.JFXHiddenApplication;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVSenderIconCache;
import mediathek.tool.ReplaceList;
import mediathek.tool.notification.INotificationCenter;
import net.engio.mbassy.bus.MBassador;
import net.engio.mbassy.bus.config.BusConfiguration;
import net.engio.mbassy.bus.config.Feature;
import net.engio.mbassy.bus.config.IBusConfiguration;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.lang.ref.Cleaner;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class Daten {
    public static final MVColor mVColor = new MVColor(); // verwendete Farben
    /**
     * Prevent the unnecessary writing of a filmlist on startup when reading is enough
     */
    public static final AtomicBoolean dontWriteFilmlistOnStartup = new AtomicBoolean(true);
    private static final Logger logger = LogManager.getLogger(Daten.class);
    /**
     * Maximum number of backup files to be stored.
     */
    private final static int MAX_COPY = 5;
    private static final ScheduledThreadPoolExecutor timerPool = new ScheduledThreadPoolExecutor(Runtime.getRuntime().availableProcessors() / 2, new TimerPoolThreadFactory());
    public static ListePset listePset;
    private static Daten instance;
    // flags
    private static boolean reset; // Programm auf Starteinstellungen zurücksetzen
    // Verzeichnis zum Speichern der Programmeinstellungen
    private static String basisverzeichnis;
    /**
     * The "garbage collector" mainly for cleaning up {@link DatenFilm} objects.
     */
    private final Cleaner cleaner = Cleaner.create();
    private final FilmeLaden filmeLaden; // erledigt das updaten der Filmliste
    /**
     * "source" list of all entries, contains everything
     */
    private final ListeFilme listeFilme;
    /**
     * "the" final list of films after all filtering is done
     */
    private final ListeFilme listeFilmeNachBlackList;
    private final ListeDownloads listeDownloads; // Filme die als "Download: Tab Download" geladen werden sollen
    private final ListeDownloads listeDownloadsButton; // Filme die über "Tab Filme" als Button/Film abspielen gestartet werden
    private final ListeBlacklist listeBlacklist;
    private final BookmarkDataList listeBookmarkList;
    private final ListeMediaDB listeMediaDB;
    private final ListeMediaPath listeMediaPath;
    private final ListeAbo listeAbo;
    private final DownloadInfos downloadInfos;
    private final MVSenderIconCache senderIconCache;
    public StarterClass starterClass; // Klasse zum Ausführen der Programme (für die Downloads): VLC, flvstreamer, ...
    private INotificationCenter notificationCenter;
    /**
     * erfolgreich geladene Abos.
     */
    private AboHistoryController erledigteAbos;
    private boolean alreadyMadeBackup;
    private MBassador<BaseEvent> messageBus;
    private ListenableFuture<AboHistoryController> aboHistoryFuture;

    private Daten() {
        setupMessageBus();

        listeFilme = new ListeFilme();
        filmeLaden = new FilmeLaden(this);

        senderIconCache = new MVSenderIconCache(this);

        listeFilmeNachBlackList = new ListeFilme();
        listeBlacklist = new ListeBlacklist();
        listeBookmarkList = BookmarkDataList.getInstance(this);

        listePset = new ListePset();

        listeAbo = new ListeAbo(this);

        listeDownloads = new ListeDownloads(this);
        listeDownloadsButton = new ListeDownloads(this);

        listeMediaDB = new ListeMediaDB(this);
        listeMediaPath = new ListeMediaPath();

        downloadInfos = new DownloadInfos(messageBus);
        starterClass = new StarterClass(this);

        setupTimerPool();
    }

    /**
     * Indicator if configuration data should be reset.
     *
     * @return true if reset is necessary.
     */
    public static boolean resetConfigurationData() {
        return reset;
    }

    public static void setResetConfigurationData(final boolean aIsReset) {
        reset = aIsReset;
    }

    public static Daten getInstance(@NotNull String aBasisverzeichnis) {
        basisverzeichnis = aBasisverzeichnis;
        return getInstance();
    }

    public static Daten getInstance() {
        return instance == null ? instance = new Daten() : instance;
    }

    /**
     * Liefert den Pfad zur Filmliste
     *
     * @return Den Pfad als String
     */
    public static String getDateiFilmliste() {
        String strFile;
        final String filePart = File.separator + Konstanten.JSON_DATEI_FILME;

        if (Config.isPortableMode())
            strFile = getSettingsDirectory_String() + filePart;
        else {
            if (SystemUtils.IS_OS_MAC_OSX) {
                //place filmlist into OS X user cache directory in order not to backup it all the time in TimeMachine...
                strFile = GuiFunktionen.getHomePath() + File.separator + Konstanten.OSX_CACHE_DIRECTORY_NAME + filePart;
            } else {
                strFile = getSettingsDirectory_String() + filePart;
            }
        }

        return strFile;
    }

    /**
     * Return the location of the settings directory.
     * If it does not exist, create one.
     *
     * @return Path to the settings directory
     * @throws IllegalStateException Will be thrown if settings directory don't exist and if there is an error on creating it.
     */
    private static Path getSettingsDirectory() throws IllegalStateException {
        final Path baseDirectoryPath;
        if (basisverzeichnis == null || basisverzeichnis.isEmpty()) {
            baseDirectoryPath = Paths.get(System.getProperty("user.home"), Konstanten.VERZEICHNIS_EINSTELLUNGEN);
        } else {
            baseDirectoryPath = Paths.get(basisverzeichnis);
        }


        if (Files.notExists(baseDirectoryPath)) {
            try {
                Files.createDirectories(baseDirectoryPath);
            } catch (IOException ioException) {
                Messages.logMessage(Messages.ERROR_CANT_CREATE_FOLDER, ioException, baseDirectoryPath.toString());
                throw new IllegalStateException(Messages.ERROR_CANT_CREATE_FOLDER.getTextFormatted(baseDirectoryPath.toString()), ioException);
            }
        }

        return baseDirectoryPath;
    }

    public static String getSettingsDirectory_String() {
        return getSettingsDirectory().toString();
    }

    /**
     * Return the path to "mediathek.xml"
     *
     * @return Path object to mediathek.xml file
     */
    public static Path getMediathekXmlFilePath() {
        return Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE);
    }

    /**
     * Return the path to "mediathek.xml_copy_" files which do exist
     *
     * @return all the existing paths to backup file
     */
    private static List<Path> getMediathekXmlCopyFilePath() {
        List<Path> xmlFilePath = new ArrayList<>();

        for (int i = 1; i <= MAX_COPY; ++i) {
            Path path = Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + i);
            if (Files.exists(path)) {
                xmlFilePath.add(path);
            }
        }

        return xmlFilePath;
    }

    /**
     * Return the path to "bookmarks.json"
     *
     * @return Path object of bookmark file
     */
    public static Path getBookmarkFilePath() {
        return Daten.getSettingsDirectory().resolve(Konstanten.BOOKMARK_FILE);
    }
    
    /**
     * Load the stored bookmarkdata form JSON file
     * into memory
     */
    public void loadBookMarkData() {
      listeBookmarkList.loadFromFile(getBookmarkFilePath());
    }
    
    /**
     * Return the number of milliseconds from today´s midnight.
     *
     * @return Number of milliseconds from today´s midnight.
     */
    private long getHeute_0Uhr() {
        LocalDateTime todayMidnight = LocalDateTime.of(LocalDate.now(), LocalTime.MIDNIGHT);
        var zdt = ZonedDateTime.of(todayMidnight, ZoneId.systemDefault());

        return zdt.toInstant().toEpochMilli();
    }

    public INotificationCenter notificationCenter() {
        return notificationCenter;
    }

    public void setNotificationCenter(INotificationCenter center) {
        notificationCenter = center;
    }

    public MVSenderIconCache getSenderIconCache() {
        return senderIconCache;
    }

    public Cleaner getCleaner() {
        return cleaner;
    }

    public MBassador<BaseEvent> getMessageBus() {
        return messageBus;
    }

    /**
     * Set up message bus to log errors to our default logger
     */
    private void setupMessageBus() {
        messageBus = new MBassador<>(new BusConfiguration()
                .addFeature(Feature.SyncPubSub.Default())
                .addFeature(Feature.AsynchronousHandlerInvocation.Default())
                .addFeature(Feature.AsynchronousMessageDispatch.Default())
                .addPublicationErrorHandler(error -> logger.error(error.getMessage(), error.getCause()))
                .setProperty(IBusConfiguration.Properties.BusId, "global bus"));
    }

    public void setAboHistoryList(AboHistoryController controller) {
        erledigteAbos = controller;
    }

    public AboHistoryController getAboHistoryController() {
        return erledigteAbos;
    }

    public ScheduledThreadPoolExecutor getTimerPool() {
        return timerPool;
    }

    private void setupTimerPool() {
        //get rid of cancelled tasks immediately...
        timerPool.setRemoveOnCancelPolicy(true);

        timerPool.scheduleWithFixedDelay(() -> messageBus.publishAsync(new TimerEvent()), 4,1, TimeUnit.SECONDS);
    }

    public boolean allesLaden() {
        if (!load()) {
            logger.info("Weder Konfig noch Backup konnte geladen werden!");
            // teils geladene Reste entfernen
            clearKonfig();
            return false;
        }
        logger.info("Konfig wurde gelesen!");
        mVColor.load(); // Farben einrichten

        return true;
    }

    public void launchHistoryDataLoading() {
        logger.trace("launching async history data loading");
        var decoratedPool = MoreExecutors.listeningDecorator(ForkJoinPool.commonPool());
        aboHistoryFuture = launchAboHistoryController(decoratedPool);

    }

    private ListenableFuture<AboHistoryController> launchAboHistoryController(ListeningExecutorService decoratedPool) {
        var aboHistoryFuture = decoratedPool.submit(AboHistoryController::new);
        Futures.addCallback(aboHistoryFuture, new FutureCallback<>() {
            @Override
            public void onSuccess(@Nullable AboHistoryController aboHistoryController) {
                setAboHistoryList(aboHistoryController);
            }

            @Override
            public void onFailure(@NotNull Throwable throwable) {
                logger.error("launchAboHistoryController", throwable);
            }
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
        Path xmlFilePath = Daten.getMediathekXmlFilePath();

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

    private boolean loadBackup() {
        boolean ret = false;

        var path = Daten.getMediathekXmlCopyFilePath();
        if (path.isEmpty()) {
            logger.info("Es gibt kein Backup");
            return false;
        }

        Main.splashScreen.ifPresent(SplashScreen::close);
        // dann gibts ein Backup
        logger.info("Es gibt ein Backup");

        var loadBackup = JavaFxUtils.invokeInFxThreadAndWait(() -> {
                    ButtonType btnYes = new ButtonType("Ja", ButtonBar.ButtonData.OK_DONE);
                    ButtonType btnNo = new ButtonType("Nein", ButtonBar.ButtonData.CANCEL_CLOSE);
                    Alert alert = new Alert(Alert.AlertType.WARNING,
                            "Die Einstellungen sind beschädigt und können nicht geladen werden. "
                                    + "Soll versucht werden diese aus einem Backup wiederherzustellen?",
                            btnYes,
                            btnNo);

                    alert.setTitle(Konstanten.PROGRAMMNAME);
                    alert.setHeaderText("Gesicherte Einstellungen laden");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.orElse(btnNo) == btnNo) {
                        logger.info("User will kein Backup laden.");
                        return false;
                    } else
                        return true;
                }
        );

        if (loadBackup) {
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
        createConfigurationBackupCopies();

        final IoXmlSchreiben configWriter = new IoXmlSchreiben();
        configWriter.writeConfigurationFile(getMediathekXmlFilePath());

        if (resetConfigurationData()) {
            // das Programm soll beim nächsten Start mit den Standardeinstellungen gestartet werden
            // dazu wird den Ordner mit den Einstellungen umbenannt
            String dir1 = getSettingsDirectory_String();
            if (dir1.endsWith(File.separator)) {
                dir1 = dir1.substring(0, dir1.length() - 1);
            }

            try {
                final Path path1 = Paths.get(dir1);
                final var nowStr = DateTimeFormatter.ofPattern("yyyy.MM.dd__HH.mm.ss").format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()));
                final String dir2 = dir1 + "--" + nowStr;

                Files.move(path1, Paths.get(dir2), StandardCopyOption.REPLACE_EXISTING);
                Files.deleteIfExists(path1);
            } catch (IOException e) {
                logger.error("Die Einstellungen konnten nicht zurückgesetzt werden.", e);
                if (MediathekGui.ui() != null) {
                    JavaFxUtils.invokeInFxThreadAndWait(() -> {
                        Alert alert = new Alert(Alert.AlertType.ERROR);
                        alert.setHeaderText("Fehler beim Zurücksetzen der Einstellungen");
                        alert.setContentText("Die Einstellungen konnten nicht zurückgesetzt werden.\n"
                                + "Sie müssen jetzt das Programm beenden und dann den Ordner:\n"
                                + getSettingsDirectory_String() + '\n'
                                + "von Hand löschen und dann das Programm wieder starten.\n\n"
                                + "Im Forum erhalten Sie weitere Hilfe.");
                        JFXHiddenApplication.showAlert(alert, MediathekGui.ui());
                    });
                }
            }
        }
    }

    /**
     * Create backup copies of settings file.
     */
    private void createConfigurationBackupCopies() {
        if (!alreadyMadeBackup) {
            // nur einmal pro Programmstart machen
            logger.info("-------------------------------------------------------");
            logger.info("Einstellungen sichern");

            try {
                final Path xmlFilePath = Daten.getMediathekXmlFilePath();
                long creatTime = -1;

                Path xmlFilePathCopy_1 = Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + 1);
                if (Files.exists(xmlFilePathCopy_1)) {
                    BasicFileAttributes attrs = Files.readAttributes(xmlFilePathCopy_1, BasicFileAttributes.class);
                    FileTime d = attrs.lastModifiedTime();
                    creatTime = d.toMillis();
                }

                if (creatTime == -1 || creatTime < getHeute_0Uhr()) {
                    // nur dann ist die letzte Kopie älter als einen Tag
                    for (int i = MAX_COPY; i > 1; --i) {
                        xmlFilePathCopy_1 = Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + (i - 1));
                        final Path xmlFilePathCopy_2 = Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + i);
                        if (Files.exists(xmlFilePathCopy_1)) {
                            Files.move(xmlFilePathCopy_1, xmlFilePathCopy_2, StandardCopyOption.REPLACE_EXISTING);
                        }
                    }
                    if (Files.exists(xmlFilePath)) {
                        Files.move(xmlFilePath, Daten.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + 1), StandardCopyOption.REPLACE_EXISTING);
                    }
                    logger.info("Einstellungen wurden gesichert");
                } else {
                    logger.info("Einstellungen wurden heute schon gesichert");
                }
            } catch (IOException e) {
                logger.error("Die Einstellungen konnten nicht komplett gesichert werden!", e);
            }

            alreadyMadeBackup = true;
            logger.info("-------------------------------------------------------");
        }
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

    public ListeMediaDB getListeMediaDB() {
        return listeMediaDB;
    }

    public ListeMediaPath getListeMediaPath() {
        return listeMediaPath;
    }

    public ListeAbo getListeAbo() {
        return listeAbo;
    }

    public DownloadInfos getDownloadInfos() {
        return downloadInfos;
    }

    /**
     * Thread factory to give timer pool threads a recognizable name.
     * Follows the java.util.concurrent.Executors.DefaultThreadFactory implementation for
     * setting up the threads.
     */
    private static class TimerPoolThreadFactory implements ThreadFactory {
        private final ThreadGroup group;
        private final AtomicInteger threadNumber = new AtomicInteger(1);
        private final String namePrefix;

        TimerPoolThreadFactory() {
            SecurityManager s = System.getSecurityManager();
            group = (s != null) ? s.getThreadGroup() :
                    Thread.currentThread().getThreadGroup();
            namePrefix = "timerPool-thread-";
        }

        public Thread newThread(@NotNull Runnable r) {
            Thread t = new Thread(group, r,
                    namePrefix + threadNumber.getAndIncrement(),
                    0);
            if (t.isDaemon())
                t.setDaemon(false);
            if (t.getPriority() != Thread.NORM_PRIORITY)
                t.setPriority(Thread.NORM_PRIORITY);
            return t;
        }
    }

}
