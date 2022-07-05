package mediathek;

import com.formdev.flatlaf.FlatLightLaf;
import com.google.common.base.Stopwatch;
import com.sun.jna.platform.win32.VersionHelpers;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import mediathek.config.*;
import mediathek.controller.history.SeenHistoryMigrator;
import mediathek.gui.dialog.DialogStarteinstellungen;
import mediathek.javafx.AustrianVlcCheck;
import mediathek.javafx.tool.JFXHiddenApplication;
import mediathek.mac.MediathekGuiMac;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.affinity.Affinity;
import mediathek.tool.javafx.FXErrorDialog;
import mediathek.tool.migrator.SettingsMigrator;
import mediathek.tool.swing.SwingUIFontChanger;
import mediathek.tool.swing.ThreadCheckingRepaintManager;
import mediathek.windows.MediathekGuiWindows;
import mediathek.x11.MediathekGuiX11;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.Filter;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.AsyncAppender;
import org.apache.logging.log4j.core.appender.ConsoleAppender;
import org.apache.logging.log4j.core.appender.FileAppender;
import org.apache.logging.log4j.core.config.AppenderRef;
import org.apache.logging.log4j.core.filter.ThresholdFilter;
import org.apache.logging.log4j.core.layout.PatternLayout;
import picocli.CommandLine;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.Security;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.Optional;

public class Main {
    private static final String MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR = "apple.laf.useScreenMenuBar";
    private static final Logger logger = LogManager.getLogger(Main.class);
    public static Optional<SplashScreen> splashScreen = Optional.empty();

    static {
        // set up log4j callback registry
        System.setProperty("log4j.shutdownCallbackRegistry", Log4jShutdownCallbackRegistry.class.getName());
    }

    /**
     * Ensures that old film lists in .mediathek directory get deleted because they were moved to
     * ~/Library/Caches/MediathekView
     * In portable mode we MUST NOT delete the files.
     */
    private static void cleanupOsxFiles() {
        if (!Config.isPortableMode()) {
            try {
                var oldFilmList = StandardLocations.getSettingsDirectory().resolve(Konstanten.JSON_DATEI_FILME);
                Files.deleteIfExists(oldFilmList);
            } catch (IOException ignored) {
            }
        }
    }

    /**
     * Remove the old and now unsupported mediafile to trash.
     * CAUTION: At least some UI MUST BE INITILIZED, otherwise on macOS VM will crash in native code!!!!
     */
    private static void removeMediaDb() {
        try {
            var mediaDbPath = StandardLocations.getSettingsDirectory().resolve("mediadb.txt");
            if (Files.exists(mediaDbPath)) {
                logger.info("Moving old unsupported media database to trash.");
                mediathek.tool.FileUtils.moveToTrash(mediaDbPath);
            }
        } catch (IOException ignored) {
        }
    }

    private static void printJvmParameters() {
        logger.debug("=== JavaVM Parameter ===");
        RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
        var jvmArgs = runtimeMXBean.getInputArguments();
        for (var arg : jvmArgs) {
            logger.debug(arg);
        }
        logger.debug("========================");
    }

    private static void printArguments(final String... aArguments) {
        for (String argument : aArguments) {
            logger.info("Startparameter: {}", argument);
        }
    }

    private static void setupLogging() {
        final var loggerContext = (LoggerContext) LogManager.getContext(false);
        final var config = loggerContext.getConfiguration();
        final String path;
        final String fileName = "/mediathekview.log";

        if (!Config.isPortableMode())
            path = StandardLocations.getSettingsDirectory() + fileName;
        else
            path = Config.baseFilePath + fileName;


        final PatternLayout consolePattern;
        if (Config.isEnhancedLoggingEnabled() || Config.isDebugModeEnabled()) {
            consolePattern = PatternLayout.newBuilder().withPattern("[%-5level] [%t] %c - %msg%n").build();
        } else {
            consolePattern = PatternLayout.newBuilder().withPattern(". %msg%n").build();
        }

        var consoleAppender = ConsoleAppender.createDefaultAppenderForLayout(consolePattern);
        //for normal users only show INFO and higher messages
        if (!Config.isEnhancedLoggingEnabled() && !Config.isDebugModeEnabled()) {
            final var thresholdFilter = ThresholdFilter.createFilter(Level.INFO, Filter.Result.ACCEPT, Filter.Result.DENY);
            consoleAppender.addFilter(thresholdFilter);
        }
        consoleAppender.start();

        var fileAppenderBuilder = FileAppender.newBuilder()
                .setName("LogFile")
                .withAppend(false)
                .withFileName(path)
                .setLayout(PatternLayout.newBuilder().withPattern("%-5p %d  [%t] %C{2} (%F:%L) - %m%n").build())
                //.setLayout(PatternLayout.newBuilder().withPattern("[%-5level] %d{yyyy-MM-dd HH:mm:ss.SSS} [%t] %c - %msg%n").build())
                .setConfiguration(config);

        //regular users may have DEBUG output in log file but not TRACE
        if (!Config.isEnhancedLoggingEnabled() && !Config.isDebugModeEnabled()) {
            final var thresholdFilter = ThresholdFilter.createFilter(Level.DEBUG, Filter.Result.ACCEPT, Filter.Result.DENY);
            fileAppenderBuilder.setFilter(thresholdFilter);
        }

        AsyncAppender asyncAppender = null;
        if (!Config.isFileLoggingDisabled()) {
            FileAppender fileAppender = fileAppenderBuilder.build();
            fileAppender.start();
            config.addAppender(fileAppender);

            asyncAppender = AsyncAppender.newBuilder()
                    .setName("Async")
                    .setAppenderRefs(new AppenderRef[]{AppenderRef.createAppenderRef(fileAppender.getName(), null, null)})
                    .setConfiguration(config)
                    .setIncludeLocation(true)
                    .setBlocking(false)
                    .build();

            asyncAppender.start();
            config.addAppender(asyncAppender);
        }

        final var rootLogger = loggerContext.getRootLogger();
        rootLogger.setLevel(Level.TRACE);
        rootLogger.addAppender(consoleAppender);
        if (!Config.isFileLoggingDisabled())
            rootLogger.addAppender(asyncAppender);

        loggerContext.updateLoggers();
    }

    private static void setupEnvironmentProperties() {
        System.setProperty("file.encoding", "UTF-8");

        //enable full strength crypto if not already done
        Security.setProperty("crypto.policy", "unlimited");

        if (SystemUtils.IS_OS_MAC_OSX) {
            System.setProperty("apple.awt.application.name", Konstanten.PROGRAMMNAME);
            System.setProperty("apple.awt.application.appearance", "system");
        }
    }

    private static void printVersionInformation() {
        logger.info("Programmstart: {}", DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(RuntimeStatistics.startZeit));
        logger.info("Version: {}", Konstanten.MVVERSION);

        logger.info("=== Java Information ===");

        logger.info("Vendor: {}", SystemUtils.JAVA_VENDOR);
        logger.info("VMname: {}", SystemUtils.JAVA_VM_NAME);
        logger.info("Version: {}", SystemUtils.JAVA_VERSION);
        logger.info("Runtime Version: {}", SystemUtils.JAVA_RUNTIME_VERSION);
        final var runtime = Runtime.getRuntime();
        logger.info("Maximum Memory: {} MB", runtime.maxMemory() / FileUtils.ONE_MB);

        logger.info("Operating System: {}", SystemUtils.OS_NAME);
        logger.info("OS Version: {}", SystemUtils.OS_VERSION);
        logger.info("OS Arch: {}", SystemUtils.OS_ARCH);
        if (DarkModeDetector.hasDarkModeDetectionSupport())
            logger.info("OS Dark Mode enabled: {}", DarkModeDetector.isDarkMode());
        else
            logger.info("OS Dark Mode detection not supported");
        logger.info("OS Available Processors: {}", runtime.availableProcessors());
    }

    /**
     * Migrate old settings stored in mediathek.xml to new app config
     */
    private static void migrateOldConfigSettings() {
        var settingsDir = StandardLocations.getSettingsDirectory().toString();
        if (settingsDir != null && !settingsDir.isEmpty()) {
            Path pSettingsDir = Paths.get(settingsDir);
            if (Files.exists(pSettingsDir)) {
                //convert existing settings
                Path settingsFile = pSettingsDir.resolve(Konstanten.CONFIG_FILE);
                if (Files.exists(settingsFile)) {
                    logger.trace("{} exists", Konstanten.CONFIG_FILE);
                    logger.trace("migrating old config settings");
                    try {
                        SettingsMigrator migrator = new SettingsMigrator(settingsFile);
                        migrator.migrate();
                    } catch (Exception e) {
                        logger.error("settings migration error", e);
                    }
                }
            } else
                logger.trace("nothing to migrate");
        }
    }

    private static void printPortableModeInfo() {
        if (Config.isPortableMode()) {
            logger.info("Configuring baseFilePath {} for portable mode", Config.baseFilePath);
        } else
            logger.info("Configuring for non-portable mode");
    }

    private static void setupCpuAffinity() {
        final int numCpus = Config.getNumCpus();
        if (numCpus != 0) {
            var affinity = Affinity.getAffinityImpl();
            affinity.setDesiredCpuAffinity(numCpus);
        }
    }

    /**
     * Install dock icon when supported.
     */
    private static void setupDockIcon() {
        try {
            if (Taskbar.isTaskbarSupported()) {
                var taskbar = Taskbar.getTaskbar();
                if (taskbar.isSupported(Taskbar.Feature.ICON_IMAGE)) {
                    final URL url = Main.class.getResource("/mediathek/res/MediathekView.png");
                    if (url != null) {
                        final BufferedImage appImage = ImageIO.read(url);
                        Taskbar.getTaskbar().setIconImage(appImage);
                    }
                }
            }
        } catch (IOException ex) {
            logger.error("OS X Application image could not be loaded", ex);
        }
    }

    private static final Color JTABLE_ALTERNATE_ROW_COLOR = new Color(247, 247, 247);

    private static void setupFlatLaf() {
        FlatLightLaf.setup();

        UIManager.put("TabbedPane.showTabSeparators", true);
        // install alternate row color only for windows >8 and macOS, Linux
        boolean installAlternateRowColor;
        if (SystemUtils.IS_OS_WINDOWS && VersionHelpers.IsWindows8OrGreater()) {
            installAlternateRowColor = true;
        } else installAlternateRowColor = SystemUtils.IS_OS_MAC_OSX || SystemUtils.IS_OS_LINUX;

        if (installAlternateRowColor)
            UIManager.put("Table.alternateRowColor", JTABLE_ALTERNATE_ROW_COLOR);
    }

    /**
     * Check if Shenandoah GC settings are supplied to JVM.
     * Otherwise display warning dialog.
     */
    private static void checkJVMSettings() {
        RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
        boolean correctParameters = false;
        var paramList = runtimeMXBean.getInputArguments();

        var useShenandoahGC = paramList.stream().filter(s -> s.equalsIgnoreCase("-XX:+UseShenandoahGC")).findAny().stream().count();
        var shenandoahHeuristics = paramList.stream().filter(s -> s.equalsIgnoreCase("-XX:ShenandoahGCHeuristics=compact")).findAny().stream().count();
        var stringDedup = paramList.stream().filter(s -> s.equalsIgnoreCase("-XX:+UseStringDeduplication")).findAny().stream().count();
        var maxRamPct = paramList.stream().filter(s -> s.startsWith("-XX:MaxRAMPercentage=")).findAny().stream().count();

        //Incorrect VM params
        var mxParamCount = paramList.stream().filter(s -> s.startsWith("-Xmx")).findAny().stream().count();

        if ((useShenandoahGC > 0)
                && (shenandoahHeuristics > 0)
                && (stringDedup > 0)
                && (maxRamPct > 0)
                && (mxParamCount == 0))
            correctParameters = true;

        if (!correctParameters) {
            //show error dialog
            logger.warn("Detected incorrect JVM parameters! Please modify your settings");
            JOptionPane.showMessageDialog(null,
                    "<html>" +
                            "<b>Inkorrekte JVM Parameter erkannt</b><br/><br/>" +
                            "Bitte stellen Sie sicher, dass die folgenden Parameter an die JVM übergeben werden:<br/>" +
                            "<ul>" +
                            "<li>-XX:+UseShenandoahGC</li>" +
                            "<li>-XX:ShenandoahGCHeuristics=compact</li>" +
                            "<li>-XX:+UseStringDeduplication</li>" +
                            "<li>-XX:MaxRAMPercentage=<b>xx.x</b></li>" +
                            "</ul><br/>" +
                            "<b>-Xmx</b> sollte nicht mehr genutzt werden!" +
                            "</html>",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.WARNING_MESSAGE);
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(final String... args) {
        EventQueue.invokeLater(() -> {
            setupEnvironmentProperties();

            if (GraphicsEnvironment.isHeadless()) {
                System.err.println("Diese Version von MediathekView unterstützt keine Kommandozeilenausführung.");
                System.exit(1);
            }

            CommandLine cmd = new CommandLine(Config.class);
            try {
                var parseResult = cmd.parseArgs(args);
                if (parseResult.isUsageHelpRequested()) {
                    cmd.usage(System.out);
                    System.exit(cmd.getCommandSpec().exitCodeOnUsageHelp());
                }

                Config.setPortableMode(parseResult.hasMatchedPositional(0));
                if (Config.isPortableMode()) {
                    StandardLocations.INSTANCE.setPortableBaseDirectory(Config.baseFilePath);
                }

                setupLogging();
                printPortableModeInfo();

                setupDockIcon();
                setupFlatLaf();

                if (!Config.isDisableJvmParameterChecks())
                    checkJVMSettings();

                if (SystemUtils.IS_OS_WINDOWS) {
                    if (!VersionHelpers.IsWindows10OrGreater())
                        logger.warn("This Operating System configuration is too old and will be unsupported in the next updates.");
                }

                setupCpuAffinity();

                initializeJavaFX();

                removeMediaDb();

                JFXHiddenApplication.launchApplication();
                checkMemoryRequirements();

                installSingleInstanceHandler();

                printVersionInformation();

                printJvmParameters();
                printArguments(args);
            } catch (CommandLine.ParameterException ex) {
                try (var err = cmd.getErr()) {
                    err.println(ex.getMessage());
                    if (!CommandLine.UnmatchedArgumentException.printSuggestions(ex, err)) {
                        ex.getCommandLine().usage(err);
                    }
                    System.exit(cmd.getCommandSpec().exitCodeOnInvalidInput());
                }
            } catch (Exception ex) {
                logger.error("Command line parse error:", ex);
                System.exit(cmd.getCommandSpec().exitCodeOnExecutionException());
            }

            printDirectoryPaths();

            if (!isDebuggerAttached()) {
                splashScreen = Optional.of(new SplashScreen());
            } else {
                logger.warn("Debugger detected -> Splash screen disabled...");
            }
            splashScreen.ifPresent(SplashScreen::show);

            migrateOldConfigSettings();

            loadConfigurationData();

            migrateSeenHistory();
            Daten.getInstance().launchHistoryDataLoading();

            Daten.getInstance().loadBookMarkData();

            if (SystemUtils.IS_OS_LINUX)
                changeGlobalFontSize();

            startGuiMode();
        });
    }

    /**
     * Checks if the application has an debugger attached to it.
     *
     * @return true if debugger was detected, false othewise.
     */
    private static boolean isDebuggerAttached() {
        return ManagementFactory.getRuntimeMXBean().getInputArguments().toString().indexOf("-agentlib:jdwp") > 0;
    }

    private static void changeGlobalFontSize() {
        try {
            var size = ApplicationConfiguration.getConfiguration().getFloat(ApplicationConfiguration.APPLICATION_UI_FONT_SIZE);
            logger.info("Custom font size found, changing global UI settings");
            SwingUIFontChanger fc = new SwingUIFontChanger();
            fc.changeFontSize(size);
        } catch (Exception e) {
            logger.info("No custom font size found.");
        }
    }

    /**
     * Migrate the old text file history to new database format
     */
    private static void migrateSeenHistory() {
        try (SeenHistoryMigrator migrator = new SeenHistoryMigrator()) {
            if (migrator.needsMigration()) {
                migrator.migrate();
            }
        } catch (Exception e) {
            logger.error("migrateSeenHistory", e);
            splashScreen.ifPresent(SplashScreen::close);
            FXErrorDialog.showErrorDialogWithoutParent(Konstanten.PROGRAMMNAME,
                    "Migration fehlgeschlagen",
                    """
                            Bei der Migration der Historie der Filme ist ein Fehler aufgetreten.
                            Das Programm kann nicht fortfahren und wird beendet.
                                                                
                            Bitte überprüfen Sie die Fehlermeldung und suchen Sie Hilfe im Forum.
                            """, e);
            System.exit(99);
        }
    }

    @SuppressWarnings("unused")
    private static void initializeJavaFX() {
        //JavaFX stuff
        Platform.setImplicitExit(false);
        //necessary to init JavaFX before loading config data
        var dummy = new JFXPanel();
    }

    private static void loadConfigurationData() {
        var daten = Daten.getInstance();

        if (!daten.allesLaden()) {
            // erster Start
            ReplaceList.init(); // einmal ein Muster anlegen, für Linux/OS X ist es bereits aktiv!
            Main.splashScreen.ifPresent(SplashScreen::close);

            var dialog = new DialogStarteinstellungen(null);
            if (dialog.showDialog() == DialogStarteinstellungen.ResultCode.CANCELLED) {
                //show termination dialog
                JOptionPane.showMessageDialog(null,
                        "<html>Sie haben die Einrichtung des Programms abgebrochen.<br>" +
                                "MediathekView muss deshalb beendet werden.</html>", Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);

                deleteSettingsDirectory();

                System.exit(1);
            }
            MVConfig.loadSystemParameter();
        }
    }

    private static void deleteSettingsDirectory() {
        try (var walk = Files.walk(StandardLocations.getSettingsDirectory())) {
            walk.sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    //.peek(System.out::println)
                    .forEach(File::delete);
        } catch (Exception ex) {
            logger.error("Got an error deleting settings directory", ex);
        }
    }

    private static void printDirectoryPaths() {
        logger.trace("Programmpfad: " + GuiFunktionenProgramme.getPathToApplicationJar());
        logger.info("Verzeichnis Einstellungen: " + StandardLocations.getSettingsDirectory());
    }

    public static SingleInstance SINGLE_INSTANCE_WATCHER;

    /**
     * Prevent startup of multiple instances of the app.
     */
    private static void installSingleInstanceHandler() {
        SINGLE_INSTANCE_WATCHER = new SingleInstance();
        if (SINGLE_INSTANCE_WATCHER.isAppAlreadyActive()) {
            JOptionPane.showMessageDialog(null,
                    "Es dürfen nicht mehrere MediathekView-Instanzen gleichzeitig laufen.\n" +
                            "Bitte beenden Sie zuerst das andere Programm.",
                    Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
            System.exit(1);
        }
    }

    private static void checkForOfficialOSXAppUse() {
        final var osxOfficialApp = System.getProperty(Konstanten.MACOS_OFFICIAL_APP);
        if (osxOfficialApp == null || osxOfficialApp.isEmpty() || osxOfficialApp.equalsIgnoreCase("false")) {
            logger.warn("WARN: macOS app NOT launched from official launcher!");
        }
    }

    private static void checkMemoryRequirements() {
        final var maxMem = Runtime.getRuntime().maxMemory();
        if (maxMem < Konstanten.MINIMUM_MEMORY_THRESHOLD) {
            JOptionPane.showMessageDialog(null,
                    "Es werden mindestens 768MB RAM zum Betrieb benötigt.\n" +
                            "Das Programm wird nun beendet.",
                    Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);

            System.exit(3);
        }
    }

    private static void startGuiMode() {
        splashScreen.ifPresent(s -> s.update(UIProgressState.INIT_FX));

        splashScreen.ifPresent(s -> s.update(UIProgressState.FILE_CLEANUP));
        if (SystemUtils.IS_OS_MAC_OSX) {
            checkForOfficialOSXAppUse();
            System.setProperty(MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR, Boolean.TRUE.toString());
            cleanupOsxFiles();
        }

        if (Config.isDebugModeEnabled() || Config.isInstallThreadCheckingRepaintManager()) {
            // use for debugging EDT violations
            RepaintManager.setCurrentManager(new ThreadCheckingRepaintManager());
            logger.info("Swing Thread checking repaint manager installed.");
        }

        splashScreen.ifPresent(s -> s.update(UIProgressState.START_UI));
        var window = getPlatformWindow();
        splashScreen.ifPresent(SplashScreen::close);
        window.setVisible(true);
            /*
                on windows there is a strange behaviour that the main window gets sent behind
                other open windows after the splash screen is closed.
             */
        if (SystemUtils.IS_OS_WINDOWS) {
            window.toFront();
            window.requestFocus();
        }
        //show a link to tutorial if we are in Austria and have never used MV before...
        AustrianVlcCheck vlcCheck = new AustrianVlcCheck();
        vlcCheck.perform();
    }

    private static MediathekGui getPlatformWindow() {
        MediathekGui window;
        Stopwatch watch = Stopwatch.createStarted();

        if (SystemUtils.IS_OS_MAC_OSX) {
            window = new MediathekGuiMac();
        } else if (SystemUtils.IS_OS_WINDOWS) {
            window = new MediathekGuiWindows();
        } else if (SystemUtils.IS_OS_UNIX) {
            window = new MediathekGuiX11();
        } else
            throw new IllegalStateException("Unknown operating system detected! Cannot create main window");

        watch.stop();
        logger.trace("getPlatformWindow(): {}", watch);

        return window;
    }
}
