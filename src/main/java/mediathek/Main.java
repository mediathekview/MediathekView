/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek;

import com.formdev.flatlaf.FlatLaf;
import com.jidesoft.utils.ThreadCheckingRepaintManager;
import javafx.application.Platform;
import mediathek.config.*;
import mediathek.controller.SenderFilmlistLoadApprover;
import mediathek.controller.history.SeenHistoryMigrator;
import mediathek.daten.IndexedFilmList;
import mediathek.gui.dialog.DialogStarteinstellungen;
import mediathek.mac.MediathekGuiMac;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.tool.affinity.Affinity;
import mediathek.tool.dns.IPvPreferenceMode;
import mediathek.tool.migrator.SettingsMigrator;
import mediathek.windows.MediathekGuiWindows;
import mediathek.windows.WindowsVersionHelper;
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
import org.jetbrains.annotations.NotNull;
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
    public static SingleInstance SINGLE_INSTANCE_WATCHER;

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
            }
            catch (IOException ignored) {
            }
        }
    }

    /**
     * Remove the old and now unsupported mediafile to trash.
     * CAUTION: At least some UI MUST BE INITILIZED, otherwise on macOS VM will crash in native code!!!!
     */
    private static void removeMediaDb() {
        var mediaDbPath = StandardLocations.getSettingsDirectory().resolve("mediadb.txt");
        if (Files.exists(mediaDbPath)) {
            logger.info("Moving old unsupported media database to trash.");
            FileUtils.moveToTrash(mediaDbPath);
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
        }
        else {
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
        if (!settingsDir.isEmpty()) {
            Path pSettingsDir = Paths.get(settingsDir);
            if (Files.exists(pSettingsDir)) {
                //convert existing settings
                Path settingsFile = pSettingsDir.resolve(Konstanten.CONFIG_FILE);
                if (Files.exists(settingsFile)) {
                    logger.trace("migrating old config settings {}", settingsFile.toAbsolutePath().toString());
                    try {
                        SettingsMigrator migrator = new SettingsMigrator(settingsFile);
                        migrator.migrate();
                    }
                    catch (Exception e) {
                        logger.error("settings migration error", e);
                    }
                }
            }
            else
                logger.trace("nothing to migrate");
        }
    }

    private static void printPortableModeInfo() {
        if (Config.isPortableMode()) {
            logger.info("Configuring baseFilePath {} for portable mode", Config.baseFilePath);
        }
        else
            logger.info("Configuring for non-portable mode");
    }

    private static void setupCpuAffinity() {
        try {
            final int numCpus = Config.getNumCpus();
            if (numCpus != 0) {
                var affinity = Affinity.getAffinityImpl();
                affinity.setDesiredCpuAffinity(numCpus);
            }
        }
        catch (Exception e) {
            logger.error("Failed to set cpu affinity", e);
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
        }
        catch (IOException ex) {
            logger.error("OS X Application image could not be loaded", ex);
        }
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
        var addOpens = paramList.stream().filter(s -> s.equalsIgnoreCase("--add-opens=java.desktop/sun.awt.X11=ALL-UNNAMED")).findAny().stream().count();

        //Incorrect VM params
        var mxParamCount = paramList.stream().filter(s -> s.startsWith("-Xmx")).findAny().stream().count();

        if ((useShenandoahGC > 0)
                && (shenandoahHeuristics > 0)
                && (stringDedup > 0)
                && (maxRamPct > 0)
                && (mxParamCount == 0))
            correctParameters = true;

        if (SystemUtils.IS_OS_LINUX) {
            if (addOpens == 0)
                correctParameters = false;
        }

        if (!correctParameters) {
            logger.warn("Detected incorrect JVM parameters! Please modify your settings");
            if (!Config.isDebugModeEnabled()) {
                //show error dialog
                JOptionPane.showMessageDialog(null, getJvmErrorMessageString(), Konstanten.PROGRAMMNAME,
                        JOptionPane.WARNING_MESSAGE);
            }
        }
    }

    @NotNull
    private static String getJvmErrorMessageString() {
        var message = "<html>" +
                "<b>Inkorrekte/fehlende JVM Parameter erkannt</b><br/><br/>" +
                "Bitte stellen Sie sicher, dass die folgenden Parameter an die JVM übergeben werden:<br/>" +
                "<ul>" +
                "<li>-XX:+UseShenandoahGC</li>" +
                "<li>-XX:ShenandoahGCHeuristics=compact</li>" +
                "<li>-XX:+UseStringDeduplication</li>" +
                "<li>-XX:MaxRAMPercentage=<b>XX.X</b></li>";
        if (SystemUtils.IS_OS_LINUX) {
            message += "<li><b>--add-opens=java.desktop/sun.awt.X11=ALL-UNNAMED</b></li>";
        }

        message += "</ul><br/>" +
                "<b>-Xmx</b> sollte nicht mehr genutzt werden!" +
                "</html>";
        return message;
    }

    /**
     * Check if a non-floating point scale factor is set on Linux.
     * Java 18 VM does not support fractional scaling.
     */
    private static void checkUiScaleSetting() {
        var strScale = System.getProperty("sun.java2d.uiScale");
        if (strScale != null) {
            try {
                Integer.parseInt(strScale);
            }
            catch (NumberFormatException ex) {
                // not an int -> show warning
                // fractional scale is NOT supported under Linux, must use integer only.
                var scaleFactor = Float.parseFloat(strScale);
                logger.trace("old uiScale factor {}", scaleFactor);
                var newScale = Math.round(scaleFactor);
                logger.trace("new uiScale factor {}", newScale);

                JOptionPane.showMessageDialog(null,
                        "<html>" +
                                "Sie verwenden den Parameter <i>-Dsun.java2d.uiScale=" + strScale + "</i>.<br>" +
                                "<b>Java unter Linux unterstützt nur ganzzahlige Skalierung!</b><br><br>" +
                                "Sie sollten <i>-Dsun.java2d.uiScale=" + newScale + "</i> oder größer verwenden falls die Schriftgröße zu klein ist.",
                        Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE);
            }
        }
    }

    private static void configureDnsPreferenceMode(CommandLine.ParseResult parseResult) {
        var config = ApplicationConfiguration.getConfiguration();
        if (parseResult.hasMatchedOption("dpm")) {
            logger.trace("Dns preference mode set via CLI, storing config value");
            config.setProperty(ApplicationConfiguration.APPLICATION_NETWORKING_DNS_MODE, String.valueOf(Config.getDnsIpPreferenceMode()));
        }
        else {
            logger.trace("Dns preference mode NOT set, using config setting");
            var mode = IPvPreferenceMode.fromString(config.getString(ApplicationConfiguration.APPLICATION_NETWORKING_DNS_MODE, String.valueOf(Config.getDnsIpPreferenceMode())));
            Config.setDnsIpPreferenceMode(mode);
        }
        logger.trace("Setting DNS selector to mode: {}", Config.getDnsIpPreferenceMode().toString());
    }

    private static void registerFlatLafCustomization() {
        if (!SystemUtils.IS_OS_MAC_OSX) {
            var settings = StandardLocations.getSettingsDirectory().resolve("flatlaf");
            logger.info("Registering {} as custom FlatLaf config folder", settings);
            FlatLaf.registerCustomDefaultsSource(settings.toFile());
        }
    }

    private static void checkWindows10OrGreater() {
        try {
            if (!WindowsVersionHelper.IsWindows10OrGreater()) {
                JOptionPane.showMessageDialog(null,
                        "MediathekView benötigt mindestens Windows 10 zum Start.\nDas Programm wird nun beendet.",
                        Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
                System.exit(1);
            }
        }
        catch (Throwable ex) {
            logger.error("Error while checking Windows version", ex);
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(final String... args) {
        if (GraphicsEnvironment.isHeadless()) {
            System.err.println("Diese Version von MediathekView unterstützt keine Kommandozeilenausführung.");
            System.exit(1);
        }

        setupEnvironmentProperties();

        EventQueue.invokeLater(() -> {

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

                configureDnsPreferenceMode(parseResult);

                if (SystemUtils.IS_OS_LINUX) {
                    if (!Config.isDisableFlatLafDecorations()) {
                        // enable custom window decorations
                        JFrame.setDefaultLookAndFeelDecorated(true);
                        JDialog.setDefaultLookAndFeelDecorated(true);
                    }
                }

                setupDockIcon();

                registerFlatLafCustomization();
                DarkModeSetup.setup();

                if (SystemUtils.IS_OS_LINUX) {
                    checkUiScaleSetting();
                }

                if (!Config.isDisableJvmParameterChecks())
                    checkJVMSettings();

                if (SystemUtils.IS_OS_WINDOWS) {
                    checkWindows10OrGreater();
                }

                setupCpuAffinity();

                removeMediaDb();
                deleteOldFilmDatabaseFiles();
                deleteOldUserAgentsDatabase();

                checkMemoryRequirements();

                installSingleInstanceHandler();

                printVersionInformation();

                printJvmParameters();
                printArguments(args);
            }
            catch (CommandLine.ParameterException ex) {
                try (var err = cmd.getErr()) {
                    var errStr = ex.getMessage() + "\n\n" + ex.getCommandLine().getUsageMessage();
                    JOptionPane.showMessageDialog(null,
                            errStr,
                            "Fehlerhafte Kommandozeilenparameter",
                            JOptionPane.ERROR_MESSAGE);
                    err.println(ex.getMessage());
                    if (!CommandLine.UnmatchedArgumentException.printSuggestions(ex, err)) {
                        ex.getCommandLine().usage(err);
                    }
                    System.exit(cmd.getCommandSpec().exitCodeOnInvalidInput());
                }
            }
            catch (Exception ex) {
                logger.error("Command line parse error:", ex);
                System.exit(cmd.getCommandSpec().exitCodeOnExecutionException());
            }

            printDirectoryPaths();

            //prevent JavaFX from exiting after the last window closed
            Platform.setImplicitExit(false);

            if (!isDebuggerAttached()) {
                if (!Config.isSplashScreenDisabled()) {
                    splashScreen = Optional.of(new SplashScreen());
                }
                else {
                    logger.warn("Splash screen disabled...");
                }
            }
            else {
                logger.warn("Debugger detected -> Splash screen disabled...");
            }
            splashScreen.ifPresent(splash -> splash.setVisible(true));

            migrateOldConfigSettings();

            loadConfigurationData();

            activateNewSenders();

            migrateSeenHistory();
            Daten.getInstance().launchHistoryDataLoading();
            Daten.getInstance().getListeBookmarkList().loadFromFile();

            removeLuceneIndexDirectory();
            // enable modern search on demand
            var useModernSearch = ApplicationConfiguration.getConfiguration()
                    .getBoolean(ApplicationConfiguration.APPLICATION_USE_MODERN_SEARCH, false);
            if (useModernSearch)
                Daten.getInstance().setListeFilmeNachBlackList(new IndexedFilmList());

            startGuiMode();
        });
    }

    /**
     * Activate all senders when MediathekView adds additional ones.
     * For newer versions configKey must be adapted.
     */
    private static void activateNewSenders() {
        var alreadyActivated = ApplicationConfiguration.getConfiguration().getBoolean(Konstanten.NEW_SENDER_ACTIVATED_QUESTION_CONFIG_KEY, false);
        if (!alreadyActivated) {
            splashScreen.ifPresent(s -> s.setVisible(false));
            var op = new JOptionPane(
                    "<html>Diese Version unterstützt neue Sender, die in den Einstellungen aktiviert werden müssen.<br/>" +
                            "Soll MediathekView einmalig alle Sender aktivieren?</html>", JOptionPane.QUESTION_MESSAGE,
                    JOptionPane.YES_NO_OPTION);
            var dialog = op.createDialog(Konstanten.PROGRAMMNAME);
            dialog.setAlwaysOnTop(true);
            dialog.setModal(true);
            dialog.setResizable(true);
            dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
            dialog.setVisible(true);
            var res = op.getValue();
            if (res != null) {
                if ((int) res == JOptionPane.YES_OPTION) {
                    logger.info("Activating new senders...");
                    SenderFilmlistLoadApprover.approveAll();
                }
                ApplicationConfiguration.getConfiguration().setProperty(Konstanten.NEW_SENDER_ACTIVATED_QUESTION_CONFIG_KEY, true);
            }

            splashScreen.ifPresent(s -> s.setVisible(true));
        }
    }

    /**
     * Remove modern search index when not in use.
     */
    private static void removeLuceneIndexDirectory() {
        //when modern search is not in use, delete unused film index directory as a precaution
        var indexPath = StandardLocations.getFilmIndexPath();
        if (Files.exists(indexPath)) {
            try {
                FileUtils.deletePathRecursively(indexPath);
            }
            catch (IOException e) {
                logger.error("Failed to remove Lucene index directory", e);
            }
        }
    }

    /**
     * Checks if the application has an debugger attached to it.
     *
     * @return true if debugger was detected, false othewise.
     */
    private static boolean isDebuggerAttached() {
        return ManagementFactory.getRuntimeMXBean().getInputArguments().toString().indexOf("-agentlib:jdwp") > 0;
    }

    /**
     * Migrate the old text file history to new database format
     */
    private static void migrateSeenHistory() {
        try (SeenHistoryMigrator migrator = new SeenHistoryMigrator()) {
            if (migrator.needsMigration()) {
                migrator.migrate();
            }
        }
        catch (Exception e) {
            logger.error("migrateSeenHistory", e);
            splashScreen.ifPresent(SplashScreen::close);
            SwingErrorDialog.showExceptionMessage(null,
                    """
                            <html>Bei der Migration der Historie der Filme ist ein Fehler aufgetreten.<br>
                            Das Programm kann nicht fortfahren und wird beendet.<br><br>
                            Bitte überprüfen Sie die Fehlermeldung und suchen Sie Hilfe im Forum.</html>
                            """, e);
            System.exit(99);
        }
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

    private static void deleteOldUserAgentsDatabase() {
        try {
            var settingsPath = StandardLocations.getSettingsDirectory();
            var agentDb = settingsPath.resolve("user_agents.mv.db");
            Files.deleteIfExists(agentDb);
        }
        catch (IOException e) {
            logger.error("Error deleting old user agent database occured", e);
        }
    }

    private static void deleteOldFilmDatabaseFiles() {
        var settingsPath = StandardLocations.getSettingsDirectory();
        var dbFolder = settingsPath.resolve("database");
        var traceFile = settingsPath.resolve("databasemediathekview.trace.db");

        if (!Files.exists(dbFolder))
            return;

        try (var walk = Files.walk(dbFolder)) {
            walk.sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    //.peek(System.out::println)
                    .forEach(File::delete);

            Files.deleteIfExists(traceFile);
        }
        catch (Exception ex) {
            logger.error("Got an error deleting old database directory", ex);
        }
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private static void deleteSettingsDirectory() {
        try (var walk = Files.walk(StandardLocations.getSettingsDirectory())) {
            walk.sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    //.peek(System.out::println)
                    .forEach(File::delete);
        }
        catch (Exception ex) {
            logger.error("Got an error deleting settings directory", ex);
        }
    }

    private static void printDirectoryPaths() {
        logger.trace("Programmpfad: {}", GuiFunktionenProgramme.getPathToApplicationJar());
        logger.info("Verzeichnis Einstellungen: {}", StandardLocations.getSettingsDirectory());
    }

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
                on windows and linux there is a strange behaviour that the main window gets sent behind
                other open windows after the splash screen is closed.
             */
        if (!SystemUtils.IS_OS_MAC_OSX) {
            window.toFront();
            window.requestFocus();
        }
    }

    private static MediathekGui getPlatformWindow() {
        MediathekGui window = null;

        if (SystemUtils.IS_OS_MAC_OSX) {
            window = new MediathekGuiMac();
        }
        else if (SystemUtils.IS_OS_WINDOWS) {
            window = new MediathekGuiWindows();
        }
        else if (SystemUtils.IS_OS_LINUX) {
            window = new MediathekGuiX11();
        }
        else {
            JOptionPane.showMessageDialog(null,
                    """
                            Sie führen MediathekView auf einem nicht unterstützten Betriebssystem aus.
                            Es werden nur macOS, Windows und Linux unterstützt.
                            
                            Das Programm wird beendet, da die Funktionsfähigkeit nicht gewährleistet werden kann.""",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.ERROR_MESSAGE);
            System.exit(2);
        }

        return window;
    }

    static class DarkModeSetup {
        private static LookAndFeel getCurrentLookAndFeel(boolean darkMode) {
            LookAndFeel laf;
            if (darkMode) {
                laf = DarkModeFactory.getLookAndFeel();
            }
            else {
                laf = LightModeFactory.getLookAndFeel();
            }

            return laf;
        }

        private static void setupFlatLaf() {
            var darkMode = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_DARK_MODE, false);
            FlatLaf.setup(getCurrentLookAndFeel(darkMode));
        }

        public static void setup() {
            if (DarkModeDetector.hasDarkModeDetectionSupport()) {
                logger.trace("setting up dark mode system laf");
                var useSystemMode = ApplicationConfiguration
                        .getConfiguration()
                        .getBoolean(ApplicationConfiguration.APPLICATION_USE_SYSTEM_DARK_MODE, false);
                if (useSystemMode) {
                    FlatLaf.setup(getCurrentLookAndFeel(DarkModeDetector.isDarkMode()));
                }
                else {
                    setupFlatLaf();
                }
            }
            else {
                logger.trace("dark mode detection not supported, using config");
                setupFlatLaf();
            }
        }
    }
}
