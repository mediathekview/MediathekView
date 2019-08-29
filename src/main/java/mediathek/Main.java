package mediathek;

import com.google.common.base.Stopwatch;
import com.jidesoft.utils.ThreadCheckingRepaintManager;
import com.zaxxer.sansorm.SansOrm;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import mediathek.config.Config;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenFilm;
import mediathek.daten.PooledDatabaseConnection;
import mediathek.gui.SplashScreenManager;
import mediathek.gui.dialog.DialogStarteinstellungen;
import mediathek.mac.MediathekGuiMac;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import mediathek.windows.MediathekGuiWindows;
import mediathek.x11.MediathekGuiX11;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import picocli.CommandLine;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {
    private static final String MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR = "apple.laf.useScreenMenuBar";
    private static final String LOG_TEXT_MEDIATHEK_VIEW_IS_ALREADY_RUNNING = "MediathekView wird bereits ausgeführt!";

    private static final Logger logger = LogManager.getLogger(Main.class);

    /**
     * Ensures that old film lists in .mediathek directory get deleted because they were moved to
     * ~/Library/Caches/MediathekView
     * In portable mode we MUST NOT delete the files.
     */
    private static void cleanupOsxFiles() {
        if (!Config.isPortableMode()) {
            try {
                var oldFilmList = Paths.get(Daten.getSettingsDirectory_String(), Konstanten.JSON_DATEI_FILME);
                Files.deleteIfExists(oldFilmList);
            } catch (IOException ignored) {
            }
        }
    }

    private static void printArguments(final String... aArguments) {
        for (String argument : aArguments) {
            logger.info("Startparameter: {}", argument);
        }
    }

    private static void setupPortableMode() {
        Stopwatch stopwatch = Stopwatch.createStarted();
        var portableMode = Config.isPortableMode();
        logger.info("Portable Mode: {}", portableMode);

        if (portableMode) {
            logger.trace("Configuring baseFilePath {} for portable mode", Config.baseFilePath);
            Daten.getInstance(Config.baseFilePath);
        } else {
            logger.trace("Configuring for non-portable mode");
            Daten.getInstance();
        }
        stopwatch.stop();
        logger.trace("setupPortableMode: {}", stopwatch);
    }

    /**
     * Query the class name for Nimbus L&F.
     *
     * @return the class name for Nimbus, otherwise return the system default l&f class name.
     */
    private static String queryNimbusLaFName() {
        String systemLaF = UIManager.getSystemLookAndFeelClassName();

        try {
            for (UIManager.LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    systemLaF = info.getClassName();
                    break;
                }
            }
        } catch (Exception e) {
            systemLaF = UIManager.getSystemLookAndFeelClassName();
        }

        return systemLaF;
    }

    /**
     * Set the look and feel for various OS.
     * On macOS, don´t change anything as the JVM will use the native UI L&F for swing.
     * On windows, use the system windows l&f for swing.
     * On Linux, use Nimbus l&f which is more modern than Metal.
     * <p>
     * One can override the L&F stuff for non-macOS by supplying -Dswing.defaultlaf=class_name and the class name on the CLI.
     */
    private static void setSystemLookAndFeel() {
        //don´t set L&F on macOS...
        if (SystemUtils.IS_OS_MAC_OSX)
            return;

        final String laf = System.getProperty("swing.defaultlaf");
        if (laf == null || laf.isEmpty()) {
            //only set L&F if there was no define on CLI
            logger.trace("L&F property is empty, setting L&F");
            //use system for windows and macOS
            String systemLaF = UIManager.getSystemLookAndFeelClassName();
            //on linux, use more modern Nimbus L&F...
            if (SystemUtils.IS_OS_LINUX) {
                systemLaF = queryNimbusLaFName();
            }

            //set the L&F...
            try {
                UIManager.setLookAndFeel(systemLaF);
            } catch (IllegalAccessException | InstantiationException | UnsupportedLookAndFeelException | ClassNotFoundException e) {
                logger.error("L&F error: ", e);
            }
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

        CommandLine cmd = new CommandLine(Config.class);
        try {
            var parseResult = cmd.parseArgs(args);
            if (parseResult.isUsageHelpRequested()) {
                cmd.usage(System.out);
                System.exit(cmd.getCommandSpec().exitCodeOnUsageHelp());
            }

            Log.printVersionInformation();
            printArguments(args);

            Config.setPortableMode(parseResult.hasMatchedPositional(0));
            setupPortableMode();
        } catch (CommandLine.ParameterException ex) {
            cmd.getErr().println(ex.getMessage());
            if (!CommandLine.UnmatchedArgumentException.printSuggestions(ex, cmd.getErr())) {
                ex.getCommandLine().usage(cmd.getErr());
            }
            System.exit(cmd.getCommandSpec().exitCodeOnInvalidInput());
        } catch (Exception ex) {
            logger.error("Command line parse error: {}", cmd.getErr());
            System.exit(cmd.getCommandSpec().exitCodeOnExecutionException());
        }

        printDirectoryPaths();

        checkMemoryRequirements();

        setSystemLookAndFeel();

        initializeJavaFX();

        loadConfigurationData();

        Daten.getInstance().launchHistoryDataLoading();

        deleteDatabase();

        if (MemoryUtils.isLowMemoryEnvironment()) {
            setupDatabase();
            DatenFilm.Database.initializeDatabase();
        }

        installSingleInstanceHandler();

        startGuiMode();
    }

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
            var dialog = new DialogStarteinstellungen(null, daten);
            dialog.setVisible(true);
            MVConfig.loadSystemParameter();
        }
    }

    private static void printDirectoryPaths() {
        logger.trace("Programmpfad: " + MVFunctionSys.getPathJar());
        logger.info("Verzeichnis Einstellungen: " + Daten.getSettingsDirectory_String());
    }

    private static void deleteDatabase() {
        if (!MemoryUtils.isLowMemoryEnvironment()) {
            //we can delete the database as it is not needed.
            try {
                final String dbLocation = PooledDatabaseConnection.getDatabaseLocation() + "mediathekview.mv.db";
                Files.deleteIfExists(Paths.get(dbLocation));
            } catch (IOException e) {
                logger.error("deleteDatabase()", e);
            }
        }
    }

    private static void setupDatabase() {
        logger.trace("setupDatabase()");
        SansOrm.initializeTxSimple(PooledDatabaseConnection.getInstance().getDataSource());
    }

    private static void installSingleInstanceHandler() {
        //prevent startup of multiple instances...
        var singleInstanceWatcher = new SingleInstance();
        if (singleInstanceWatcher.isAppAlreadyActive()) {
            JOptionPane.showMessageDialog(null, LOG_TEXT_MEDIATHEK_VIEW_IS_ALREADY_RUNNING);
            System.exit(1);
        }
    }

    private static void checkForOfficialOSXAppUse() {
        final var osxOfficialApp = System.getProperty("OSX_OFFICIAL_APP");
        if (osxOfficialApp == null || osxOfficialApp.isEmpty() || osxOfficialApp.equalsIgnoreCase("false")) {
            logger.warn("WARN: macOS app NOT launched from official launcher!");
        }
    }

    private static void checkMemoryRequirements() {
        final var maxMem = Runtime.getRuntime().maxMemory();
        // more than 450MB avail...
        if (maxMem < 450 * FileUtils.ONE_MB) {
            if (GraphicsEnvironment.isHeadless()) {
                System.err.println("Die VM hat nicht genügend Arbeitsspeicher zugewiesen bekommen.");
                System.err.println("Nutzen Sie den Startparameter -Xmx512M für Minimumspeicher");
            } else {
                JOptionPane.showMessageDialog(null,
                        "MediathekView hat nicht genügend Arbeitsspeicher zugewiesen bekommen.",
                        "Speicherwarnung",
                        JOptionPane.ERROR_MESSAGE);
            }

            System.exit(3);
        }
    }

    private static void startGuiMode() {
        SwingUtilities.invokeLater(() ->
        {
            final SplashScreenManager splashScreenManager = Daten.getSplashScreenManager();
            splashScreenManager.updateSplashScreenText(UIProgressState.INIT_FX);

            splashScreenManager.updateSplashScreenText(UIProgressState.FILE_CLEANUP);
            if (SystemUtils.IS_OS_MAC_OSX) {
                checkForOfficialOSXAppUse();
                System.setProperty(MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR, Boolean.TRUE.toString());
                cleanupOsxFiles();
            }

            if (Config.isDebugModeEnabled()) {
                // use for debugging EDT violations
                RepaintManager.setCurrentManager(new ThreadCheckingRepaintManager());
                logger.info("Swing Thread checking repaint manager installed.");
            }

            splashScreenManager.updateSplashScreenText(UIProgressState.START_UI);
            getPlatformWindow().setVisible(true);
        });
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
