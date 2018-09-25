/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek;

import com.google.common.hash.HashCode;
import com.google.common.hash.Hashing;
import com.jidesoft.utils.ThreadCheckingRepaintManager;
import com.zaxxer.sansorm.SansOrm;
import javafx.application.Platform;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mSearch.daten.PooledDatabaseConnection;
import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.Log;
import mSearch.tool.SingleInstance;
import mediathek.config.Config;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.mac.MediathekGuiMac;
import mediathek.windows.MediathekGuiWindows;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.UUID;

import static mediathek.tool.MVFunctionSys.startMeldungen;

public class Main {
    private static final String JAVAFX_CLASSNAME_APPLICATION_PLATFORM = "javafx.application.Platform";
    private static final String HTTP_PROXY_USER = "http.proxyUser";
    private static final String HTTP_PROXY_PW = "http.proxyPassword";
    private static final String LOG_TEXT_PROXY_AUTHENTICATION_SUCESSFUL = "Proxy Authentication: (%s)";
    private static final String LOG_TEXT_PROXY_AUTHENTICATION_NOT_CONFIGURED = "Proxy Authentication: not configured";
    private static final String LOG_TEXT_PROXY_PASSWORD_NOT_SET = "Proxy Authentication: Password is not set";
    private static final String LOG_TEXT_PROXY_AUTHENTICATION_CANNOT_ACCESS_PROXY_USER_PROXY_PW = "Proxy Authentication: cannot access proxyUser / proxyPassword";
    private static final String MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR = "apple.laf.useScreenMenuBar";
    private static final String LOG_TEXT_MEDIATHEK_VIEW_IS_ALREADY_RUNNING = "MediathekView wird bereits ausgeführt!";
    private static final String X11_AWT_APP_CLASS_NAME = "awtAppClassName";

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
            } catch (Exception ignored) {
            }
        }
    }

    /**
     * Tests if javafx is in the classpath by loading a well known class.
     */
    private static void checkForJavaFX() {
        final var message = "MediathekView benötigt ein installiertes JavaFX.";

        try {
            Class.forName(JAVAFX_CLASSNAME_APPLICATION_PLATFORM);
        } catch (ClassNotFoundException e) {
            logger.error("JavaFX was not found on system.", e);
            if (GraphicsEnvironment.isHeadless()) {
                System.err.println(message);
            } else {
                //we have a screen
                JOptionPane.showMessageDialog(null,
                        message,
                        "JavaFX nicht gefunden", JOptionPane.ERROR_MESSAGE);
            }
            System.exit(3);
        }
    }

    private static void printArguments(final String... aArguments) {
        for (String argument : aArguments) {
            logger.info("Startparameter: {}", argument);
        }
    }

    private static String readPfadFromArguments(final String... aArguments) {
        var pfad = "";
        if (aArguments != null && aArguments.length > 0) {
            for (String arg : aArguments) {
                if (!arg.startsWith("-")) {
                    if (!arg.endsWith(File.separator)) {
                        arg += File.separator;
                    }
                    pfad = arg;
                }
            }
        }

        return pfad;
    }

    private static void setupPortableMode(String... args) {
        printArguments(args);
        final var basePath = readPfadFromArguments(args);
        if (!basePath.isEmpty()) {
            Config.setPortableMode(true);
            logger.info("Portable Mode: true");
        } else
            logger.info("Portable Mode: false");

        //initialize Daten object now for database
        Daten.getInstance(basePath);
    }

    /**
     * Due to a controlsfx bug no notifications on windows
     */
    private static void disableNotifications() {
        ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS,false);
    }
    /**
     * @param args the command line arguments
     */
    public static void main(final String... args) {
        if (SystemUtils.IS_OS_WINDOWS || SystemUtils.IS_OS_LINUX)
            disableNotifications();

        generateAntiThrottlingId();

        setupPortableMode(args);

        setupDatabase();

        checkMemoryRequirements();
        checkForJavaFX();

        IconFontSwing.register(FontAwesome.getIconFont());

        installSingleInstanceHandler();

        new Main().start(args);
    }

    private static void setupDatabase() {
        logger.trace("setupDatabase()");
        SansOrm.initializeTxSimple(PooledDatabaseConnection.getInstance().getDataSource());
    }

    /**
     * This ID will be used by the server-side load balancer to prevent throttling of legitimate
     * MediathekView users. We have black sheeps who downloads lists REALLY often :(
     * It is not intented to track user behaviour!
     */
    private static void generateAntiThrottlingId() {
            //Test if we already have an id
            String id = ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_ANTI_THROTTLING_ID,null);
            if (id == null) {
                //generate one that can´t be reconstructed
                final HashCode hc = Hashing.murmur3_128().newHasher()
                        .putString(UUID.randomUUID().toString(), StandardCharsets.UTF_8)
                        .hash();

                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_ANTI_THROTTLING_ID,hc.toString());
            }
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
            JOptionPane.showMessageDialog(null,
                    "Bitte nutzen Sie die offizielle macOS Applikation für das beste Nutzererlebnis.",
                    "Anwendungshinweis",
                    JOptionPane.WARNING_MESSAGE);
        }
    }

    private static void checkMemoryRequirements() {
        final var maxMem = Runtime.getRuntime().maxMemory();
        // more than 450MB avail...
        if (maxMem < 450 * FileUtils.ONE_MB) {
            if (GraphicsEnvironment.isHeadless()) {
                System.err.println("Die VM hat nicht genügend Arbeitsspeicher zugewiesen.");
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

    private void start(String... args) {
        proxyAuthentication();

        if (args != null) {
            processArgs(args);
        }

        startGuiMode();
    }

    /*
     * Aufruf:
     * java -jar Mediathek [Pfad zur Konfigdatei, sonst homeverzeichnis] [Schalter]
     *
     * Programmschalter:
     *
     * -M Fenster maximiert starten
     * -A Automodus
     * -noGui ohne GUI starten und die Filmliste laden
     *
     * */

    private void startGuiMode() {
        EventQueue.invokeLater(() ->
        {
            //JavaFX stuff
            Platform.setImplicitExit(false);

            if (SystemUtils.IS_OS_MAC_OSX) {
                checkForOfficialOSXAppUse();
                System.setProperty(MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR, Boolean.TRUE.toString());
                cleanupOsxFiles();
            }

            if (Config.isDebuggingEnabled()) {
                // use for debugging EDT violations
                RepaintManager.setCurrentManager(new ThreadCheckingRepaintManager());
            }

            getPlatformWindow().setVisible(true);
        });
    }

    private MediathekGui getPlatformWindow() {
        MediathekGui window;

        if (SystemUtils.IS_OS_MAC_OSX) {
            window = new MediathekGuiMac();
        } else if (SystemUtils.IS_OS_WINDOWS) {
            window = new MediathekGuiWindows();
        } else {
            if (SystemUtils.IS_OS_UNIX) {
                setupX11WindowManagerClassName();
            }
            window = new MediathekGui();
        }

        return window;
    }

    /**
     * Setup the X11 window manager WM_CLASS hint.
     * Enables e.g. GNOME to determine application name and to enable app specific functionality.
     */
    private void setupX11WindowManagerClassName() {
        try {
            var xToolkit = Toolkit.getDefaultToolkit();
            java.lang.reflect.Field awtAppClassNameField = xToolkit.getClass().getDeclaredField(X11_AWT_APP_CLASS_NAME);
            awtAppClassNameField.setAccessible(true);
            awtAppClassNameField.set(xToolkit, Konstanten.PROGRAMMNAME);
        } catch (Exception ignored) {
            logger.warn("Could not set awtAppClassName");
        }
    }

    private void processArgs(final String... aArguments) {
        for (String argument : aArguments) {
            argument = argument.toLowerCase();
            switch (argument) {
                case ProgramArguments.STARTUPMODE_VERBOSE:
                    EventQueue.invokeLater(() ->
                    {
                        startMeldungen();
                        logger.info("Systemmeldung");
                        Log.errorLog(100000000, "Fehlermeldung");
                        Log.endMsg();
                        System.exit(0);
                    });
                    break;

                case ProgramArguments.STARTUPMODE_DEBUG:
                    Config.enableDebugMode();
                    break;

                case ProgramArguments.STARTUPMODE_MAXIMIZED:
                    Daten.setStartMaximized(true);
                    break;
            }
        }
    }

    private void proxyAuthentication() {
        //TODO remove if not used anymore by URLConnection
        try {
            final var prxUser = System.getProperty(HTTP_PROXY_USER, null);
            final var prxPassword = System.getProperty(HTTP_PROXY_PW, null);
            if (prxUser != null && prxPassword != null) {
                final var authenticator = new PasswordAuthentication(prxUser, prxPassword.toCharArray());
                Authenticator.setDefault(new Authenticator() {
                    @Override
                    protected PasswordAuthentication getPasswordAuthentication() {
                        return authenticator;
                    }
                });
                logger.debug(String.format(LOG_TEXT_PROXY_AUTHENTICATION_SUCESSFUL, prxUser));
            } else if (prxUser != null && prxPassword == null) {
                logger.debug(LOG_TEXT_PROXY_PASSWORD_NOT_SET);
            } else {
                logger.debug(LOG_TEXT_PROXY_AUTHENTICATION_NOT_CONFIGURED);
            }

        } catch (SecurityException se) {
            logger.warn(LOG_TEXT_PROXY_AUTHENTICATION_CANNOT_ACCESS_PROXY_USER_PROXY_PW + se.toString());
        }
    }

    private final class ProgramArguments {
        private static final String STARTUPMODE_DEBUG = "-d";
        private static final String STARTUPMODE_MAXIMIZED = "-m";
        private static final String STARTUPMODE_VERBOSE = "-v";
    }
}
