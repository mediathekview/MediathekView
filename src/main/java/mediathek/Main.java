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

import com.jidesoft.utils.SystemInfo;
import com.jidesoft.utils.ThreadCheckingRepaintManager;
import javafx.application.Platform;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.Log;
import mSearch.tool.SingleInstance;
import mediathek.config.Config;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.mac.MediathekGuiMac;
import mediathek.windows.MediathekGuiWindows;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.nio.file.Files;
import java.nio.file.Paths;

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
        if (SystemInfo.isWindows() || SystemInfo.isLinux())
            disableNotifications();

        setupPortableMode(args);
        checkMemoryRequirements();
        checkForJavaFX();

        IconFontSwing.register(FontAwesome.getIconFont());

        installSingleInstanceHandler();

        new Main().start(args);
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
            if (SystemInfo.isMacOSX())
                checkForOfficialOSXAppUse();

            //JavaFX stuff
            Platform.setImplicitExit(false);

            if (SystemInfo.isMacOSX()) {
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

        if (SystemInfo.isMacOSX()) {
            window = new MediathekGuiMac();
        } else if (SystemInfo.isWindows()) {
            window = new MediathekGuiWindows();
        } else {
            if (SystemInfo.isUnix()) {
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
                logger.info(String.format(LOG_TEXT_PROXY_AUTHENTICATION_SUCESSFUL, prxUser));
            } else if (prxUser != null && prxPassword == null) {
                logger.info(LOG_TEXT_PROXY_PASSWORD_NOT_SET);
            } else {
                logger.info(LOG_TEXT_PROXY_AUTHENTICATION_NOT_CONFIGURED);
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
