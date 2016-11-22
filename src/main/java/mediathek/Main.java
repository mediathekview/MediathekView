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
import mSearch.Config;
import mSearch.tool.Log;
import mSearch.tool.SingleInstance;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.Messages;
import mediathek.mac.MediathekGuiMac;

import javax.swing.*;
import java.awt.*;
import java.io.Console;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static mediathek.tool.MVFunctionSys.startMeldungen;

public class Main
{
    private static final String JAVAFX_CLASSNAME_APPLICATION_PLATFORM = "javafx.application.Platform";
    private static final String ARGUMENT_STARTUPMODE_AUTO = "-auto";
    private static final String ARGUMENT_STARTUPMODE_FASTAUTO = "-fastauto";
    private static final String ARGUMENT_STARTUPMODE_DEBUG = "-d";
    private static final String ARGUMENT_STARTUPMODE_MAXIMIZED = "-m";
    private static final String ARGUMENT_STARTUPMODE_SHOWMESSAGES = "-v";
    private static final String LOG_TEXT_SYSTEMMELDUNG = "Systemmeldung";
    private static final String LOG_TEXT_FEHLERMELDUNG = "Fehlermeldung";
    private static final String HTTP_PROXY_USER = "http.proxyUser";
    private static final String HTTP_PROXY_PW = "http.proxyPassword";
    private static final String LOG_TEXT_PROXY_AUTHENTICATION_SUCESSFULL = "Proxy Authentication: (%s)";
    private static final String LOG_TEXT_PROXY_AUTHENTICATION_NOT_CONFIGURED = "Proxy Authentication: not configured";
    private static final String LOG_TEXT_PROXY_AUTHENTICATION_CANNOT_ACCESS_PROXY_USER_PROXY_PW = "Proxy Authentication: cannot access proxyUser / proxyPassword";
    private static final String MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR = "apple.laf.useScreenMenuBar";
    private static final String TRUE = "true";
    private static final String LOG_TEXT_MEDIATHEK_VIEW_IS_ALREADY_RUNNING = "MediathekView is already running!";
    private static final String REFLECTION_FIELD_NAME_AWT_APP_CLASS_NAME = "awtAppClassName";
    private static final String MEDIATHEK_VIEW = "MediathekView";
    public static final String TEXT_LINE = "===========================================";

    private enum StartupMode
    {

        GUI, AUTO, FASTAUTO
    }

    /**
     * Ensures that old film lists in .mediathek directory get deleted because they were moved to
     * ~/Library/Caches/MediathekView
     */
    private static void cleanupOsxFiles()
    {
        try
        {
            Path oldFilmList = Paths.get(Daten.getSettingsDirectory_String(), Konstanten.JSON_DATEI_FILME);
            Files.deleteIfExists(oldFilmList);
        } catch (Exception ignored)
        {
        }
    }

    /**
     * Tests if javafx is in the classpath by loading a well known class.
     */
    private static boolean hasJavaFx()
    {
        try
        {
            Class.forName(JAVAFX_CLASSNAME_APPLICATION_PLATFORM);
            return true;

        } catch (ClassNotFoundException e)
        {
            System.out.println(TEXT_LINE);
            System.out.printf(Messages.ERROR_NO_JAVAFX_INSTALLED.getText());
            System.out.println(TEXT_LINE);

            return false;
        }
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

    /**
     * @param args the command line arguments
     */
    public static void main(final String args[])
    {
        new Main().start(args);
    }

    private void start(String... args)
    {
        if (hasJavaFx())
        {
            StartupMode startupMode = StartupMode.GUI;

            proxyAuthentication();

            if (args != null)
            {
               startupMode = processArgs(startupMode, args);
            }

            startUI(startupMode, args);
        }
    }

    private void startUI(StartupMode aStartupMode, final String... aArguments)
    {
        aStartupMode = switchToCLIModeIfNecessary(aStartupMode);
        switch (aStartupMode)
        {
            case AUTO:
                startAutoMode(aArguments);
                break;

            case FASTAUTO:
                startFastAutoMode(aArguments);
                break;

            case GUI:
                startGuiMode(aArguments);
                break;
            default:
                startUI(StartupMode.GUI);
        }
    }

    private void startGuiMode(final String[] args)
    {
        EventQueue.invokeLater(() ->
        {
            //JavaFX stuff
            Platform.setImplicitExit(false);

            if (SystemInfo.isMacOSX())
            {
                System.setProperty(MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR, TRUE);
                cleanupOsxFiles();
            }

            if (Daten.isDebug())
            {
                // use for debugging EDT violations
                RepaintManager.setCurrentManager(new ThreadCheckingRepaintManager());

                if (SystemInfo.isMacOSX())
                {
                    //prevent startup of multiple instances...useful during debugging :(
                    SingleInstance singleInstanceWatcher = new SingleInstance();
                    if (singleInstanceWatcher.isAppAlreadyActive())
                    {
                        JOptionPane.showMessageDialog(null, LOG_TEXT_MEDIATHEK_VIEW_IS_ALREADY_RUNNING);
                    }
                }
            }
            if (SystemInfo.isMacOSX())
            {
                new MediathekGuiMac(args).setVisible(true);
            } else
            {
                if (SystemInfo.isLinux())
                {
                    checkLinuxUi();
                }
                new MediathekGui(args).setVisible(true);
            }
        });
    }

    private void checkLinuxUi()
    {
        try
        {
            Toolkit xToolkit = Toolkit.getDefaultToolkit();
            java.lang.reflect.Field awtAppClassNameField = xToolkit.getClass().getDeclaredField(REFLECTION_FIELD_NAME_AWT_APP_CLASS_NAME);
            awtAppClassNameField.setAccessible(true);
            awtAppClassNameField.set(xToolkit, MEDIATHEK_VIEW);
        } catch (Exception ignored)
        {
            System.err.println("Couldn't set awtAppClassName");
        }
    }

    private void startFastAutoMode(final String[] args)
    {
        final MediathekAuto mvAuto = new MediathekAuto(args);
        mvAuto.setFastAuto(true);
        mvAuto.starten();
    }

    private void startAutoMode(final String[] args)
    {
        new MediathekAuto(args).starten();
    }

    private StartupMode switchToCLIModeIfNecessary(final StartupMode aState)
    {
    /*
     If user tries to start MV from command-line without proper options,
     instead of crashing while trying to open Swing windows, just change to CLI mode and warn the user.
     */
        if (GraphicsEnvironment.isHeadless() && (aState == StartupMode.GUI))
        {
            System.err.println("MediathekView wurde nicht als Kommandozeilenprogramm gestartet.");
            System.err.println("Startmodus wurde auf -auto geändert.");
            System.err.println();
          return StartupMode.AUTO;
        }
        return aState;
    }

    private StartupMode processArgs(final StartupMode aStartupMode, final String... aArguments)
    {
        StartupMode newStartupMode = null;
        for (String argument : aArguments)
        {
            argument = argument.toLowerCase();
            switch (argument)
            {
                case ARGUMENT_STARTUPMODE_AUTO:
                    newStartupMode =  StartupMode.AUTO;
                    break;

                case ARGUMENT_STARTUPMODE_FASTAUTO:
                    newStartupMode = StartupMode.FASTAUTO;
                    break;

                case ARGUMENT_STARTUPMODE_SHOWMESSAGES:
                    EventQueue.invokeLater(() ->
                    {
                        startMeldungen();
                        SysMsg.sysMsg(LOG_TEXT_SYSTEMMELDUNG);
                        Log.errorLog(100000000, LOG_TEXT_FEHLERMELDUNG);
                        Log.endMsg();
                        System.exit(0);
                    });
                    break;

                case ARGUMENT_STARTUPMODE_DEBUG:
                    Daten.setDebug(true);
                    Config.debug = true;
                    break;

                case ARGUMENT_STARTUPMODE_MAXIMIZED:
                    Daten.setStartMaximized(true);
                    break;
            }
        }

        return newStartupMode == null ? aStartupMode : newStartupMode;
    }

    private void proxyAuthentication()
    {
        try
        {
            String tmp = System.getProperty(HTTP_PROXY_USER, System.getProperty(HTTP_PROXY_USER));
            if (tmp != null)
            {
                Authenticator.setDefault(new Authenticator()
                {
                    @Override
                    protected PasswordAuthentication getPasswordAuthentication()
                    {
                        String proxyUser = System.getProperty(HTTP_PROXY_USER);
                        String proxyPassword = System.getProperty(HTTP_PROXY_PW);
                        return new PasswordAuthentication(proxyUser, proxyPassword.toCharArray());
                    }
                });
                SysMsg.sysMsg(String.format(LOG_TEXT_PROXY_AUTHENTICATION_SUCESSFULL, System.getProperty(HTTP_PROXY_USER)));
            } else
            {
                SysMsg.sysMsg(LOG_TEXT_PROXY_AUTHENTICATION_NOT_CONFIGURED);
            }

        } catch (SecurityException se)
        {
            SysMsg.sysMsg(LOG_TEXT_PROXY_AUTHENTICATION_CANNOT_ACCESS_PROXY_USER_PROXY_PW + se.toString());
        }
    }
}
