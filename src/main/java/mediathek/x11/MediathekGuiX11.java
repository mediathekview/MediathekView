/*
 * Copyright (c) 2026 derreisende77.
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

package mediathek.x11;

import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.ProcessCommandUtils;
import mediathek.tool.notification.GenericNotificationCenter;
import mediathek.tool.notification.INotificationCenter;
import mediathek.tool.notification.LinuxNotificationCenter;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.*;
import java.io.IOException;

public class MediathekGuiX11 extends MediathekGui {
    private static final Logger logger = LogManager.getLogger(MediathekGuiX11.class);

    public MediathekGuiX11() {
        setupX11WindowManagerClassName();
    }

    @Override
    protected void createDarkModeToggleButton() {
        // we are using a menu item here
    }

    @Override
    protected void createMenuBar() {
        super.createMenuBar();
        createDarkModeMenuAction();
    }

    @Override
    protected void setupSystemTray() {
        final var useTray = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_USE_TRAY, false);
        if (!DesktopEnvDetector.trayIconSupported() && useTray) {
            logger.warn("Application tray icon is not supported on this platform, deactivating.");
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_USE_TRAY, false);
        }
        super.setupSystemTray();
    }

    /**
     * Setup the X11 window manager WM_CLASS hint.
     * Enables e.g. GNOME to determine application name and to enable app specific functionality.
     */
    private void setupX11WindowManagerClassName() {
        try {
            var xToolkit = Toolkit.getDefaultToolkit();
            java.lang.reflect.Field awtAppClassNameField = xToolkit.getClass().getDeclaredField("awtAppClassName");
            awtAppClassNameField.setAccessible(true);
            awtAppClassNameField.set(xToolkit, Konstanten.PROGRAMMNAME);
        } catch (Exception e) {
            logger.error("Could not set awtAppClassName", e);
        }
    }

    @Override
    protected INotificationCenter getNotificationCenter() {
        var notificationCenter = new LinuxNotificationCenter();
        if (notificationCenter.hasNativeSupport())
            return notificationCenter;
        else
            return new GenericNotificationCenter();
    }

    @Override
    protected void shutdownComputer() {
        String strShutdownCommand;

        if (SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_FREE_BSD) {
            strShutdownCommand = MVConfig.get(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN); //strShutdownCommand = "shutdown -h now";
            if (strShutdownCommand.isEmpty()) {
                strShutdownCommand = Konstanten.SHUTDOWN_LINUX;
                MVConfig.add(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN, Konstanten.SHUTDOWN_LINUX);
            }
        } else {
            // unknown operating system
            logger.error("shutdown command is unknown for this operating system");
            return;
        }

        try {
            logger.info("Shutdown: {}", strShutdownCommand);
            new ProcessBuilder(ProcessCommandUtils.tokenizeCommand(strShutdownCommand)).start();
        } catch (IOException ex) {
            logger.error(ex);
        }
    }
}
