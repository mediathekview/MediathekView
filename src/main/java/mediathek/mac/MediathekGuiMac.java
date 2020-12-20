package mediathek.mac;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.gui.messages.DownloadFinishedEvent;
import mediathek.gui.messages.DownloadStartEvent;
import mediathek.gui.messages.InstallTabSwitchListenerEvent;
import mediathek.mac.tabs.TabDownloadsMac;
import mediathek.mac.tabs.TabFilmeMac;
import mediathek.mac.touchbar.TouchBarUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.notification.GenericNotificationCenter;
import mediathek.tool.notification.MacNotificationCenter;
import mediathek.tool.notification.NullNotificationCenter;
import mediathek.tool.threads.IndicatorThread;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;

public class MediathekGuiMac extends MediathekGui {
    protected static Logger logger = LogManager.getLogger(MediathekGuiMac.class);
    private final OsxPowerManager powerManager = new OsxPowerManager();

    public MediathekGuiMac() {
        super();

        setupDockIcon();
    }

    @Override
    public void setVisible(boolean visible) {
        super.setVisible(visible);

        if (TouchBarUtils.isTouchBarSupported()) {
            var comp = tabbedPane.getSelectedComponent();
            if (comp.equals(tabFilme)) {
                // bugfix for macOS 11.1 Big Sur which otherwise wouldn´t show the touchbar on startup...
                // window must be visible to activate touchbar
                tabFilme.showTouchBar();
            }
        }
    }

    @Override
    protected boolean officialLauncherInUse() {
        boolean macOSBinaryInuse = true;
        final var osxOfficialApp = System.getProperty(Konstanten.MACOS_OFFICIAL_APP);
        if (osxOfficialApp == null || osxOfficialApp.isEmpty() || osxOfficialApp.equalsIgnoreCase("false")) {
            macOSBinaryInuse = false;
        }
        return macOSBinaryInuse;
    }

    @Override
    protected void installAdditionalHelpEntries() {
        //unused on macOS
    }

    @Override
    public void initializeSystemTray() {
        //we don´t use it on macOS
    }

    @Override
    protected void installTouchBarSupport() {
        logger.trace("install touch bar support");
        if (TouchBarUtils.isTouchBarSupported()) {
            tabbedPane.addChangeListener(e -> {
                var comp = tabbedPane.getSelectedComponent();
                if (comp.equals(tabFilme)) {
                    tabDownloads.hideTouchBar();
                    tabFilme.showTouchBar();
                } else if (comp.equals(tabDownloads)) {
                    tabFilme.hideTouchBar();
                    tabDownloads.showTouchBar();
                }
            });
        }

    }

    @Override
    protected void setupNotificationCenter() {
        try {
            var notificationCenter = daten.notificationCenter();
            if (notificationCenter != null) {
                notificationCenter.close();
            }
        } catch (IOException e) {
            logger.error("error closing notification center", e);
        }

        final boolean showNotifications = config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS, true);
        // we need to figure if we have native support available
        config.setProperty(ApplicationConfiguration.APPLICATION_NATIVE_NOTIFICATIONS_SUPPORT, false);

        if (!showNotifications) {
            daten.setNotificationCenter(new NullNotificationCenter());
        } else {
            if (config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NATIVE_NOTIFICATIONS, false))
                daten.setNotificationCenter(new MacNotificationCenter());
            else
                daten.setNotificationCenter(new GenericNotificationCenter());
        }
    }

    @Override
    protected JPanel createTabFilme(@NotNull Daten daten) {
        return new TabFilmeMac(daten, this);
    }

    @Override
    protected JPanel createTabDownloads(@NotNull Daten daten) {
        return new TabDownloadsMac(daten, this);
    }

    @Override
    protected void shutdownComputer() {
        try {
            Runtime.getRuntime().exec("nohup bin/mv_shutdown_helper");
        } catch (IOException e) {
            logger.error(e);
        }
    }

    @Override
    protected void installMenuTabSwitchListener() {
        //do not use on OS X as it violates HIG...
    }

    @Override
    @Handler
    protected void handleInstallTabSwitchListenerEvent(InstallTabSwitchListenerEvent msg) {
        //do not use on OS X as it violates HIG...
    }

    @Override
    protected void initMenus() {
        super.initMenus();

        setupUserInterfaceForOsx();
    }

    @Override
    protected IndicatorThread createProgressIndicatorThread() {
        return new OsxIndicatorThread();
    }

    @Override
    protected void handleDownloadStart(DownloadStartEvent msg) {
        super.handleDownloadStart(msg);
        powerManager.disablePowerManagement();

        setDownloadsBadge(numDownloadsStarted.get());
    }

    @Override
    protected void handleDownloadFinishedEvent(DownloadFinishedEvent msg) {
        super.handleDownloadFinishedEvent(msg);

        final int numDownloads = numDownloadsStarted.get();
        if (numDownloads == 0)
            powerManager.enablePowerManagement();

        setDownloadsBadge(numDownloads);
    }

    /**
     * Set the OS X dock icon badge to the number of running downloads.
     *
     * @param numDownloads The number of active downloads.
     */
    private void setDownloadsBadge(int numDownloads) {
        if (Taskbar.isTaskbarSupported()) {
            var taskbar = Taskbar.getTaskbar();
            if (taskbar.isSupported(Taskbar.Feature.ICON_BADGE_NUMBER)) {
                if (numDownloads > 0)
                    taskbar.setIconBadge(Integer.toString(numDownloads));
                else {
                    taskbar.setIconBadge("");
                }
            }
        }
    }

    /**
     * Setup the UI for OS X
     */
    private void setupUserInterfaceForOsx() {
        Desktop desktop = Desktop.getDesktop();
        desktop.disableSuddenTermination();
        desktop.setQuitHandler((e, response) -> {
            if (!beenden(false, false)) {
                response.cancelQuit();
            } else {
                response.performQuit();
            }
        });
        desktop.setAboutHandler(e -> showAboutDialog());
        desktop.setPreferencesHandler(e -> getSettingsDialog().setVisible(true));
    }

    /**
     * Install MediathekView app icon in dock
     */
    private void setupDockIcon() {
        try {
            if (Taskbar.isTaskbarSupported()) {
                var taskbar = Taskbar.getTaskbar();
                if (taskbar.isSupported(Taskbar.Feature.ICON_IMAGE)) {
                    final URL url = this.getClass().getResource("/mediathek/res/MediathekView.png");
                    final BufferedImage appImage = ImageIO.read(url);
                    Taskbar.getTaskbar().setIconImage(appImage);
                }
            }
        } catch (IOException ex) {
            logger.error("OS X Application image could not be loaded", ex);
        }
    }
}
