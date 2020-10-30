package mediathek.mac;

import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import mediathek.config.Daten;
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
import java.io.File;
import java.io.IOException;
import java.net.URL;

@SuppressWarnings("serial")
public class MediathekGuiMac extends MediathekGui {
    private static final String SHUTDOWN_HELPER_APP_BINARY_PATH = "/Contents/MacOS/MediathekView Shutdown Helper";
    private final OsxPowerManager powerManager = new OsxPowerManager();
    protected static Logger logger = LogManager.getLogger(MediathekGuiMac.class);

    public MediathekGuiMac() {
        super();

        setupDockIcon();
    }

    @Override
    public void initializeSystemTray() {
        //we don´t use it on macOS
    }

    @Override
    protected void installTouchBarSupport() {
        logger.trace("install touch bar support");
        if (TouchBarUtils.isTouchBarSupported()) {
            //make filme tab touchbar visible by default, otherwise it will not appear...
            tabFilme.touchBar.show(MediathekGuiMac.this);
            final var tabbedPane = getTabbedPane();
            tabbedPane.addChangeListener(e -> {
                var comp = tabbedPane.getSelectedComponent();
                if (comp.equals(tabFilme)) {
                    tabDownloads.touchBar.hide(MediathekGuiMac.this);
                    tabFilme.touchBar.show(MediathekGuiMac.this);
                } else if (comp.equals(tabDownloads)) {
                    tabFilme.touchBar.hide(MediathekGuiMac.this);
                    tabDownloads.touchBar.show(MediathekGuiMac.this);
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

        final boolean showNotifications = config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS,true);
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
            var result = Spotlight.find("kMDItemCFBundleIdentifier == org.mediathekview.MediathekView-Shutdown-Helper");
            if (result.isEmpty())
                logger.error("could not locate mediathekview shutdown helper app");
            else {
                File appLocation = result.get(0);
                logger.debug("Shutdown Helper location: {}", appLocation.toString());
                logger.info("Executing shutdown helper");
                final ProcessBuilder builder = new ProcessBuilder(appLocation.toString() + SHUTDOWN_HELPER_APP_BINARY_PATH);
                builder.command().add("-shutdown");
                builder.start();
                logger.debug("shutdown helper app was launched");
            }
        } catch (Exception e) {
            logger.error("unexpected error occured", e);
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
        desktop.setPreferencesHandler(e -> showSettingsDialog());
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

    static class WorkaroundStage extends Stage {
        public WorkaroundStage() {
            initStyle(StageStyle.UTILITY);
            var root = new StackPane();
            root.setStyle("-fx-background-color: TRANSPARENT");

            var scene = new Scene(root, 1, 1);
            scene.setFill(javafx.scene.paint.Color.TRANSPARENT);

            setScene(scene);
            setWidth(1d);
            setHeight(1d);
            setOpacity(0d);
        }
    }
}
