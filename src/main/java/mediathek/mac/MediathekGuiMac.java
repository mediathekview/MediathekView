package mediathek.mac;

import com.formdev.flatlaf.util.SystemInfo;
import mediathek.config.Konstanten;
import mediathek.gui.actions.ShowAboutAction;
import mediathek.gui.messages.DownloadFinishedEvent;
import mediathek.gui.messages.DownloadStartEvent;
import mediathek.gui.messages.InstallTabSwitchListenerEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.notification.INotificationCenter;
import mediathek.tool.notification.MacNotificationCenter;
import mediathek.tool.threads.IndicatorThread;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;

public class MediathekGuiMac extends MediathekGui {
    protected static final Logger logger = LogManager.getLogger(MediathekGuiMac.class);
    private final OsxPowerManager powerManager = new OsxPowerManager();

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
    protected INotificationCenter getNotificationCenter() {
        return new MacNotificationCenter();
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

        if (desktop.isSupported(Desktop.Action.APP_QUIT_HANDLER))
            desktop.setQuitHandler((e, response) -> {
                if (!beenden(false, false)) {
                    response.cancelQuit();
                } else {
                    response.performQuit();
                }
            });

        if (desktop.isSupported(Desktop.Action.APP_ABOUT))
            desktop.setAboutHandler(e -> new ShowAboutAction().actionPerformed(null));

        if (desktop.isSupported(Desktop.Action.APP_PREFERENCES))
            desktop.setPreferencesHandler(e -> getSettingsDialog().setVisible(true));

        getRootPane().putClientProperty("apple.awt.windowTitleVisible", false);
        System.setProperty("apple.awt.application.appearance", "system");

        if (SystemInfo.isMacFullWindowContentSupported) {
            getRootPane().putClientProperty("apple.awt.fullWindowContent", true);
            getRootPane().putClientProperty("apple.awt.transparentTitleBar", true);

            commonToolBar.add(Box.createHorizontalStrut(70),0);
        }
    }
}
