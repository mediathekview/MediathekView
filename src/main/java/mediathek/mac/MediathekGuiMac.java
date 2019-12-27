package mediathek.mac;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import mediathek.config.Konstanten;
import mediathek.gui.messages.DownloadFinishedEvent;
import mediathek.gui.messages.DownloadStartEvent;
import mediathek.gui.messages.InstallTabSwitchListenerEvent;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.Log;
import mediathek.tool.threads.IndicatorThread;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.concurrent.TimeUnit;

@SuppressWarnings("serial")
public class MediathekGuiMac extends MediathekGui {
    private static final String SHUTDOWN_HELPER_APP_BINARY_PATH = "/Contents/MacOS/MediathekView Shutdown Helper";
    private final OsxPowerManager powerManager = new OsxPowerManager();
    private final Logger logger = LogManager.getLogger(MediathekGuiMac.class);
    protected Stage controlsFxWorkaroundStage;

    public MediathekGuiMac() {
        super();

        setupDockIcon();

        var versionCheckThread = new CheckMacOSVersion();
        versionCheckThread.start();
    }

    @Override
    public void initializeSystemTray() {
        //we don´t use it on macOS
    }

    @Override
    protected void closeControlsFxWorkaroundStage() {
        Platform.runLater(() -> {
            if (controlsFxWorkaroundStage != null)
                controlsFxWorkaroundStage.close();
        });
    }

    static class WorkaroundStage extends Stage {
        public WorkaroundStage() {
            initStyle(StageStyle.UTILITY);
            var root = new StackPane();
            root.setStyle("-fx-background-color: TRANSPARENT");

            var  scene = new Scene(root, 1, 1);
            scene.setFill(javafx.scene.paint.Color.TRANSPARENT);

            setScene(scene);
            setWidth(1d);
            setHeight(1d);
            setOpacity(0d);
        }
    }

    @Override
    protected void workaroundControlsFxNotificationBug() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            controlsFxWorkaroundStage = new WorkaroundStage();
            controlsFxWorkaroundStage.show();
            controlsFxWorkaroundStage.toBack();
        });
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
        if (numDownloads > 0)
            Taskbar.getTaskbar().setIconBadge(Integer.toString(numDownloads));
        else {
            Taskbar.getTaskbar().setIconBadge("");
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

    private void setupDockIcon() {
        //setup the MediathekView Dock Icon
        try {
            final URL url = this.getClass().getResource("/mediathek/res/MediathekView.png");
            final BufferedImage appImage = ImageIO.read(url);
            Taskbar.getTaskbar().setIconImage(appImage);
        } catch (IOException ex) {
            Log.errorLog(165623698, "OS X Application image could not be loaded");
        }
    }

    static class CheckMacOSVersion extends Thread {
        @Override
        public void run() {
            try {
                TimeUnit.SECONDS.sleep(5);
                SwingUtilities.invokeLater(() -> {
                    var version = SystemUtils.OS_VERSION;
                    if (version.contains("10.14") || version.contains("10.15")) {
                        var config = ApplicationConfiguration.getConfiguration();
                        boolean showWarning = config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_SPOTLIGHT_DISABLED_WARNING, true);
                        if (showWarning) {
                            Platform.runLater(() -> {
                                Alert alert = new Alert(Alert.AlertType.INFORMATION);
                                alert.setTitle(Konstanten.PROGRAMMNAME);
                                alert.setHeaderText("Spotlight-Kommentare schreiben");
                                alert.setContentText("Aufgrund neuer Sicherheitsfunktionen in macOS ist das Schreiben von " +
                                        "Spotlight-Kommentaren deaktiviert.\n" +
                                        "Wir arbeiten an einer zukünftigen Lösung.\n\n" +
                                        "Dieser Dialog wird nicht mehr angezeigt werden.");
                                alert.showAndWait();
                            });

                            config.setProperty(ApplicationConfiguration.APPLICATION_SHOW_SPOTLIGHT_DISABLED_WARNING, false);
                        }
                    }
                });
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
