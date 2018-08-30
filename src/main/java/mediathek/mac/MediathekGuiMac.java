package mediathek.mac;

import com.jidesoft.utils.SystemInfo;
import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import mSearch.tool.Log;
import mediathek.MediathekGui;
import mediathek.gui.bandwidth.MVBandwidthMonitorOSX;
import mediathek.gui.messages.DownloadFinishedEvent;
import mediathek.gui.messages.DownloadStartEvent;
import mediathek.gui.messages.InstallTabSwitchListenerEvent;
import mediathek.tool.threads.IndicatorThread;
import net.engio.mbassy.listener.Handler;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;

@SuppressWarnings("serial")
public class MediathekGuiMac extends MediathekGui {
    private static final String ACTION_KEY_MAC_F = "mac-f";

    public MediathekGuiMac() {
        super();

        setupDockIcon();
    }

    @Override
    protected void workaroundControlsFxNotificationBug() {
        Platform.runLater(() -> {
            controlsFxWorkaroundStage = new Stage(StageStyle.UTILITY);
            StackPane root = new StackPane();
            root.setStyle("-fx-background-color: TRANSPARENT");
            Scene scene = new Scene(root, 1, 1);
            scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
            controlsFxWorkaroundStage.setScene(scene);
            controlsFxWorkaroundStage.setWidth(1);
            controlsFxWorkaroundStage.setHeight(1);
            controlsFxWorkaroundStage.toBack();
            controlsFxWorkaroundStage.setOpacity(0d);
            controlsFxWorkaroundStage.show();
        });    }

    @Override
    protected void setupShutdownCommand() {
        shutdownCommand = new OsxShutdownComputerCommand();
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
    protected void initMenue() {
        super.initMenue();

        setupUserInterfaceForOsx();
        setupAcceleratorsForOsx();
    }

    @Override
    protected IndicatorThread createProgressIndicatorThread() {
        return new OsxIndicatorThread();
    }

    @Override
    protected void handleDownloadStart(DownloadStartEvent msg) {
        super.handleDownloadStart(msg);
        setDownloadsBadge(numDownloadsStarted.get());
    }

    @Override
    protected void handleDownloadFinishedEvent(DownloadFinishedEvent msg) {
        super.handleDownloadFinishedEvent(msg);
        setDownloadsBadge(numDownloadsStarted.get());
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

    @Override
    protected void setupSearchKeyForMac() {
        // für den Mac
        final JRootPane rootPane = getRootPane();
        rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_F, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), ACTION_KEY_MAC_F);
        rootPane.getActionMap().put(ACTION_KEY_MAC_F, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setFocusOnSearchField();
            }
        });
    }

    @Override
    protected void setupHelpMenu() {
        super.setupHelpMenu();
        //not needed on OSX, located in apple menu
        jMenuHilfe.remove(jSeparatorAboutApplication);
        jMenuHilfe.remove(jMenuItemAboutApplication);
    }

    @Override
    protected void createBandwidthMonitor(JFrame parent) {
        bandwidthMonitor = new MVBandwidthMonitorOSX(this);
    }

    /**
     * Keyboard shortcuts for some actions need to be changed for OS X
     */
    private void setupAcceleratorsForOsx() {
        jMenuItemFilmAbspielen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F6, InputEvent.META_DOWN_MASK));
        jMenuItemFilmAufzeichnen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F7, InputEvent.META_DOWN_MASK));
        jMenuItemBlacklist.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, InputEvent.META_DOWN_MASK));
        cbkBeschreibung.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, InputEvent.META_DOWN_MASK));
        jCheckBoxMenuItemVideoplayer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, InputEvent.META_DOWN_MASK));
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

        //Remove all menu items which don´t need to be displayed due to OS X´s native menu support
        if (SystemInfo.isMacOSX()) {
            //Datei->Beenden
            jMenuDatei.remove(jSeparator2);
            jMenuDatei.remove(jMenuItemBeenden);
            //Datei->Einstellungen
            jMenuDatei.remove(jMenuItemEinstellungen);
        }
    }

    private void setupDockIcon() {
        //TODO maybe useful on other platforms as well?
        //setup the MediathekView Dock Icon
        try {
            final URL url = this.getClass().getResource("/mediathek/res/MediathekView.png");
            final BufferedImage appImage = ImageIO.read(url);
            Taskbar.getTaskbar().setIconImage(appImage);
        } catch (IOException ex) {
            Log.errorLog(165623698, "OS X Application image could not be loaded");
        }
    }
}
