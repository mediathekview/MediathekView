package mediathek.mac;

import com.apple.eawt.Application;
import com.jidesoft.utils.SystemInfo;
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

    public MediathekGuiMac(String[] ar) {
        super(ar);

        setupDockIcon();

        //Window must be fully initialized to become fullscreen cadidate...
        setWindowFullscreenCapability();
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

    /**
     * Enable Fullscreen window mode on OS X.
     * Depends on OS X only java classes.
     */
    private void setWindowFullscreenCapability() {
        try {
            Class.forName("com.apple.eawt.FullScreenUtilities")
                    .getMethod("setWindowCanFullScreen", Window.class, boolean.class)
                    .invoke(null, this, true);
        } catch (Exception ignored) {
        }
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
        final Application app = Application.getApplication();
        if (numDownloads > 0)
            app.setDockIconBadge(Integer.toString(numDownloads));
        else {
            app.setDockIconBadge("");
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
        jMenuItemFilmAbspielen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F6, InputEvent.META_MASK));
        jMenuItemFilmAufzeichnen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F7, InputEvent.META_MASK));
        jMenuItemBlacklist.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, InputEvent.META_MASK));
        cbkBeschreibung.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, InputEvent.META_MASK));
        jCheckBoxMenuItemVideoplayer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, InputEvent.META_MASK));
    }

    /**
     * Setup the UI for OS X
     */
    private void setupUserInterfaceForOsx() {
        final Application application = Application.getApplication();
        application.disableSuddenTermination();
        application.setAboutHandler(aboutEvent -> showAboutDialog());
        application.setPreferencesHandler(preferencesEvent -> showSettingsDialog());
        application.setQuitHandler((quitEvent, quitResponse) -> {
            if (!beenden(false, false)) {
                quitResponse.cancelQuit();
            } else {
                quitResponse.performQuit();
            }
        });

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
        //setup the MediathekView Dock Icon
        try {
            final Application application = Application.getApplication();
            final URL url = this.getClass().getResource("/mediathek/res/MediathekView.png");
            final BufferedImage appImage = ImageIO.read(url);
            application.setDockIconImage(appImage);
        } catch (IOException ex) {
            Log.errorLog(165623698, "OS X Application image could not be loaded");
        }
    }

    @Override
    protected void shutdownComputer() {
        //we cannot shutdown the system while we are running...
        //MV (or java) will prevent OS X shutdown process and there seems to be no way around it.
        //NASTY WORKAROUND:
        //use applescript to execute a scriptlet application which will wait 5 seconds until it
        //asks the system to shut down
        //meanwhile we MUST terminate MV WITHIN 5 seconds in order not to interrupt the
        //shutdown process :(
        //AND this whole shit works ONLY with osascript, not with the java script engine...
        //Scriptlet(executable) content:
        //delay 5
        //tell application "system events" to shut down
        //EOF
        //The OSX_Shutdown scriptlet application is provided in the official MV app bundle.
        try {
            final ProcessBuilder builder = new ProcessBuilder("/usr/bin/osascript", "-e");
            builder.command().add("tell application \"OSX_Shutdown\" to activate");
            builder.start();
        } catch (Exception ignored) {
        }
    }
}
