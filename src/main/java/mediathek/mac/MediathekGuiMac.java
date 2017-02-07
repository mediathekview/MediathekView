package mediathek.mac;

import com.apple.eawt.Application;
import mSearch.tool.Log;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.gui.bandwidth.MVBandwidthMonitorOSX;
import mediathek.gui.filmInformation.MVFilmInformationOSX;
import mediathek.gui.messages.DownloadFinishedEvent;
import mediathek.gui.messages.DownloadStartEvent;
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
import java.util.concurrent.atomic.AtomicInteger;

@SuppressWarnings("serial")
public class MediathekGuiMac extends MediathekGui {
    private static final String ACTION_KEY_MAC_F = "mac-f";
    /**
     * Repaint-Thread for progress indicator on OS X.
     */
    private Thread osxProgressIndicatorThread = null;
    private final AtomicInteger numDownloads = new AtomicInteger(0);

    public MediathekGuiMac(String[] ar) {
        super(ar);

        setupDockIcon();

        setSearchKeyForMac();

        //Window must be fully initialized to become fullscreen cadidate...
        setWindowFullscreenCapability();

        daten.getMessageBus().subscribe(this);
    }

    @Override
    protected void installAutomaticTabSwitcher() {
        //do not use tab switching in OS X
    }

    private void setSearchKeyForMac() {
        // für den Mac
        final JRootPane rootPane = getRootPane();
        rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_F, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), ACTION_KEY_MAC_F);
        rootPane.getActionMap().put(ACTION_KEY_MAC_F, new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setFocusSuchfeld();
            }
        });
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
    protected void installAboutMenuItem() {
        //do not install on OS X
    }

    @Override
    protected void createFilmInformationHUD(JTabbedPane tabPane, Daten daten) {
        filmInfo = new MVFilmInformationOSX(this);
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
        jMenuItemFilterLoeschen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F8, InputEvent.META_MASK));
        jMenuItemBlacklist.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, InputEvent.META_MASK));
        cbkBeschreibung.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, InputEvent.META_MASK));
        jCheckBoxMenuItemVideoplayer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, InputEvent.META_MASK));
    }

    @Handler
    public void handleDownloadStart(DownloadStartEvent msg) {
        final int numDL = numDownloads.incrementAndGet();

        setDownloadsBadge(numDL);

        if (osxProgressIndicatorThread == null) {
            osxProgressIndicatorThread = new OsxIndicatorThread();
            osxProgressIndicatorThread.start();
        }
    }

    @Handler
    public void handleDownloadFinishedEvent(DownloadFinishedEvent msg) {
        final int numDL = numDownloads.decrementAndGet();

        setDownloadsBadge(numDL);

        if (numDL == 0 && osxProgressIndicatorThread != null) {
            osxProgressIndicatorThread.interrupt();
            osxProgressIndicatorThread = null;
        }
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
        else
            app.setDockIconBadge("");
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

    /**
     * Setup the UI for OS X
     */
    private void setupUserInterfaceForOsx() {
        final Application application = Application.getApplication();
        application.disableSuddenTermination();
        application.setAboutHandler(aboutEvent -> aboutAction.actionPerformed(null));
        application.setPreferencesHandler(preferencesEvent -> showSettingsDialog());
        application.setQuitHandler((quitEvent, quitResponse) -> {
            if (!beenden(false, false)) {
                quitResponse.cancelQuit();
            } else {
                quitResponse.performQuit();
            }
        });
    }

    @Override
    protected void installSettingsAndTerminateMenuItems() {
        //do not install these on OSX
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
