package mediathek.mac;

import com.apple.eawt.Application;
import com.jidesoft.utils.SystemInfo;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.gui.actions.ShowAboutDialogAction;
import mediathek.gui.bandwidth.MVBandwidthMonitorOSX;
import mediathek.gui.filmInformation.MVFilmInformationOSX;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;

@SuppressWarnings("serial")
public class MediathekGuiMac extends MediathekGui {
    private final Daten daten;
    /**
     * Repaint-Thread for progress indicator on OS X.
     */
    private Thread osxProgressIndicatorThread = null;

    public MediathekGuiMac(String[] ar) {
        super(ar);
        aboutAction = new ShowAboutDialogAction(this);
        daten = Daten.getInstance();
        //Window must be fully initialized to become fullscreen cadidate...
        setWindowFullscreenCapability();
    }

    /**
     * Enable Fullscreen window mode on OS X.
     * Depends on OS X only java classes.
     */
    private void setWindowFullscreenCapability() {
        try {
            Class.forName("com.apple.eawt.FullScreenUtilities")
                    .getMethod("setWindowCanFullScreen",Window.class,boolean.class)
                    .invoke(null, this,true);
        }
        catch (Exception ignored) {
        }
    }

    @Override
    protected void initMenue() {
        super.initMenue();

        setupUserInterfaceForOsx();
        setupAcceleratorsForOsx();
    }

    @Override
    protected void setupHelpMenu() {
        super.setupHelpMenu();
        //not needed on OSX, located in apple menu
        jMenuHilfe.remove(jSeparatorAboutApplication);
        jMenuHilfe.remove(jMenuItemAboutApplication);
    }


    @Override
    protected void createFilmInformationHUD(JFrame parent, JTabbedPane tabPane, Daten daten)
    {
        Daten.filmInfo = new MVFilmInformationOSX(parent);
    }

    @Override
    protected void createBandwidthMonitor(JFrame parent)
    {
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

    /**
     * Setup the OS X dock icon badge handler.
     */
    private void setupOsxDockIconBadge() {
        //setup the badge support for displaying active downloads
        Listener.addListener(new Listener(new int[]{
            Listener.EREIGNIS_START_EVENT, Listener.EREIGNIS_LISTE_DOWNLOADS}, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                final int activeDownloads = daten.getDownloadInfos().downloadStarts[4];
                final Application application = Application.getApplication();
                if (activeDownloads > 0) {
                    application.setDockIconBadge(String.valueOf(activeDownloads));

                    if (osxProgressIndicatorThread == null) {
                        osxProgressIndicatorThread = new OsxIndicatorThread();
                        osxProgressIndicatorThread.start();
                    }
                } else {
                    application.setDockIconBadge("");
                    if (osxProgressIndicatorThread != null) {
                        osxProgressIndicatorThread.interrupt();
                        osxProgressIndicatorThread = null;
                    }
                }
            }
        });
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

        setupDockIcon();

        //Remove all menu items which don´t need to be displayed due to OS X´s native menu support
        if (SystemInfo.isMacOSX()) {
            //Datei->Beenden
            jMenuDatei.remove(jSeparator2);
            jMenuDatei.remove(jMenuItemBeenden);
            //Datei->Einstellungen
            jMenuDatei.remove(jMenuItemEinstellungen);
        }

        setupOsxDockIconBadge();
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
