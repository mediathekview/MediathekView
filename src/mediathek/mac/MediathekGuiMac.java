package mediathek.mac;

import com.apple.eawt.Application;
import com.jidesoft.utils.SystemInfo;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
import javax.imageio.ImageIO;
import javax.swing.*;
import mSearch.tool.Log;
import mediathek.MediathekGui;
import mediathek.daten.Daten;
import mSearch.tool.Listener;
import mSearch.tool.MVConfig;

/**
 * User: crystalpalace1977
 * Date: 07.03.15
 * Time: 23:12
 */
public class MediathekGuiMac extends MediathekGui {

    /**
     * Repaint-Thread for progress indicator on OS X.
     */
    private Thread osxProgressIndicatorThread = null;

    /**
     * Group manager for maximum number of Downloads Radio Buttons.
     * */
    private ButtonGroup group = null;

    public MediathekGuiMac(String[] ar) {
        super(ar);
    }

    @Override
    protected void initMenue() {
        super.initMenue();
        if (SystemInfo.isMacOSX()) {
            setupUserInterfaceForOsx();
            setupAcceleratorsForOsx();
        }
    }

    /**
     * Keyboard shortcuts for some actions need to be changed for OS X
     */
    private void setupAcceleratorsForOsx() {
        jMenuItemFilmAbspielen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F6, InputEvent.META_MASK));
        jMenuItemFilmAufzeichnen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F7, InputEvent.META_MASK));
        jMenuItemFilterLoeschen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F8, InputEvent.META_MASK));
        jMenuItemBlacklist.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, InputEvent.META_MASK));
        jCheckBoxMenuItemBeschreibung.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, InputEvent.META_MASK));
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
                final int activeDownloads = Daten.downloadInfos.downloadStarts[4];
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

    /**
     * Setup the UI for OS X
     */
    private void setupUserInterfaceForOsx() {
        final Application application = Application.getApplication();
        application.disableSuddenTermination();
        application.setAboutHandler(aboutEvent -> showAboutDialog());
        application.setPreferencesHandler(preferencesEvent -> Daten.dialogEinstellungen.setVisible(true));
        application.setQuitHandler((quitEvent, quitResponse) -> {
            if (!beenden(false, false)) {
                quitResponse.cancelQuit();
            } else {
                quitResponse.performQuit();
            }
        });

        //setup the MediathekView Dock Icon
        try {
            final URL url = this.getClass().getResource("/mediathek/res/MediathekView.png");
            final BufferedImage appImage = ImageIO.read(url);
            application.setDockIconImage(appImage);
        } catch (IOException ex) {
            Log.errorLog(165623698, "OS X Application image could not be loaded");
        }

        //Remove all menu items which don´t need to be displayed due to OS X´s native menu support
        if (SystemInfo.isMacOSX()) {
            //Datei->Beenden
            jMenuDatei.remove(jSeparator2);
            jMenuDatei.remove(jMenuItemBeenden);
            //Datei->Einstellungen
            jMenuDatei.remove(jMenuItemEinstellungen);
            //Hilfe->Über
            jMenuHilfe.remove(jSeparator4);
            jMenuHilfe.remove(jMenuItemAbout);
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

    private int getNumberOfDownloads() {
        int numDownloads;
        if (MVConfig.get(MVConfig.SYSTEM_MAX_DOWNLOAD).equals("")) {
            MVConfig.add(MVConfig.SYSTEM_MAX_DOWNLOAD, "1");
            numDownloads = 1;
        } else {
            numDownloads = Integer.parseInt(MVConfig.get(MVConfig.SYSTEM_MAX_DOWNLOAD));
        }

        return numDownloads;
    }

    @Override
    protected void setupMaximumNumberOfDownloadsMenuItem() {
        jMenuDownload.addSeparator();

        final JMenu numDownloadsMenu = new JMenu("Anzahl gleichzeitiger Downloads");
        group = new ButtonGroup();

        for (int i = 1; i <= 10; i++) {
            JRadioButtonMenuItem menuItem = new JRadioButtonMenuItem(Integer.toString(i));
            menuItem.addActionListener(e -> {
                final AbstractButton btn = (AbstractButton) e.getSource();
                if (btn != null) {
                    MVConfig.add(MVConfig.SYSTEM_MAX_DOWNLOAD, btn.getText());
                    Listener.notify(Listener.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName());
                }
            });
            group.add(menuItem);
            numDownloadsMenu.add(menuItem);
        }
        jMenuDownload.add(numDownloadsMenu);

        Listener.addListener(new Listener(Listener.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                //FIXME selection is not properly changed when JFrame settings dialog is displayed...needs to be JDialog in the future...
                //JMenuBar gets destroyed when a JFrame is shown
                //modal JDialog will keep it
                //System.out.println("LISTENER NUM DOWNLOADS CALLED with " + getNumberOfDownloads());
                setupMaxDownloadButtonSelection(getNumberOfDownloads());
            }
        });

        setupMaxDownloadButtonSelection(getNumberOfDownloads());
    }

    private void setupMaxDownloadButtonSelection(int numDownloads) {
        if (group == null) {
            //System.out.println("GROUP IS NULL");
            return;
        }

        final Enumeration<AbstractButton> elem = group.getElements();
        while (elem.hasMoreElements()) {
            final AbstractButton btn = elem.nextElement();
            if (btn.getText().equals(Integer.toString(numDownloads))) {
                btn.doClick();
                btn.setSelected(true);
                break;
            }
        }
    }

    @Override
    protected void setupBandwidthMenuItem() {
    }

    @Override
    protected void initSpinner() {
        //unused in OS X
    }
}
