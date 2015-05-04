/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek;

import com.jidesoft.utils.SystemInfo;
import java.awt.AlphaComposite;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.SplashScreen;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import javax.swing.AbstractAction;
import javax.swing.InputMap;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.controller.CheckUpdate;
import mediathek.controller.IoXmlLesen;
import mediathek.controller.Log;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.gui.GuiAbo;
import mediathek.gui.GuiDebug;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.gui.MVBandwidthMonitor;
import mediathek.gui.MVDownloadInfo;
import mediathek.gui.MVStatusBar;
import mediathek.gui.MVToolBar;
import mediathek.gui.MVTray;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogBeenden;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialog.DialogStarteinstellungen;
import mediathek.gui.MVAboutDialog;
import mediathek.gui.dialog.MVFilmInformation;
import mediathek.gui.MVHelpDialog;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.gui.dialogEinstellungen.PanelMeldungen;
import mediathek.res.GetIcon;
import mediathek.tool.Duration;
import mediathek.tool.Funktionen;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVFont;
import mediathek.tool.MVFrame;
import mediathek.tool.MVMessageDialog;
import msearch.filmeSuchen.MSListenerFilmeLaden;
import msearch.filmeSuchen.MSListenerFilmeLadenEvent;

public class MediathekGui extends JFrame {

    private Daten daten;
    protected final DialogEinstellungen dialogEinstellungen;
    private final JSpinner jSpinnerAnzahl = new JSpinner(new SpinnerNumberModel(1, 1, 9, 1));
    private final JLabel jLabelAnzahl = new JLabel("Anzahl gleichzeitige Downloads");
    private final JPanel jPanelAnzahl = new JPanel();
    private PanelVorlage panelMeldungen = new PanelVorlage(daten, this);
    private JSplitPane splitPane = null;
    private final MVToolBar mVToolBar;
    private MVStatusBar statusBar;
    private final MVFrame[] frames = new MVFrame[3]; // Downloads, Abos, Meldungen
    private JCheckBoxMenuItem jCheckBoxFilterAnzeigen = new JCheckBoxMenuItem();
    private JCheckBoxMenuItem jCheckBoxFilterExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxDownloadAnzeigen = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxDownloadExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxAboAnzeigen = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxAboExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxMeldungenAnzeigen = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxMeldungenExtrafenster = new JCheckBoxMenuItem();
    private MVTray tray = null;

    /**
     * Bandwidth monitoring for downloads.
     */
    private MVBandwidthMonitor bandwidthMonitor = null;
    private MVDownloadInfo mvDownloadInfo = null;

    /**
     * Legt die statusbar an.
     */
    private void createStatusBar() {
        statusBar = new MVStatusBar(daten);

        JScrollPane js = new JScrollPane();
        js.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        js.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        js.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
        js.setViewportView(statusBar.getComponent());

        jPanelInfo.add(js, BorderLayout.CENTER);
    }

    public MVStatusBar getStatusBar() {
        return statusBar;
    }

    public String getFilterTextFromSearchField() {
        return mVToolBar.jTextFieldFilter.getText();
    }
    /**
     * The JVM {@link java.awt.SplashScreen} storage
     */
    private SplashScreen splash = null;
    /**
     * Store the splash screen {@link Graphics2D} context here for reuse
     */
    private Graphics2D splashScreenContext = null;
    /**
     * helper variable to calculate splash screen progress
     */
    private int splashScreenProgress = 0;

    /**
     * Update the {@link java.awt.SplashScreen} with the given text
     *
     * @param text The text which is to be displayed.
     */
    public void updateSplashScreenText(final String text) {
        //bail out when we don´ have a splash screen...
        if (splashScreenContext == null) {
            return;
        }

        final int y = 430;
        final int x = 120;
        final int width = 300;
        final int maxSteps = 11; // KEEP THIS CURRENT!

        splashScreenProgress++;
        //System.out.println("HOW_MANY_STEPS: " + splashScreenProgress);

        splashScreenContext.setRenderingHint(
                RenderingHints.KEY_TEXT_ANTIALIASING,
                RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        //clear the drawing area...
        splashScreenContext.setComposite(AlphaComposite.Clear);
        splashScreenContext.fillRect(x, (y - 10), width, 40);
        splashScreenContext.setPaintMode();
        //paint the text string...
        splashScreenContext.setFont(new Font("SansSerif", Font.BOLD, 12));
        splashScreenContext.setColor(Color.WHITE);
        splashScreenContext.drawString(text, x, y + 2);
        // paint the full progress indicator...
        splashScreenContext.setColor(Color.BLUE);
        splashScreenContext.fillRect(x, y - 15, width, 5);
        //paint how much is done...
        splashScreenContext.setColor(Color.GREEN);
        splashScreenContext.fillRect(x, y - 15, splashScreenProgress * (width / maxSteps), 5);
        splash.update();
    }

    /**
     * Initialize the Splash Screen variables.
     */
    private void initializeSplashScreen() {
        try {
            splash = SplashScreen.getSplashScreen();
            if (splash != null) {
                splashScreenContext = splash.createGraphics();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public MediathekGui(String[] ar, final boolean maximized) {
        super();
        initializeSplashScreen();

        String pfad = "";
        initComponents();
        if (ar != null) {
            if (ar.length > 0) {
                if (!ar[0].startsWith("-")) {
                    if (!ar[0].endsWith(File.separator)) {
                        ar[0] += File.separator;
                    }
                    pfad = ar[0];
                }
            }
        }

        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE); // soll abgefangen werden
        setIconImage(Toolkit.getDefaultToolkit().getImage(MediathekGui.class.getResource("/mediathek/res/MediathekView_k.gif")));
        //Hier wird F10 default Funktion unterbunden:
        InputMap im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(KeyStroke.getKeyStroke("F10"), "none");

        updateSplashScreenText("Anwendungsdaten laden...");
        Duration duration = new Duration(MediathekGui.class.getSimpleName());
        duration.ping("Start");

        daten = new Daten(pfad, this);
        duration.ping("Daten");

        Log.startMeldungen();
        createStatusBar();
        mVToolBar = new MVToolBar(daten);
        jPanelToolBar.setLayout(new BorderLayout());
        jPanelToolBar.add(mVToolBar, BorderLayout.CENTER);

        //create the Film Information HUD
        daten.filmInfoHud = new MVFilmInformation(this, jTabbedPane, daten);
        duration.ping("HUD");

        if (IoXmlLesen.einstellungenExistieren()) {
            // gibt schon Programmeinstellungen, dann damit starten
            daten.allesLaden();
            updateSplashScreenText("GUI Initialisieren...");
        } else {
            // erster Start
            Daten.mVReplaceList.init(); // einmal ein Muster anlegen, für Linux/OS X ist es bereits aktiv!
            new DialogStarteinstellungen(this, daten).setVisible(true);
            this.pack();
        }
        duration.ping("Alles laden");

        setOrgTitel();
        setLookAndFeel();
        duration.ping("LookAndFeel");
        init();
        duration.ping("init");
        setSize(maximized);
        duration.ping("setSize");

        // Dialog mit den Programmeinstellungen einrichten
        dialogEinstellungen = new DialogEinstellungen(this, daten);

        // Prüfen obs ein Programmupdate gibt
        new CheckUpdate(this, daten).suchen();
        duration.ping("CheckUpdate");

        if (GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUTO) {
            if (Daten.listeFilme.isTooOld()) {
                Log.systemMeldung("Neue Filmliste laden");
                Daten.filmeLaden.importFilmliste("", true);
            }
        }
        duration.ping("Filmliste laden");

        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_ORG_TITEL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setOrgTitel();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_PROGRAMM_AKTUELL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitelAllesAktuell();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitelUpdate();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxMenuItemBeschreibung.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN)));
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxFilterAnzeigen.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER)));
                jCheckBoxFilterExtrafenster.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_FILTER)));
            }
        });

        // für den Mac
        final JRootPane rootPane = getRootPane();
        rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_F, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "mac-f");
        rootPane.getActionMap().put("mac-f", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setFocusSuchfeld();
            }
        });

        setFocusSuchfeld();

        cbBandwidthDisplay.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_BANDWIDTH_MONITOR_VISIBLE)));
        if (SystemInfo.isMacOSX()) {
            bandwidthMonitor = new MVBandwidthMonitor(this, cbBandwidthDisplay);
            bandwidthMonitor.toggleVisibility();
            cbBandwidthDisplay.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    bandwidthMonitor.toggleVisibility();
                }
            });
        } else {
            mvDownloadInfo = new MVDownloadInfo(this, cbBandwidthDisplay);
            mvDownloadInfo.toggleVisibility();
            cbBandwidthDisplay.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    mvDownloadInfo.toggleVisibility();
                }
            });

        }
        duration.ping("Gui steht!");
    }

    private void setFocusSuchfeld() {
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_SUCHFELD_FOCUS_SETZEN, MediathekGui.class.getName());
        if (!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER))) {
            mVToolBar.jTextFieldFilter.requestFocus();
            mVToolBar.jTextFieldFilter.setCaretPosition(0);
        }
    }

    /**
     * This will set the Look&Feel based on Application Preferences. In case of
     * error it will always reset to system LAF.
     */
    private void setLookAndFeel() {
        try {
            String laf = Daten.mVConfig.get(MVConfig.SYSTEM_LOOK);
            //if we have the old values, reset to System LAF
            if (laf.equals("") || laf.length() == 1) {
                if (Funktionen.getOs() != Funktionen.OperatingSystemType.LINUX) {
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                }
            } else {
                //otherwise set the requested UI
                laf = Daten.mVConfig.get(MVConfig.SYSTEM_LOOK);
                UIManager.setLookAndFeel(laf);
            }
            SwingUtilities.updateComponentTreeUI(this);
            for (Frame f : Frame.getFrames()) {
                SwingUtilities.updateComponentTreeUI(f);
                for (Window w : f.getOwnedWindows()) {
                    SwingUtilities.updateComponentTreeUI(w);
                }
            }
        } catch (Exception ignored) {
            //update the LAF parameter, just in case we tried to load a non-existing LAF before
            Daten.mVConfig.add(MVConfig.SYSTEM_LOOK, UIManager.getSystemLookAndFeelClassName());
        }
    }

    //===================================
    // public
    //===================================
    public void setToolbar(String state) {
        mVToolBar.setToolbar(state);
        switch (state) {
            case "":
                buttonAus();
                break;
            case MVToolBar.TOOLBAR_TAB_FILME:
                buttonAus();
                jMenuItemFilmAbspielen.setEnabled(true);
                jMenuItemFilmAufzeichnen.setEnabled(true);
                jMenuItemFilterLoeschen.setEnabled(true);
                jMenuItemBlacklist.setEnabled(true);
                break;
            case MVToolBar.TOOLBAR_TAB_DOWNLOADS:
                buttonAus();
                jMenuItemDownloadStarten.setEnabled(true);
                jMenuItemDownloadStoppen.setEnabled(true);
                jMenuItemDownloadAlleStoppen.setEnabled(true);
                jMenuItemDownloadWartendeStoppen.setEnabled(true);
                jMenuItemDownloadsAktualisieren.setEnabled(true);
                jMenuItemDownloadAbspielen.setEnabled(true);
                jMenuItemDownloadsAufraeumen.setEnabled(true);
                jMenuItemDownloadsLoeschen.setEnabled(true);
                jMenuItemDownloadsAlleStarten.setEnabled(true);
                jMenuItemDownloadStartTime.setEnabled(true);
                jMenuItemDownloadAendern.setEnabled(true);
                jMenuItemDownloadsZurueckstellen.setEnabled(true);
                jMenuItemDownloadVorziehen.setEnabled(true);
                jMenuItemDownloadShutDown.setEnabled(true);
                jSpinnerAnzahl.setEnabled(true);
                jLabelAnzahl.setEnabled(true);
                break;
            case MVToolBar.TOOLBAR_TAB_ABOS:
                buttonAus();
                jMenuItemAbosEinschalten.setEnabled(true);
                jMenuItemAbosAusschalten.setEnabled(true);
                jMenuItemAbosLoeschen.setEnabled(true);
                jMenuItemAbosAendern.setEnabled(true);
                jMenuItemAboNeu.setEnabled(true);
                break;
        }
    }

    public void videoplayerAnzeigen(boolean anz) {
        jCheckBoxMenuItemVideoplayer.setSelected(!anz);
    }

    //===================================
    // private
    //===================================
    private void setOrgTitel() {
        this.setTitle(Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION);
    }

    private void setTitelUpdate() {
        setTitle("Ein Programmupdate ist verfügbar");
    }

    private void setTitelAllesAktuell() {
        setTitle("Programmversion ist aktuell");
    }

    private void buttonAus() {
        // Menü
        jMenuItemFilmAbspielen.setEnabled(false);
        jMenuItemFilmAufzeichnen.setEnabled(false);
        jMenuItemFilterLoeschen.setEnabled(false);
        jMenuItemBlacklist.setEnabled(false);
        jMenuItemDownloadsAktualisieren.setEnabled(false);
        jMenuItemDownloadAbspielen.setEnabled(false);
        jMenuItemDownloadsAufraeumen.setEnabled(false);
        jMenuItemDownloadsLoeschen.setEnabled(false);
        jMenuItemDownloadsAlleStarten.setEnabled(false);
        jMenuItemDownloadStartTime.setEnabled(false);
        jMenuItemDownloadStarten.setEnabled(false);
        jMenuItemDownloadStoppen.setEnabled(false);
        jMenuItemDownloadAlleStoppen.setEnabled(false);
        jMenuItemDownloadWartendeStoppen.setEnabled(false);
        jMenuItemDownloadAendern.setEnabled(false);
        jMenuItemDownloadsZurueckstellen.setEnabled(false);
        jMenuItemDownloadVorziehen.setEnabled(false);
        jMenuItemDownloadShutDown.setEnabled(false);
        jSpinnerAnzahl.setEnabled(false);
        jLabelAnzahl.setEnabled(false);
        jMenuItemAbosEinschalten.setEnabled(false);
        jMenuItemAbosAusschalten.setEnabled(false);
        jMenuItemAbosLoeschen.setEnabled(false);
        jMenuItemAbosAendern.setEnabled(false);
        jMenuItemAboNeu.setEnabled(false);
    }

    private void setSize(boolean max) {
        if (max || Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_MAX))) {
            this.setExtendedState(Frame.MAXIMIZED_BOTH);
        } else {
            GuiFunktionen.setSize(MVConfig.SYSTEM_GROESSE, this, null);
        }
        try {
            int divider = Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_BREITE_MELDUNGEN));
            if (divider > 0) {
                splitPane.setDividerLocation(divider);
            }
        } catch (NumberFormatException ignored) {
        }
    }

    private void init() {
        jMenuItemFilmlisteLaden.setIcon(GetIcon.getProgramIcon("filmlisteLaden_16.png"));
        jMenuItemEinstellungen.setIcon(GetIcon.getProgramIcon("configure_16.png"));
        jMenuItemBeenden.setIcon(GetIcon.getProgramIcon("beenden_16.png"));
        jMenuItemFilmAbspielen.setIcon(GetIcon.getProgramIcon("film_start_16.png"));
        jMenuItemFilmAufzeichnen.setIcon(GetIcon.getProgramIcon("film_rec_16.png"));
        jMenuItemBlacklist.setIcon(GetIcon.getProgramIcon("blacklist_16.png"));
        jMenuItemFilterLoeschen.setIcon(GetIcon.getProgramIcon("clear_16.png"));
        jMenuItemDownloadsAlleStarten.setIcon(GetIcon.getProgramIcon("download_alleStarten_16.png"));
        jMenuItemDownloadStartTime.setIcon(GetIcon.getProgramIcon("download_alleStarten_16.png"));
        jMenuItemDownloadAlleStoppen.setIcon(GetIcon.getProgramIcon("download_stop_16.png"));
        jMenuItemDownloadWartendeStoppen.setIcon(GetIcon.getProgramIcon("download_stop_16.png"));
        jMenuItemDownloadStarten.setIcon(GetIcon.getProgramIcon("download_start_16.png"));
        jMenuItemDownloadStoppen.setIcon(GetIcon.getProgramIcon("download_stop_16.png"));
        jMenuItemDownloadVorziehen.setIcon(GetIcon.getProgramIcon("move_up_16.png"));
        jMenuItemDownloadsZurueckstellen.setIcon(GetIcon.getProgramIcon("undo_16.png"));
        jMenuItemDownloadsLoeschen.setIcon(GetIcon.getProgramIcon("download_del_16.png"));
        jMenuItemDownloadAendern.setIcon(GetIcon.getProgramIcon("configure_16.png"));
        jMenuItemDownloadsAktualisieren.setIcon(GetIcon.getProgramIcon("view-refresh_16.png"));
        jMenuItemDownloadAbspielen.setIcon(GetIcon.getProgramIcon("film_start_16.png"));
        jMenuItemDownloadsAufraeumen.setIcon(GetIcon.getProgramIcon("download_clear_16.png"));
        jMenuItemDownloadShutDown.setIcon(GetIcon.getProgramIcon("beenden_16.png"));
        jMenuItemAbosEinschalten.setIcon(GetIcon.getProgramIcon("ja_16.png"));
        jMenuItemAbosAusschalten.setIcon(GetIcon.getProgramIcon("nein_16.png"));
        jMenuItemAbosLoeschen.setIcon(GetIcon.getProgramIcon("del_16.png"));
        jMenuItemAbosAendern.setIcon(GetIcon.getProgramIcon("configure_16.png"));
        jMenuItemAboNeu.setIcon(GetIcon.getProgramIcon("add_16.png"));
        jMenuItemAnleitung.setIcon(GetIcon.getProgramIcon("help_16.png"));
        initTabs();
        initMenue();
        mVToolBar.loadVisible(); // erst jetzt sind die Einstellungen geladen!
        Daten.filmeLaden.addAdListener(new MSListenerFilmeLaden() {
            @Override
            public void start(MSListenerFilmeLadenEvent event) {
                jMenuItemFilmlisteLaden.setEnabled(false);
            }

            @Override
            public void progress(MSListenerFilmeLadenEvent event) {
            }

            @Override
            public void fertig(MSListenerFilmeLadenEvent event) {
                jMenuItemFilmlisteLaden.setEnabled(true);
                daten.allesSpeichern(); // damit nichts verlorengeht
            }
        });
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                if (tray != null && !SystemInfo.isMacOSX() && Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_USE_TRAY))) {
                    daten.mediathekGui.setVisible(false);
                } else {
                    beenden(false, false);
                }
            }
        });
        setTray();
    }

    public void setTray() {
        if (tray == null && Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_USE_TRAY))) {
            tray = new MVTray(daten).systemTray();
        } else if (tray != null && !Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_USE_TRAY))) {
            tray.beenden();
            tray = null;
        }
    }

    public void hideFrame(String state) {
        switch (state) {
            case MVToolBar.TOOLBAR_TAB_DOWNLOADS:
                jCheckBoxDownloadAnzeigen.setSelected(false);
                Daten.mVConfig.add(MVConfig.SYSTEM_VIS_DOWNLOAD, Boolean.toString(false));
                break;
            case MVToolBar.TOOLBAR_TAB_ABOS:
                jCheckBoxAboAnzeigen.setSelected(false);
                Daten.mVConfig.add(MVConfig.SYSTEM_VIS_ABO, Boolean.toString(false));
                break;
            case MVToolBar.TOOLBAR_TAB_MELDUNGEN:
                jCheckBoxMeldungenAnzeigen.setSelected(false);
                Daten.mVConfig.add(MVConfig.SYSTEM_VIS_MELDUNGEN, Boolean.toString(false));
                break;
        }
        initFrames();
    }

    public void initFrames() {
        // Downloads
        int nr = 1;
        if (!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_DOWNLOAD))) {
            hide(0, daten.guiDownloads);
        } else {
            if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_DOWNLOAD))) {
                setFrame(0, MVConfig.SYSTEM_GROESSE_DOWNLOAD, daten.guiDownloads, MVToolBar.TOOLBAR_TAB_DOWNLOADS, "Downloads");
            } else {
                setTab(0, daten.guiDownloads, "Downloads", nr++);
            }
        }
        // Abos
        if (!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_ABO))) {
            hide(1, daten.guiAbo);
        } else {
            if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_ABO))) {
                setFrame(1, MVConfig.SYSTEM_GROESSE_ABO, daten.guiAbo, MVToolBar.TOOLBAR_TAB_ABOS, "Abos");
            } else {
                setTab(1, daten.guiAbo, "Abos", nr++);
            }
        }
        // Meldungen
        if (!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_MELDUNGEN))) {
            hide(2, panelMeldungen);
        } else {
            if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_MELDUNGEN))) {
                setFrame(2, MVConfig.SYSTEM_GROESSE_MELDUNGEN, panelMeldungen, MVToolBar.TOOLBAR_TAB_MELDUNGEN, "Meldungen");
            } else {
                setTab(2, panelMeldungen, "Meldungen", nr);
            }
        }
        mVToolBar.loadVisible(); // die können sich im externen Fenster geändert haben
        jTabbedPane.setSelectedIndex(0);
        daten.guiFilme.isShown();
    }

    private void hide(int nrFrameArr, PanelVorlage panelVorlage) {
        panelVorlage.solo = true;
        if (frames[nrFrameArr] != null) {
            frames[nrFrameArr].dispose();
            frames[nrFrameArr] = null;
        }
        if (tabContain(panelVorlage)) {
            jTabbedPane.remove(panelVorlage);
        }
    }

    private void setFrame(int nrFrameArr, String nrGroesse, PanelVorlage panelVorlage, String sparte, String titel) {
        panelVorlage.solo = true;
        if (frames[nrFrameArr] == null) {
            if (tabContain(panelVorlage)) {
                jTabbedPane.remove(panelVorlage);
            }
            frames[nrFrameArr] = new MVFrame(daten, panelVorlage, sparte, titel);
            frames[nrFrameArr].setSize(nrGroesse);
        }
        frames[nrFrameArr].setVisible(true);
    }

    private void setTab(int nrFrameArr, PanelVorlage panelVorlage, String titel, int nrTab) {
        if (frames[nrFrameArr] != null) {
            frames[nrFrameArr].dispose();
            frames[nrFrameArr] = null;
        }
        if (!tabContain(panelVorlage)) {
            jTabbedPane.add(panelVorlage, nrTab);
            jTabbedPane.setTitleAt(nrTab, titel);
        }
        panelVorlage.solo = false;
    }

    private boolean tabContain(Component check) {
        Component[] c = jTabbedPane.getComponents();
        for (Component co : c) {
            if (co.equals(check)) {
                return true;
            }
        }
        return false;
    }

    private void initTabs() {
        daten.guiDownloads = new GuiDownloads(daten, daten.mediathekGui);
        daten.guiAbo = new GuiAbo(daten, daten.mediathekGui);

        daten.guiFilme = new GuiFilme(daten, daten.mediathekGui);
        daten.guiFilme.init();
        jTabbedPane.addTab("Filme", daten.guiFilme);

        // jetzt noch den Rest
        panelMeldungen = new PanelVorlage(daten, this) {

            @Override
            public void isShown() {
                if (!solo) {
                    setToolbar(MVToolBar.TOOLBAR_NIX);
                    statusBar.setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.FILME);
                }
            }
        };
        PanelMeldungen panelMeldungenSystem = new PanelMeldungen(daten, daten.mediathekGui, Log.textSystem, ListenerMediathekView.EREIGNIS_LOG_SYSTEM, "Systemmeldungen");
        PanelMeldungen panelMeldungenPlayer = new PanelMeldungen(daten, daten.mediathekGui, Log.textProgramm, ListenerMediathekView.EREIGNIS_LOG_PLAYER, "Meldungen Hilfsprogramme");
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                panelMeldungenSystem,
                panelMeldungenPlayer);
        Log.panelMeldungenPlayer = panelMeldungenPlayer;
        Log.panelMeldungenSystem = panelMeldungenSystem;

        panelMeldungen.setLayout(new BorderLayout());
        panelMeldungen.add(splitPane, BorderLayout.CENTER);
        if (Daten.debug) {
            Daten.guiDebug = new GuiDebug(daten);
            jTabbedPane.addTab("Debug", Daten.guiDebug);
        }
        initFrames();
    }

    private void initSpinner() {
        if (Daten.mVConfig.get(MVConfig.SYSTEM_MAX_DOWNLOAD).equals("")) {
            jSpinnerAnzahl.setValue(1);
            Daten.mVConfig.add(MVConfig.SYSTEM_MAX_DOWNLOAD, "1");
        } else {
            jSpinnerAnzahl.setValue(Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_MAX_DOWNLOAD)));
        }
    }

    protected void initMenue() {
        initSpinner();
        // Anzahl gleichzeitiger Downlaods
        jPanelAnzahl.setLayout(new BorderLayout());
        jPanelAnzahl.add(jLabelAnzahl, BorderLayout.WEST);
        jPanelAnzahl.add(jSpinnerAnzahl, BorderLayout.EAST);
        jLabelAnzahl.setIcon(GetIcon.getProgramIcon("up_down_16.png"));
        jSpinnerAnzahl.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent arg0) {
                Daten.mVConfig.add(MVConfig.SYSTEM_MAX_DOWNLOAD,
                        String.valueOf(((Number) jSpinnerAnzahl.getModel().getValue()).intValue()));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName());
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                initSpinner();
            }
        });
        //jMenuDownload.add(jPanelAnzahl, 14);
        jMenuDownload.add(jPanelAnzahl);
        // Datei
        jMenuItemEinstellungen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                dialogEinstellungen.setVisible(true);
            }
        });
        jMenuItemBeenden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                beenden(false, false);
            }
        });
        // Filme
        jMenuItemFilmlisteLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.filmeLaden.filmeLaden(daten, false);
            }
        });
        jMenuItemFilmAbspielen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiFilme.guiFilmeFilmAbspielen();
            }
        });
        jMenuItemFilmAufzeichnen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiFilme.guiFilmeFilmSpeichern();
            }
        });
        jMenuItemFilterLoeschen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiFilme.guiFilmeFilterLoeschen();
            }
        });
        jMenuItemBlacklist.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                DialogLeer dialog = new DialogLeer(daten.mediathekGui, true);
                dialog.init("Blacklist", new PanelBlacklist(daten, daten.mediathekGui, PanelBlacklist.class.getName() + "_2"));
                dialog.setVisible(true);
            }
        });

        // Downloads
        jMenuItemDownloadsAktualisieren.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.aktualisieren();
            }
        });
        jMenuItemDownloadAbspielen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.filmAbspielen();
            }
        });
        jMenuItemDownloadsAufraeumen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.aufraeumen();
            }
        });
        jMenuItemDownloadsLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.loeschen();
            }
        });
        jMenuItemDownloadsAlleStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.starten(true /* alle */);
            }
        });
        jMenuItemDownloadStartTime.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.startAtTime(true);
            }
        });
        jMenuItemDownloadStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.starten(false /* alle */);
            }
        });
        jMenuItemDownloadsZurueckstellen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.zurueckstellen();
            }
        });
        jMenuItemDownloadVorziehen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.vorziehen();
            }
        });
        jMenuItemDownloadAendern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.aendern();
            }
        });
        jMenuItemDownloadAlleStoppen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.stoppen(true /* alle */);
            }
        });
        jMenuItemDownloadWartendeStoppen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.wartendeStoppen();
            }
        });
        jMenuItemDownloadStoppen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.stoppen(false /* alle */);
            }
        });
        jMenuItemDownloadShutDown.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (Daten.listeDownloads.nochNichtFertigeDownloads() > 0) {
                    // ansonsten gibts keine laufenden Downloads auf die man warten sollte
                    beenden(true /*Dialog auf "warten" einstellen*/, false /*shutdown computer*/);
                } else {
                    MVMessageDialog.showMessageDialog(daten.mediathekGui, "Die Downloads müssen zuerst gestartet werden.",
                            "Keine laufenden Downloads!", JOptionPane.ERROR_MESSAGE);
                }
            }
        });

        // Abo
        jMenuItemAbosEinschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.einAus(true);
            }
        });
        jMenuItemAbosAusschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.einAus(false);
            }
        });
        jMenuItemAbosLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.loeschen();
            }
        });
        jMenuItemAbosAendern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.aendern();
            }
        });
        jMenuItemAboNeu.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.neu();
            }
        });

        // Ansicht
        jCheckBoxMenuItemToolBar.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_TOOLBAR_ALLES_ANZEIGEN)));
        mVToolBar.setVisible(jCheckBoxMenuItemToolBar.isSelected());
        jCheckBoxMenuItemToolBar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_TOOLBAR_ALLES_ANZEIGEN, Boolean.toString(jCheckBoxMenuItemToolBar.isSelected()));
                mVToolBar.setVisible(jCheckBoxMenuItemToolBar.isSelected());
            }
        });
        jCheckBoxMenuItemVideoplayer.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
        jCheckBoxMenuItemVideoplayer.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, String.valueOf(jCheckBoxMenuItemVideoplayer.isSelected()));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName());
            }
        });
        jCheckBoxMenuItemBeschreibung.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN)));
        jCheckBoxMenuItemBeschreibung.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN, String.valueOf(jCheckBoxMenuItemBeschreibung.isSelected()));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName());
            }
        });
        jMenuItemSchriftGr.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                MVFont.setFontSize(true);
            }
        });
        jMenuItemSchriftKl.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                MVFont.setFontSize(false);
            }
        });
        jMenuItemSchriftNormal.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                MVFont.resetFontSize();
            }
        });
        // ============================
        // Filter
        jMenuAnsicht.add(new JSeparator());
        jCheckBoxFilterAnzeigen.setText("Filter anzeigen");
        jCheckBoxFilterExtrafenster.setText("in Extrafenster");
        jCheckBoxFilterExtrafenster.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jMenuAnsicht.add(jCheckBoxFilterAnzeigen);
        jMenuAnsicht.add(jCheckBoxFilterExtrafenster);
        jCheckBoxFilterAnzeigen.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER)));
        jCheckBoxFilterAnzeigen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_VIS_FILTER, Boolean.toString(jCheckBoxFilterAnzeigen.isSelected()));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getSimpleName());
            }
        });
        jCheckBoxFilterExtrafenster.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_FILTER)));
        jCheckBoxFilterExtrafenster.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_FENSTER_FILTER, Boolean.toString(jCheckBoxFilterExtrafenster.isSelected()));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getSimpleName());
            }
        });
        // ============================
        // Downloads
        jMenuAnsicht.add(new JSeparator());
        jCheckBoxDownloadAnzeigen.setText("Downloads anzeigen");
        jCheckBoxDownloadExtrafenster.setText("in Extrafenster");
        jCheckBoxDownloadExtrafenster.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jMenuAnsicht.add(jCheckBoxDownloadAnzeigen);
        jMenuAnsicht.add(jCheckBoxDownloadExtrafenster);
        jCheckBoxDownloadAnzeigen.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_DOWNLOAD)));
        jCheckBoxDownloadAnzeigen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_VIS_DOWNLOAD, Boolean.toString(jCheckBoxDownloadAnzeigen.isSelected()));
                initFrames();
            }
        });
        jCheckBoxDownloadExtrafenster.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_DOWNLOAD)));
        jCheckBoxDownloadExtrafenster.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_FENSTER_DOWNLOAD, Boolean.toString(jCheckBoxDownloadExtrafenster.isSelected()));
                initFrames();
            }
        });
        // 
        // ============================
        // Abos
        jMenuAnsicht.add(new JSeparator());
        jCheckBoxAboAnzeigen.setText("Abos anzeigen");
        jCheckBoxAboExtrafenster.setText("in Extrafenster");
        jCheckBoxAboExtrafenster.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jMenuAnsicht.add(jCheckBoxAboAnzeigen);
        jMenuAnsicht.add(jCheckBoxAboExtrafenster);
        jCheckBoxAboAnzeigen.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_ABO)));
        jCheckBoxAboAnzeigen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_VIS_ABO, Boolean.toString(jCheckBoxAboAnzeigen.isSelected()));
                initFrames();
            }
        });
        jCheckBoxAboExtrafenster.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_ABO)));
        jCheckBoxAboExtrafenster.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_FENSTER_ABO, Boolean.toString(jCheckBoxAboExtrafenster.isSelected()));
                initFrames();
            }
        });
        // 
        // ============================
        // Meldungen
        jMenuAnsicht.add(new JSeparator());
        jCheckBoxMeldungenAnzeigen.setText("Meldungen anzeigen");
        jCheckBoxMeldungenExtrafenster.setText("in Extrafenster");
        jCheckBoxMeldungenExtrafenster.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jCheckBoxMeldungenAnzeigen.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_MELDUNGEN)));
        jCheckBoxMeldungenAnzeigen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_VIS_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenAnzeigen.isSelected()));
                initFrames();
            }
        });
        jCheckBoxMeldungenExtrafenster.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_MELDUNGEN)));
        jCheckBoxMeldungenExtrafenster.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_FENSTER_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenExtrafenster.isSelected()));
                initFrames();
            }
        });
        jMenuAnsicht.add(jCheckBoxMeldungenAnzeigen);
        jMenuAnsicht.add(jCheckBoxMeldungenExtrafenster);
        // ===============================

        // Hilfe
        jMenuItemAnleitung.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                MVHelpDialog dialogOk = new MVHelpDialog(daten.mediathekGui, true, daten, "Hilfe zum Programm");
                dialogOk.setVisible(true);
            }
        });
        // Über
        jMenuItemAbout.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                showAboutDialog();
            }
        });
    }

    public void showDialogPreferences() {
        dialogEinstellungen.setVisible(true);
    }

    /**
     * Display the About Box
     */
    protected void showAboutDialog() {
        MVAboutDialog aboutDialog = new MVAboutDialog(this, SystemInfo.isMacOSX());
        aboutDialog.setVisible(true);
        aboutDialog.dispose();
    }

    public boolean beenden(boolean showOptionTerminate, boolean shutDown) {
        if (Daten.listeDownloads.nochNichtFertigeDownloads() > 0) {
            // erst mal prüfen ob noch Downloads laufen
            DialogBeenden dialogBeenden = new DialogBeenden(this);
            if (showOptionTerminate) {
                dialogBeenden.setComboWaitAndTerminate();
            }
            dialogBeenden.setModal(true);
            dialogBeenden.setVisible(true);
            if (!dialogBeenden.applicationCanTerminate()) {
                return false;
            }
            shutDown = dialogBeenden.isShutdownRequested();
        }
        // Tabelleneinstellungen merken
        daten.guiFilme.tabelleSpeichern();
        daten.guiDownloads.tabelleSpeichern();
        daten.guiAbo.tabelleSpeichern();
        if (Daten.listeDownloads != null) {
            // alle laufenden Downloads/Programme stoppen
            for (DatenDownload download : Daten.listeDownloads) {
                Start s = download.start;
                if (s != null) {
                    s.stoppen = true;
                }
            }
        }
        if (this.getExtendedState() == JFrame.MAXIMIZED_BOTH) {
            Daten.mVConfig.add(MVConfig.SYSTEM_FENSTER_MAX, Boolean.TRUE.toString());
        } else {
            Daten.mVConfig.add(MVConfig.SYSTEM_FENSTER_MAX, Boolean.FALSE.toString());
        }

        // Hauptfenster
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE, this);
        // Dialog Einstellungen
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_EINSTELLUNGEN, dialogEinstellungen);
        // Infodialog/Bandwidth
        if (mvDownloadInfo != null) {
            GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_INFODIALOG, mvDownloadInfo.getDialog());
            mvDownloadInfo.getDividerLocation();
        }
        Daten.mVConfig.add(MVConfig.SYSTEM_BREITE_MELDUNGEN, String.valueOf(splitPane.getDividerLocation()));

        // Frames
        if (frames[0] != null) {
            GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_DOWNLOAD, frames[0]);
        }
        if (frames[1] != null) {
            GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_ABO, frames[1]);
        }
        if (frames[2] != null) {
            GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_MELDUNGEN, frames[2]);
        }

        // FilterFrame
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_FILTER, daten.guiFilme.mVFilterFrame);
        daten.allesSpeichern();
        Log.printEndeMeldung();

        if (shutDown) {
            shutdownComputer();
        }

        dispose();
        System.exit(0);

        return false;
    }

    /**
     * Shutdown the computer depending on Operating System.
     */
    protected void shutdownComputer() {
        String strShutdownCommand = "";

        switch (Funktionen.getOs()) {
            case LINUX:
                strShutdownCommand = "shutdown -h now";
                break;

            case WIN32:
            case WIN64:
                strShutdownCommand = "shutdown.exe -s -t 0";
                break;

            default:
                Log.fehlerMeldung(465321789, "Shutdown unsupported operating system ...");
                break;
        }

        //only run if we have a proper shutdown command...
        if (!strShutdownCommand.isEmpty()) {
            try {
                Log.systemMeldung("Shutdown: " + strShutdownCommand);
                Runtime.getRuntime().exec(strShutdownCommand);
            } catch (IOException ex) {
                Log.fehlerMeldung(915263047, ex);
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jPanelInfo = new javax.swing.JPanel();
        jTabbedPane = new javax.swing.JTabbedPane();
        jPanelToolBar = new javax.swing.JPanel();
        jMenuBar = new javax.swing.JMenuBar();
        jMenuDatei = new javax.swing.JMenu();
        jMenuItemFilmlisteLaden = new javax.swing.JMenuItem();
        jMenuItemEinstellungen = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JPopupMenu.Separator();
        jMenuItemBeenden = new javax.swing.JMenuItem();
        javax.swing.JMenu jMenuFilme = new javax.swing.JMenu();
        jMenuItemFilmAbspielen = new javax.swing.JMenuItem();
        jMenuItemFilmAufzeichnen = new javax.swing.JMenuItem();
        jMenuItemFilterLoeschen = new javax.swing.JMenuItem();
        jMenuItemBlacklist = new javax.swing.JMenuItem();
        jMenuDownload = new javax.swing.JMenu();
        jMenuItemDownloadsAlleStarten = new javax.swing.JMenuItem();
        jMenuItemDownloadStartTime = new javax.swing.JMenuItem();
        jMenuItemDownloadWartendeStoppen = new javax.swing.JMenuItem();
        jMenuItemDownloadAlleStoppen = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator1 = new javax.swing.JPopupMenu.Separator();
        jMenuItemDownloadStarten = new javax.swing.JMenuItem();
        jMenuItemDownloadStoppen = new javax.swing.JMenuItem();
        jMenuItemDownloadVorziehen = new javax.swing.JMenuItem();
        jMenuItemDownloadsZurueckstellen = new javax.swing.JMenuItem();
        jMenuItemDownloadsLoeschen = new javax.swing.JMenuItem();
        jMenuItemDownloadAendern = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator3 = new javax.swing.JPopupMenu.Separator();
        jMenuItemDownloadAbspielen = new javax.swing.JMenuItem();
        jMenuItemDownloadsAktualisieren = new javax.swing.JMenuItem();
        jMenuItemDownloadsAufraeumen = new javax.swing.JMenuItem();
        jMenuItemDownloadShutDown = new javax.swing.JMenuItem();
        javax.swing.JMenu jMenuAbos = new javax.swing.JMenu();
        jMenuItemAbosEinschalten = new javax.swing.JMenuItem();
        jMenuItemAbosAusschalten = new javax.swing.JMenuItem();
        jMenuItemAbosLoeschen = new javax.swing.JMenuItem();
        jMenuItemAbosAendern = new javax.swing.JMenuItem();
        jMenuItemAboNeu = new javax.swing.JMenuItem();
        jMenuAnsicht = new javax.swing.JMenu();
        jCheckBoxMenuItemToolBar = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxMenuItemBeschreibung = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxMenuItemVideoplayer = new javax.swing.JCheckBoxMenuItem();
        javax.swing.JMenu jMenu1 = new javax.swing.JMenu();
        jMenuItemSchriftGr = new javax.swing.JMenuItem();
        jMenuItemSchriftKl = new javax.swing.JMenuItem();
        jMenuItemSchriftNormal = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator5 = new javax.swing.JPopupMenu.Separator();
        cbBandwidthDisplay = new javax.swing.JCheckBoxMenuItem();
        jMenuHilfe = new javax.swing.JMenu();
        jMenuItemAnleitung = new javax.swing.JMenuItem();
        jSeparator4 = new javax.swing.JPopupMenu.Separator();
        jMenuItemAbout = new javax.swing.JMenuItem();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);

        jPanel1.setLayout(new java.awt.BorderLayout());

        jPanelInfo.setLayout(new java.awt.BorderLayout());
        jPanel1.add(jPanelInfo, java.awt.BorderLayout.PAGE_END);

        jTabbedPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 1, 1, 1));
        jPanel1.add(jTabbedPane, java.awt.BorderLayout.CENTER);

        javax.swing.GroupLayout jPanelToolBarLayout = new javax.swing.GroupLayout(jPanelToolBar);
        jPanelToolBar.setLayout(jPanelToolBarLayout);
        jPanelToolBarLayout.setHorizontalGroup(
            jPanelToolBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelToolBarLayout.setVerticalGroup(
            jPanelToolBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );

        jMenuDatei.setMnemonic('d');
        jMenuDatei.setText("Datei");

        jMenuItemFilmlisteLaden.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F5, 0));
        jMenuItemFilmlisteLaden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/filmlisteLaden_16.png"))); // NOI18N
        jMenuItemFilmlisteLaden.setText("neue Filmliste laden");
        jMenuDatei.add(jMenuItemFilmlisteLaden);

        jMenuItemEinstellungen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4, 0));
        jMenuItemEinstellungen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/configure_16.png"))); // NOI18N
        jMenuItemEinstellungen.setText("Einstellungen");
        jMenuItemEinstellungen.setToolTipText("allgemeine Programmeinstellungen");
        jMenuDatei.add(jMenuItemEinstellungen);
        jMenuDatei.add(jSeparator2);

        jMenuItemBeenden.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Q, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemBeenden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/beenden_16.png"))); // NOI18N
        jMenuItemBeenden.setText("Beenden");
        jMenuDatei.add(jMenuItemBeenden);

        jMenuBar.add(jMenuDatei);

        jMenuFilme.setMnemonic('f');
        jMenuFilme.setText("Filme");

        jMenuItemFilmAbspielen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F6, 0));
        jMenuItemFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/film_start_16.png"))); // NOI18N
        jMenuItemFilmAbspielen.setText("Film abspielen");
        jMenuFilme.add(jMenuItemFilmAbspielen);

        jMenuItemFilmAufzeichnen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F7, 0));
        jMenuItemFilmAufzeichnen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/film_rec_16.png"))); // NOI18N
        jMenuItemFilmAufzeichnen.setText("Film aufzeichnen");
        jMenuFilme.add(jMenuItemFilmAufzeichnen);

        jMenuItemFilterLoeschen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F8, 0));
        jMenuItemFilterLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/clear_16.png"))); // NOI18N
        jMenuItemFilterLoeschen.setText("Filter löschen");
        jMenuFilme.add(jMenuItemFilterLoeschen);

        jMenuItemBlacklist.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F9, 0));
        jMenuItemBlacklist.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/blacklist_16.png"))); // NOI18N
        jMenuItemBlacklist.setText("Blacklist öffnen");
        jMenuFilme.add(jMenuItemBlacklist);

        jMenuBar.add(jMenuFilme);

        jMenuDownload.setMnemonic('w');
        jMenuDownload.setText("Downloads");

        jMenuItemDownloadsAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_alleStarten_16.png"))); // NOI18N
        jMenuItemDownloadsAlleStarten.setText("alle Downloads starten");
        jMenuDownload.add(jMenuItemDownloadsAlleStarten);

        jMenuItemDownloadStartTime.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_alleStarten_16.png"))); // NOI18N
        jMenuItemDownloadStartTime.setText("alle Downloads um xx:yy Uhr starten");
        jMenuDownload.add(jMenuItemDownloadStartTime);

        jMenuItemDownloadWartendeStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_stop_16.png"))); // NOI18N
        jMenuItemDownloadWartendeStoppen.setText("wartende stoppen");
        jMenuItemDownloadWartendeStoppen.setToolTipText("wartende Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadWartendeStoppen);

        jMenuItemDownloadAlleStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_stop_16.png"))); // NOI18N
        jMenuItemDownloadAlleStoppen.setText("alle stoppen");
        jMenuItemDownloadAlleStoppen.setToolTipText("alle Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadAlleStoppen);
        jMenuDownload.add(jSeparator1);

        jMenuItemDownloadStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_start_16.png"))); // NOI18N
        jMenuItemDownloadStarten.setText("Downloads starten");
        jMenuDownload.add(jMenuItemDownloadStarten);

        jMenuItemDownloadStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_stop_16.png"))); // NOI18N
        jMenuItemDownloadStoppen.setText("Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadStoppen);

        jMenuItemDownloadVorziehen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/move_up_16.png"))); // NOI18N
        jMenuItemDownloadVorziehen.setText("Downloads vorziehen");
        jMenuDownload.add(jMenuItemDownloadVorziehen);

        jMenuItemDownloadsZurueckstellen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_BACK_SPACE, 0));
        jMenuItemDownloadsZurueckstellen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/undo_16.png"))); // NOI18N
        jMenuItemDownloadsZurueckstellen.setText("Downloads zurückstellen");
        jMenuDownload.add(jMenuItemDownloadsZurueckstellen);

        jMenuItemDownloadsLoeschen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_DELETE, 0));
        jMenuItemDownloadsLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_del_16.png"))); // NOI18N
        jMenuItemDownloadsLoeschen.setText("Download aus Liste entfernen");
        jMenuDownload.add(jMenuItemDownloadsLoeschen);

        jMenuItemDownloadAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/configure_16.png"))); // NOI18N
        jMenuItemDownloadAendern.setText("Download ändern");
        jMenuDownload.add(jMenuItemDownloadAendern);
        jMenuDownload.add(jSeparator3);

        jMenuItemDownloadAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/film_start_16.png"))); // NOI18N
        jMenuItemDownloadAbspielen.setText("gespeicherten Film abspielen");
        jMenuDownload.add(jMenuItemDownloadAbspielen);

        jMenuItemDownloadsAktualisieren.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_SPACE, 0));
        jMenuItemDownloadsAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/view-refresh_16.png"))); // NOI18N
        jMenuItemDownloadsAktualisieren.setText("Downloads aktualisieren");
        jMenuDownload.add(jMenuItemDownloadsAktualisieren);

        jMenuItemDownloadsAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_clear_16.png"))); // NOI18N
        jMenuItemDownloadsAufraeumen.setText("Downloads aufräumen");
        jMenuDownload.add(jMenuItemDownloadsAufraeumen);

        jMenuItemDownloadShutDown.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/beenden_16.png"))); // NOI18N
        jMenuItemDownloadShutDown.setText("Rechner nach Downloads herunterfahren");
        jMenuDownload.add(jMenuItemDownloadShutDown);

        jMenuBar.add(jMenuDownload);

        jMenuAbos.setMnemonic('b');
        jMenuAbos.setText("Abos");

        jMenuItemAbosEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/ja_16.png"))); // NOI18N
        jMenuItemAbosEinschalten.setText("einschalten");
        jMenuAbos.add(jMenuItemAbosEinschalten);

        jMenuItemAbosAusschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/nein_16.png"))); // NOI18N
        jMenuItemAbosAusschalten.setText("ausschalten");
        jMenuAbos.add(jMenuItemAbosAusschalten);

        jMenuItemAbosLoeschen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_DELETE, 0));
        jMenuItemAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/del_16.png"))); // NOI18N
        jMenuItemAbosLoeschen.setText("löschen");
        jMenuAbos.add(jMenuItemAbosLoeschen);

        jMenuItemAbosAendern.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, 0));
        jMenuItemAbosAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/configure_16.png"))); // NOI18N
        jMenuItemAbosAendern.setText("ändern");
        jMenuAbos.add(jMenuItemAbosAendern);

        jMenuItemAboNeu.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/add_16.png"))); // NOI18N
        jMenuItemAboNeu.setText("neues Abo anlegen");
        jMenuAbos.add(jMenuItemAboNeu);

        jMenuBar.add(jMenuAbos);

        jMenuAnsicht.setMnemonic('a');
        jMenuAnsicht.setText("Ansicht");

        jCheckBoxMenuItemToolBar.setSelected(true);
        jCheckBoxMenuItemToolBar.setText("Toolbar");
        jMenuAnsicht.add(jCheckBoxMenuItemToolBar);

        jCheckBoxMenuItemBeschreibung.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F10, 0));
        jCheckBoxMenuItemBeschreibung.setText("Beschreibung anzeigen");
        jMenuAnsicht.add(jCheckBoxMenuItemBeschreibung);

        jCheckBoxMenuItemVideoplayer.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F11, 0));
        jCheckBoxMenuItemVideoplayer.setText("Buttons anzeigen");
        jMenuAnsicht.add(jCheckBoxMenuItemVideoplayer);

        jMenu1.setText("Schriftgröße");

        jMenuItemSchriftGr.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ADD, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemSchriftGr.setText("vergrößern");
        jMenu1.add(jMenuItemSchriftGr);

        jMenuItemSchriftKl.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_SUBTRACT, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemSchriftKl.setText("verkleinern");
        jMenu1.add(jMenuItemSchriftKl);

        jMenuItemSchriftNormal.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_0, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemSchriftNormal.setText("Normalgröße");
        jMenu1.add(jMenuItemSchriftNormal);

        jMenuAnsicht.add(jMenu1);
        jMenuAnsicht.add(jSeparator5);

        cbBandwidthDisplay.setText("Bandbreitennutzung");
        jMenuAnsicht.add(cbBandwidthDisplay);

        jMenuBar.add(jMenuAnsicht);

        jMenuHilfe.setMnemonic('h');
        jMenuHilfe.setText("Hilfe");

        jMenuItemAnleitung.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/help_16.png"))); // NOI18N
        jMenuItemAnleitung.setText("Hilfe und Fragen zum Programm");
        jMenuHilfe.add(jMenuItemAnleitung);
        jMenuHilfe.add(jSeparator4);

        jMenuItemAbout.setText("Über MediathekView");
        jMenuHilfe.add(jMenuItemAbout);

        jMenuBar.add(jMenuHilfe);

        setJMenuBar(jMenuBar);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 1083, Short.MAX_VALUE)
            .addComponent(jPanelToolBar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addComponent(jPanelToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 805, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBoxMenuItem cbBandwidthDisplay;
    protected javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemBeschreibung;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemToolBar;
    protected javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemVideoplayer;
    private javax.swing.JMenu jMenuAnsicht;
    private javax.swing.JMenuBar jMenuBar;
    protected javax.swing.JMenu jMenuDatei;
    private javax.swing.JMenu jMenuDownload;
    protected javax.swing.JMenu jMenuHilfe;
    private javax.swing.JMenuItem jMenuItemAboNeu;
    private javax.swing.JMenuItem jMenuItemAbosAendern;
    private javax.swing.JMenuItem jMenuItemAbosAusschalten;
    private javax.swing.JMenuItem jMenuItemAbosEinschalten;
    private javax.swing.JMenuItem jMenuItemAbosLoeschen;
    protected javax.swing.JMenuItem jMenuItemAbout;
    private javax.swing.JMenuItem jMenuItemAnleitung;
    protected javax.swing.JMenuItem jMenuItemBeenden;
    protected javax.swing.JMenuItem jMenuItemBlacklist;
    private javax.swing.JMenuItem jMenuItemDownloadAbspielen;
    private javax.swing.JMenuItem jMenuItemDownloadAendern;
    private javax.swing.JMenuItem jMenuItemDownloadAlleStoppen;
    private javax.swing.JMenuItem jMenuItemDownloadShutDown;
    private javax.swing.JMenuItem jMenuItemDownloadStartTime;
    private javax.swing.JMenuItem jMenuItemDownloadStarten;
    private javax.swing.JMenuItem jMenuItemDownloadStoppen;
    private javax.swing.JMenuItem jMenuItemDownloadVorziehen;
    private javax.swing.JMenuItem jMenuItemDownloadWartendeStoppen;
    private javax.swing.JMenuItem jMenuItemDownloadsAktualisieren;
    private javax.swing.JMenuItem jMenuItemDownloadsAlleStarten;
    private javax.swing.JMenuItem jMenuItemDownloadsAufraeumen;
    private javax.swing.JMenuItem jMenuItemDownloadsLoeschen;
    private javax.swing.JMenuItem jMenuItemDownloadsZurueckstellen;
    protected javax.swing.JMenuItem jMenuItemEinstellungen;
    protected javax.swing.JMenuItem jMenuItemFilmAbspielen;
    protected javax.swing.JMenuItem jMenuItemFilmAufzeichnen;
    private javax.swing.JMenuItem jMenuItemFilmlisteLaden;
    protected javax.swing.JMenuItem jMenuItemFilterLoeschen;
    private javax.swing.JMenuItem jMenuItemSchriftGr;
    private javax.swing.JMenuItem jMenuItemSchriftKl;
    private javax.swing.JMenuItem jMenuItemSchriftNormal;
    private javax.swing.JPanel jPanelInfo;
    private javax.swing.JPanel jPanelToolBar;
    protected javax.swing.JPopupMenu.Separator jSeparator2;
    protected javax.swing.JPopupMenu.Separator jSeparator4;
    private javax.swing.JTabbedPane jTabbedPane;
    // End of variables declaration//GEN-END:variables

}
