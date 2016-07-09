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
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.basic.BasicTabbedPaneUI;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Functions.OperatingSystemType;
import static mSearch.tool.Functions.getOs;
import mSearch.tool.*;
import mediathek.controller.CheckUpdate;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.gui.*;
import mediathek.gui.dialog.*;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.dialogEinstellungen.Einstellungen;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.gui.dialogEinstellungen.PanelMeldungen;
import mediathek.res.GetIcon;
import static mediathek.tool.MVFunctionSys.startMeldungen;
import mediathek.tool.*;

public class MediathekGui extends JFrame {

    private Daten daten;
    private final SpacerIcon spacerIcon = new SpacerIcon(30);
    protected final DialogEinstellungen dialogEinstellungen;
    private final JSpinner jSpinnerAnzahl = new JSpinner(new SpinnerNumberModel(1, 1, 9, 1));
    private final JLabel jLabelAnzahl = new JLabel("Anzahl gleichzeitige Downloads");
    private final JPanel jPanelAnzahl = new JPanel();
    private final JLabel jLabelBandbreite = new JLabel("Bandbreite pro Download");
    private final JPanel jPanelBandbreite = new JPanel();
    private final JSlider jSliderBandbreite = new JSlider();
    private PanelVorlage panelMeldungen = new PanelVorlage(daten, this);
    private JSplitPane splitPane = null;
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
        //return mVToolBar.jTextFieldFilter.getText();
        return daten.guiFilme.getFilterTextFromSearchField();
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
     * wegeb der möglichen Abfrage: "Backup laden.."
     */
    public void closeSplashScreen() {
        splashScreenContext = null;
    }

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

    public MediathekGui(String[] ar) {
        super();
        initializeSplashScreen();

        String pfad = "";
        initComponents();
        if (ar != null) {
            SysMsg.sysMsg("");
            SysMsg.sysMsg("==========================================");
            for (String s : ar) {
                SysMsg.sysMsg("Startparameter: " + s);
            }
            SysMsg.sysMsg("==========================================");
            SysMsg.sysMsg("");
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

        startMeldungen();
        createStatusBar();
//        mVToolBar = new MVToolBar(daten);
//        jPanelToolBar.setLayout(new BorderLayout());
//        jPanelToolBar.add(mVToolBar, BorderLayout.CENTER);

        //create the Film Information HUD
        if (SystemInfo.isMacOSX()) {
            daten.filmInfo = new MVFilmInformation(this, jTabbedPane, daten);
        } else {
            //klappte nicht auf allen Desktops
            daten.filmInfo = new MVFilmInformationLinux(this, jTabbedPane, daten);
        }
        duration.ping("HUD");

        if (daten.allesLaden()) {
            // alles geladen
            updateSplashScreenText("GUI Initialisieren...");
        } else {
            // erster Start
            ReplaceList.init(); // einmal ein Muster anlegen, für Linux/OS X ist es bereits aktiv!
            new DialogStarteinstellungen(this, daten).setVisible(true);
            this.pack();
        }
        duration.ping("Alles laden");

        setOrgTitel();
        setLookAndFeel();
        duration.ping("LookAndFeel");
        init();
        duration.ping("init");
        setSize();
        duration.ping("setSize");

        // Dialog mit den Programmeinstellungen einrichten
        dialogEinstellungen = new DialogEinstellungen(this, daten);

        daten.dialogMediaDB = new DialogMediaDB(this);
        daten.dialogMediaDB.setVis();

        // Prüfen obs ein Programmupdate gibt
        new CheckUpdate(this, daten).checkProgUpdate();
        duration.ping("CheckUpdate");

        if (GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUTO) {
            if (Daten.listeFilme.isTooOld()) {
                SysMsg.sysMsg("Neue Filmliste laden");
                Daten.filmeLaden.importFilmliste("", true);
            }
        }
        duration.ping("Filmliste laden");

        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIATHEKGUI_ORG_TITEL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setOrgTitel();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIATHEKGUI_PROGRAMM_AKTUELL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitelAllesAktuell();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitelUpdate();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxMenuItemBeschreibung.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN)));
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_DIALOG_MEDIA_DB, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxMenuItemMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxFilterAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_FILTER)));
                jCheckBoxFilterExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_FILTER)));
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_TABS_LEFT, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTabColor();
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

        cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BANDWIDTH_MONITOR_VISIBLE)));
        if (SystemInfo.isMacOSX()) {
            bandwidthMonitor = new MVBandwidthMonitor(this, cbBandwidthDisplay);
            bandwidthMonitor.toggleVisibility();
            cbBandwidthDisplay.addActionListener(e -> bandwidthMonitor.toggleVisibility());
        } else {
            mvDownloadInfo = new MVDownloadInfo(this, cbBandwidthDisplay);
            mvDownloadInfo.toggleVisibility();
            cbBandwidthDisplay.addActionListener(e -> mvDownloadInfo.toggleVisibility());

        }
        duration.ping("Gui steht!");

    }

    private void setFocusSuchfeld() {
        Listener.notify(Listener.EREIGNIS_SUCHFELD_FOCUS_SETZEN, MediathekGui.class.getName());
        if (!Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_FILTER))) {
//            mVToolBar.jTextFieldFilter.requestFocus();
//            mVToolBar.jTextFieldFilter.setCaretPosition(0);
        }
    }

    /**
     * This will set the Look&Feel based on Application Preferences. In case of
     * error it will always reset to system LAF.
     */
    private void setLookAndFeel() {
        try {
            String laf = MVConfig.get(MVConfig.SYSTEM_LOOK);
            //if we have the old values, reset to System LAF
            if (laf.equals("") || laf.length() == 1) {
                if (getOs() != OperatingSystemType.LINUX) {
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                }
            } else {
                //otherwise set the requested UI
                laf = MVConfig.get(MVConfig.SYSTEM_LOOK);
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
            MVConfig.add(MVConfig.SYSTEM_LOOK, UIManager.getSystemLookAndFeelClassName());
        }
    }

    //===================================
    // public
    //===================================
    public void setToolbar(String state) {
//        mVToolBar.setToolbar(state);
        jMenuFilme.setEnabled(false);
        jMenuDownload.setEnabled(false);
        jMenuAbos.setEnabled(false);
        switch (state) {
            case "":
                break;
            case MVToolBar.TOOLBAR_TAB_FILME:
                jMenuFilme.setEnabled(true);
                break;
            case MVToolBar.TOOLBAR_TAB_DOWNLOADS:
                jMenuDownload.setEnabled(true);
                break;
            case MVToolBar.TOOLBAR_TAB_ABOS:
                jMenuAbos.setEnabled(true);
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

    private void setSize() {
        if (Daten.startMaximized || Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_MAX))) {
            this.setExtendedState(Frame.MAXIMIZED_BOTH);
        } else {
            GuiFunktionen.setSize(MVConfig.SYSTEM_GROESSE_GUI, this, null);
        }
        try {
            int divider = Integer.parseInt(MVConfig.get(MVConfig.SYSTEM_BREITE_MELDUNGEN));
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
        jMenuItemFilmeGesehen.setIcon(GetIcon.getProgramIcon("history_add_16.png"));
        jMenuItemFilmeUngesehen.setIcon(GetIcon.getProgramIcon("history_remove_16.png"));
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
        jMenuItemDownloadGesehen.setIcon(GetIcon.getProgramIcon("history_add_16.png"));
        jMenuItemDownloadUngesehen.setIcon(GetIcon.getProgramIcon("history_remove_16.png"));
        jMenuItemAbosEinschalten.setIcon(GetIcon.getProgramIcon("ja_16.png"));
        jMenuItemAbosAusschalten.setIcon(GetIcon.getProgramIcon("nein_16.png"));
        jMenuItemAbosLoeschen.setIcon(GetIcon.getProgramIcon("del_16.png"));
        jMenuItemAbosAendern.setIcon(GetIcon.getProgramIcon("configure_16.png"));
        jMenuItemAboNeu.setIcon(GetIcon.getProgramIcon("add_16.png"));
        jMenuItemAnleitung.setIcon(GetIcon.getProgramIcon("help_16.png"));
        initTabs();
        initMenue();
//        mVToolBar.loadVisible(); // erst jetzt sind die Einstellungen geladen!
        Daten.filmeLaden.addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                jMenuItemFilmlisteLaden.setEnabled(false);
            }

            @Override
            public void progress(ListenerFilmeLadenEvent event) {
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                jMenuItemFilmlisteLaden.setEnabled(true);
                daten.allesSpeichern(); // damit nichts verlorengeht
            }
        });
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                if (tray != null && !SystemInfo.isMacOSX() && Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_USE_TRAY))) {
                    daten.mediathekGui.setVisible(false);
                } else {
                    beenden(false, false);
                }
            }
        });
        setTray();
    }

    public void setTray() {
        if (tray == null && Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_USE_TRAY))) {
            tray = new MVTray(daten).systemTray();
        } else if (tray != null && !Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_USE_TRAY))) {
            tray.beenden();
            tray = null;
        }
    }

    public void hideFrame(String state) {
        switch (state) {
            case MVToolBar.TOOLBAR_TAB_DOWNLOADS:
                jCheckBoxDownloadAnzeigen.setSelected(false);
                MVConfig.add(MVConfig.SYSTEM_VIS_DOWNLOAD, Boolean.toString(false));
                break;
            case MVToolBar.TOOLBAR_TAB_ABOS:
                jCheckBoxAboAnzeigen.setSelected(false);
                MVConfig.add(MVConfig.SYSTEM_VIS_ABO, Boolean.toString(false));
                break;
            case MVToolBar.TOOLBAR_TAB_MELDUNGEN:
                jCheckBoxMeldungenAnzeigen.setSelected(false);
                MVConfig.add(MVConfig.SYSTEM_VIS_MELDUNGEN, Boolean.toString(false));
                break;
        }
        initFrames();
    }

    private void initFrames() {
        // Downloads
        int nr = 1;
        if (!Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_DOWNLOAD))) {
            hide(0, daten.guiDownloads);
        } else if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_DOWNLOAD))) {
            setFrame(0, MVConfig.SYSTEM_GROESSE_DOWNLOAD, daten.guiDownloads, MVToolBar.TOOLBAR_TAB_DOWNLOADS, "Downloads");
        } else {
            setTab(0, daten.guiDownloads, "Downloads", nr++);
        }
        // Abos
        if (!Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_ABO))) {
            hide(1, daten.guiAbo);
        } else if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_ABO))) {
            setFrame(1, MVConfig.SYSTEM_GROESSE_ABO, daten.guiAbo, MVToolBar.TOOLBAR_TAB_ABOS, "Abos");
        } else {
            setTab(1, daten.guiAbo, "Abos", nr++);
        }
        // Meldungen
        if (!Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_MELDUNGEN))) {
            hide(2, panelMeldungen);
        } else if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_MELDUNGEN))) {
            setFrame(2, MVConfig.SYSTEM_GROESSE_MELDUNGEN, panelMeldungen, MVToolBar.TOOLBAR_TAB_MELDUNGEN, "Meldungen");
        } else {
            setTab(2, panelMeldungen, "Meldungen", nr);
        }
//        mVToolBar.loadVisible(); // die können sich im externen Fenster geändert haben
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
            jTabbedPane.add(panelVorlage, spacerIcon, nrTab);
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

    private void setTabColor() {
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_TABS_LEFT))) {
            jTabbedPane.setTabPlacement(JTabbedPane.LEFT);
        } else {
            jTabbedPane.setTabPlacement(JTabbedPane.TOP);
        }

        for (int i = 0; i < jTabbedPane.getTabCount(); ++i) {
            String s = jTabbedPane.getTitleAt(i);
            JLabel lbl = makeLable(s);
            jTabbedPane.setTabComponentAt(i, lbl);
//            jTabbedPane.setIconAt(i, GetIcon.getGuiIcon("tab-button.png"));
        }

        jTabbedPane.setUI(new BasicTabbedPaneUI() {
            @Override
            protected void installDefaults() {
                super.installDefaults();
//                highlight = null;
//                lightHighlight = null;
//                shadow = null;
//                darkShadow = null;
//                focus = null;
                tabInsets = new Insets(9, 9, 9, 9);

            }
        });
        UIManager.getDefaults().put("TabbedPane.tabsOverlapBorder", false);
        UIManager.getDefaults().put("TabbedPane.tabsOpaque", true);
        UIManager.put("TabbedPane.selected", null);
    }

    private JLabel makeLable(String text) {
        JLabel lbl = new JLabel(text);
        ImageIcon ic = GetIcon.getGuiIcon("tab-button.png");
        ic.setImage(ic.getImage().getScaledInstance(150, 40, Image.SCALE_DEFAULT));

        lbl.setBorder(null);
        lbl.setIcon(ic);
        lbl.setOpaque(false);
        lbl.setHorizontalTextPosition(SwingConstants.CENTER);
        return lbl;
    }

    private void initTabs() {
        daten.guiDownloads = new GuiDownloads(daten, daten.mediathekGui);
        daten.guiAbo = new GuiAbo(daten, daten.mediathekGui);

        daten.guiFilme = new GuiFilme(daten, daten.mediathekGui);
        daten.guiFilme.init();
        setTabColor();
        jTabbedPane.addTab("Filme", spacerIcon, daten.guiFilme);

        // jetzt noch den Rest
        panelMeldungen = new PanelVorlage(daten, this) {

            @Override
            public void isShown() {
                if (!solo) {
                    setToolbar(MVToolBar.TOOLBAR_NIX);
                    statusBar.setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.NONE);
                }
            }
        };
        PanelMeldungen panelMeldungenSystem = new PanelMeldungen(daten, daten.mediathekGui, SysMsg.LOG_SYSTEM, "Systemmeldungen");
        PanelMeldungen panelMeldungenPlayer = new PanelMeldungen(daten, daten.mediathekGui, SysMsg.LOG_PLAYER, "Meldungen Hilfsprogramme");
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                panelMeldungenSystem,
                panelMeldungenPlayer);

        panelMeldungen.setLayout(new BorderLayout());
        panelMeldungen.add(splitPane, BorderLayout.CENTER);
        if (Daten.debug) {
            Daten.guiDebug = new GuiDebug(daten);
            jTabbedPane.addTab("Debug", spacerIcon, Daten.guiDebug);
        }
        jTabbedPane.addTab("Einstellungen", spacerIcon, new Einstellungen(this, daten));
        initFrames();
    }

    protected void initSpinner() {
        if (MVConfig.get(MVConfig.SYSTEM_MAX_DOWNLOAD).equals("")) {
            jSpinnerAnzahl.setValue(1);
            MVConfig.add(MVConfig.SYSTEM_MAX_DOWNLOAD, "1");
        } else {
            jSpinnerAnzahl.setValue(Integer.parseInt(MVConfig.get(MVConfig.SYSTEM_MAX_DOWNLOAD)));
        }
    }

    private void setSlider() {
        MVDownloadInfo.setSliderBandwith(jSliderBandbreite, null);
        setSliderText();
    }

    private void setSliderText() {
        String s = MVDownloadInfo.getTextBandwith();
        s = " [" + s + "]: ";
        while (s.length() < 20) {
            s = s + " ";
        }
        jLabelBandbreite.setText("Bandbreite pro Download" + s);
    }

    protected void setupBandwidthMenuItem() {
        // Bandbreite pro Downloads
        jPanelBandbreite.setLayout(new BorderLayout());
        jPanelBandbreite.setBorder(new EmptyBorder(3, 5, 3, 5));
        jPanelBandbreite.add(jLabelBandbreite, BorderLayout.WEST);
        jPanelBandbreite.add(jSliderBandbreite, BorderLayout.EAST);
        jLabelBandbreite.setIcon(GetIcon.getProgramIcon("bandwith_16.png"));
        jSliderBandbreite.setMinimum(5); //50 kByte/s
        jSliderBandbreite.setMaximum(100); //1.000 kByte/s
        setSlider();
        jSliderBandbreite.addChangeListener(e -> {
            int bandbreiteKByte = jSliderBandbreite.getValue() * 10;
            MVConfig.add(MVConfig.SYSTEM_BANDBREITE_KBYTE, String.valueOf(bandbreiteKByte));
            setSliderText();
            Listener.notify(Listener.EREIGNIS_BANDBREITE, MediathekGui.class.getSimpleName());
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_BANDBREITE, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setSlider();
            }
        });
        jMenuDownload.add(jPanelBandbreite);
    }

    protected void setupMaximumNumberOfDownloadsMenuItem() {
        // Anzahl gleichzeitiger Downloads
        initSpinner();

        jMenuDownload.add(new javax.swing.JPopupMenu.Separator());
        jPanelAnzahl.setLayout(new BorderLayout());
        jPanelAnzahl.setBorder(new EmptyBorder(3, 5, 3, 5));
        jPanelAnzahl.add(jLabelAnzahl, BorderLayout.WEST);
        jPanelAnzahl.add(jSpinnerAnzahl, BorderLayout.EAST);
        jLabelAnzahl.setIcon(GetIcon.getProgramIcon("up_down_16.png"));
        jSpinnerAnzahl.addChangeListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_MAX_DOWNLOAD,
                    String.valueOf(((Number) jSpinnerAnzahl.getModel().getValue()).intValue()));
            Listener.notify(Listener.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName());
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                initSpinner();
            }
        });
        jMenuDownload.add(jPanelAnzahl);
    }

    protected void initMenue() {
        setupMaximumNumberOfDownloadsMenuItem();
        setupBandwidthMenuItem();

        // Datei
////        jMenuItemEinstellungen.addActionListener(e -> dialogEinstellungen.setVisible(true));
        jMenuItemEinstellungen.setVisible(false);

        jMenuItemBeenden.addActionListener(e -> beenden(false, false));
        // Filme
        jMenuItemFilmlisteLaden.addActionListener(e -> Daten.filmeLaden.filmeLaden(daten, false));
        jMenuItemFilmAbspielen.addActionListener(e -> daten.guiFilme.guiFilmeFilmAbspielen());
        jMenuItemFilmAufzeichnen.addActionListener(e -> daten.guiFilme.guiFilmeFilmSpeichern());
        jMenuItemFilterLoeschen.addActionListener(e -> daten.guiFilme.guiFilmeFilterLoeschen());
        jMenuItemBlacklist.addActionListener(e -> {
            DialogLeer dialog = new DialogLeer(daten.mediathekGui, true);
            dialog.init("Blacklist", new PanelBlacklist(daten, daten.mediathekGui, PanelBlacklist.class.getName() + "_2"));
            dialog.setVisible(true);
        });
        jMenuItemFilmeGesehen.addActionListener(e -> daten.guiFilme.filmGesehen());
        jMenuItemFilmeUngesehen.addActionListener(e -> daten.guiFilme.filmUngesehen());
        jMenuItemFilmeMediensammlung.addActionListener(e -> daten.guiFilme.guiFilmMediensammlung());
        // Downloads
        jMenuItemDownloadsAktualisieren.addActionListener(e -> daten.guiDownloads.aktualisieren());
        jMenuItemDownloadAbspielen.addActionListener(e -> daten.guiDownloads.filmAbspielen());
        jMenuItemDownloadsAufraeumen.addActionListener(e -> daten.guiDownloads.aufraeumen());
        jMenuItemDownloadsLoeschen.addActionListener(e -> daten.guiDownloads.loeschen());
        jMenuItemDownloadsAlleStarten.addActionListener(e -> daten.guiDownloads.starten(true /* alle */));
        jMenuItemDownloadStartTime.addActionListener(e -> daten.guiDownloads.startAtTime());
        jMenuItemDownloadStarten.addActionListener(e -> daten.guiDownloads.starten(false /* alle */));
        jMenuItemDownloadsZurueckstellen.addActionListener(e -> daten.guiDownloads.zurueckstellen());
        jMenuItemDownloadVorziehen.addActionListener(e -> daten.guiDownloads.vorziehen());
        jMenuItemDownloadAendern.addActionListener(e -> daten.guiDownloads.aendern());
        jMenuItemDownloadAlleStoppen.addActionListener(e -> daten.guiDownloads.stoppen(true /* alle */));
        jMenuItemDownloadWartendeStoppen.addActionListener(e -> daten.guiDownloads.wartendeStoppen());
        jMenuItemDownloadStoppen.addActionListener(e -> daten.guiDownloads.stoppen(false /* alle */));
        jMenuItemDownloadShutDown.addActionListener(e -> {
            if (Daten.listeDownloads.nochNichtFertigeDownloads() > 0) {
                // ansonsten gibts keine laufenden Downloads auf die man warten sollte
                beenden(true /*Dialog auf "warten" einstellen*/, false /*shutdown computer*/);
            } else {
                MVMessageDialog.showMessageDialog(daten.mediathekGui, "Die Downloads müssen zuerst gestartet werden.",
                        "Keine laufenden Downloads!", JOptionPane.ERROR_MESSAGE);
            }
        });
        jMenuItemDownloadGesehen.addActionListener(e -> daten.guiDownloads.filmGesehen());
        jMenuItemDownloadUngesehen.addActionListener(e -> daten.guiDownloads.filmUngesehen());
        jMenuItemDownloadMediensammlung.addActionListener(e -> daten.guiDownloads.guiFilmMediensammlung());

        // Abo
        jMenuItemAbosEinschalten.addActionListener(e -> daten.guiAbo.einAus(true));
        jMenuItemAbosAusschalten.addActionListener(e -> daten.guiAbo.einAus(false));
        jMenuItemAbosLoeschen.addActionListener(e -> daten.guiAbo.loeschen());
        jMenuItemAbosAendern.addActionListener(e -> daten.guiAbo.aendern());
        jMenuItemAboNeu.addActionListener(e -> daten.guiAbo.neu());

        // Ansicht
        jCheckBoxMenuItemToolBar.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_TOOLBAR_ALLES_ANZEIGEN)));
//        mVToolBar.setVisible(jCheckBoxMenuItemToolBar.isSelected());
        jCheckBoxMenuItemToolBar.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_TOOLBAR_ALLES_ANZEIGEN, Boolean.toString(jCheckBoxMenuItemToolBar.isSelected()));
 //            mVToolBar.setVisible(jCheckBoxMenuItemToolBar.isSelected());
            Listener.notify(Listener.EREIGNIS_TOOLBAR_VIS, MediathekGui.class.getSimpleName());
        });
        jCheckBoxMenuItemVideoplayer.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
        jCheckBoxMenuItemVideoplayer.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, String.valueOf(jCheckBoxMenuItemVideoplayer.isSelected()));
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName());
        });
        jCheckBoxMenuItemBeschreibung.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN)));
        jCheckBoxMenuItemBeschreibung.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN, String.valueOf(jCheckBoxMenuItemBeschreibung.isSelected()));
            Listener.notify(Listener.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName());
        });
        jCheckBoxMenuItemMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
        jCheckBoxMenuItemMediaDb.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, String.valueOf(jCheckBoxMenuItemMediaDb.isSelected()));
            daten.dialogMediaDB.setVis();
        });
        jMenuItemSchriftGr.addActionListener(e -> MVFont.setFontSize(true));
        jMenuItemSchriftKl.addActionListener(e -> MVFont.setFontSize(false));
        jMenuItemSchriftNormal.addActionListener(e -> MVFont.resetFontSize());
        // ============================
        // Filter
        jMenuAnsicht.add(new JSeparator());
        jCheckBoxFilterAnzeigen.setText("Filter anzeigen");
        jCheckBoxFilterExtrafenster.setText("in Extrafenster");
        jCheckBoxFilterExtrafenster.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jMenuAnsicht.add(jCheckBoxFilterAnzeigen);
        jMenuAnsicht.add(jCheckBoxFilterExtrafenster);
        jCheckBoxFilterAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_FILTER)));
        jCheckBoxFilterAnzeigen.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_VIS_FILTER, Boolean.toString(jCheckBoxFilterAnzeigen.isSelected()));
            Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getSimpleName());
        });
        jCheckBoxFilterExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_FILTER)));
        jCheckBoxFilterExtrafenster.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_FENSTER_FILTER, Boolean.toString(jCheckBoxFilterExtrafenster.isSelected()));
            Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getSimpleName());
        });
        // ============================
        // Downloads
        jMenuAnsicht.add(new JSeparator());
        jCheckBoxDownloadAnzeigen.setText("Downloads anzeigen");
        jCheckBoxDownloadExtrafenster.setText("in Extrafenster");
        jCheckBoxDownloadExtrafenster.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jMenuAnsicht.add(jCheckBoxDownloadAnzeigen);
        jMenuAnsicht.add(jCheckBoxDownloadExtrafenster);
        jCheckBoxDownloadAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_DOWNLOAD)));
        jCheckBoxDownloadAnzeigen.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_VIS_DOWNLOAD, Boolean.toString(jCheckBoxDownloadAnzeigen.isSelected()));
            initFrames();
        });
        jCheckBoxDownloadExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_DOWNLOAD)));
        jCheckBoxDownloadExtrafenster.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_FENSTER_DOWNLOAD, Boolean.toString(jCheckBoxDownloadExtrafenster.isSelected()));
            initFrames();
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
        jCheckBoxAboAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_ABO)));
        jCheckBoxAboAnzeigen.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_VIS_ABO, Boolean.toString(jCheckBoxAboAnzeigen.isSelected()));
            initFrames();
        });
        jCheckBoxAboExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_ABO)));
        jCheckBoxAboExtrafenster.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_FENSTER_ABO, Boolean.toString(jCheckBoxAboExtrafenster.isSelected()));
            initFrames();
        });
        // 
        // ============================
        // Meldungen
        jMenuAnsicht.add(new JSeparator());
        jCheckBoxMeldungenAnzeigen.setText("Meldungen anzeigen");
        jCheckBoxMeldungenExtrafenster.setText("in Extrafenster");
        jCheckBoxMeldungenExtrafenster.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jCheckBoxMeldungenAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_MELDUNGEN)));
        jCheckBoxMeldungenAnzeigen.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_VIS_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenAnzeigen.isSelected()));
            initFrames();
        });
        jCheckBoxMeldungenExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_MELDUNGEN)));
        jCheckBoxMeldungenExtrafenster.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_FENSTER_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenExtrafenster.isSelected()));
            initFrames();
        });
        jMenuAnsicht.add(jCheckBoxMeldungenAnzeigen);
        jMenuAnsicht.add(jCheckBoxMeldungenExtrafenster);
        // ===============================

        // Hilfe
        jMenuItemAnleitung.addActionListener(e -> {
            MVHelpDialog dialogOk = new MVHelpDialog(daten.mediathekGui, true, daten, "Hilfe zum Programm");
            dialogOk.setVisible(true);
        });
        // Über
        jMenuItemAbout.addActionListener(e -> showAboutDialog());
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
        daten.dialogMediaDB.tabelleSpeichern();

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
            MVConfig.add(MVConfig.SYSTEM_FENSTER_MAX, Boolean.TRUE.toString());
        } else {
            MVConfig.add(MVConfig.SYSTEM_FENSTER_MAX, Boolean.FALSE.toString());
        }

        // Hauptfenster
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_GUI, this);
        // Dialog Einstellungen
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_EINSTELLUNGEN, dialogEinstellungen);
        // Infodialog/Bandwidth
        if (mvDownloadInfo != null) {
            GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_INFODIALOG, mvDownloadInfo.getDialog());
            mvDownloadInfo.getDividerLocation();
        }
        // MediaDB
        GuiFunktionen.getSize(MVConfig.SYSTEM_MEDIA_DB_DIALOG_GROESSE, daten.dialogMediaDB);

        MVConfig.add(MVConfig.SYSTEM_BREITE_MELDUNGEN, String.valueOf(splitPane.getDividerLocation()));

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
        Log.endMsg();

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

        switch (getOs()) {
            case LINUX:
                //strShutdownCommand = "shutdown -h now";
                strShutdownCommand = MVConfig.get(MVConfig.SYSTEM_LINUX_SHUTDOWN);
                if (strShutdownCommand.isEmpty()) {
                    // sicherheitshalber
                    strShutdownCommand = Konstanten.SHUTDOWN_LINUX;
                    MVConfig.add(MVConfig.SYSTEM_LINUX_SHUTDOWN, Konstanten.SHUTDOWN_LINUX);
                }
                break;

            case WIN32:
            case WIN64:
                strShutdownCommand = "shutdown.exe -s -t 0";
                break;

            default:
                Log.errorLog(465321789, "Shutdown unsupported operating system ...");
                break;
        }

        //only run if we have a proper shutdown command...
        if (!strShutdownCommand.isEmpty()) {
            try {
                SysMsg.sysMsg("Shutdown: " + strShutdownCommand);
                Runtime.getRuntime().exec(strShutdownCommand);
            } catch (IOException ex) {
                Log.errorLog(915263047, ex);
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jPanelInfo = new javax.swing.JPanel();
        jTabbedPane = new javax.swing.JTabbedPane();
        jMenuBar = new javax.swing.JMenuBar();
        jMenuDatei = new javax.swing.JMenu();
        jMenuItemFilmlisteLaden = new javax.swing.JMenuItem();
        jMenuItemEinstellungen = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JPopupMenu.Separator();
        jMenuItemBeenden = new javax.swing.JMenuItem();
        jMenuFilme = new javax.swing.JMenu();
        jMenuItemFilmAbspielen = new javax.swing.JMenuItem();
        jMenuItemFilmAufzeichnen = new javax.swing.JMenuItem();
        jMenuItemFilterLoeschen = new javax.swing.JMenuItem();
        jMenuItemBlacklist = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator6 = new javax.swing.JPopupMenu.Separator();
        jMenuItemFilmeGesehen = new javax.swing.JMenuItem();
        jMenuItemFilmeUngesehen = new javax.swing.JMenuItem();
        jMenuItemFilmeMediensammlung = new javax.swing.JMenuItem();
        jMenuDownload = new javax.swing.JMenu();
        jMenuItemDownloadsAlleStarten = new javax.swing.JMenuItem();
        jMenuItemDownloadStartTime = new javax.swing.JMenuItem();
        jMenuItemDownloadAlleStoppen = new javax.swing.JMenuItem();
        jMenuItemDownloadWartendeStoppen = new javax.swing.JMenuItem();
        jMenuItemDownloadsAktualisieren = new javax.swing.JMenuItem();
        jMenuItemDownloadsAufraeumen = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator3 = new javax.swing.JPopupMenu.Separator();
        jMenuItemDownloadStarten = new javax.swing.JMenuItem();
        jMenuItemDownloadStoppen = new javax.swing.JMenuItem();
        jMenuItemDownloadVorziehen = new javax.swing.JMenuItem();
        jMenuItemDownloadsZurueckstellen = new javax.swing.JMenuItem();
        jMenuItemDownloadsLoeschen = new javax.swing.JMenuItem();
        jMenuItemDownloadAendern = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator1 = new javax.swing.JPopupMenu.Separator();
        jMenuItemDownloadGesehen = new javax.swing.JMenuItem();
        jMenuItemDownloadUngesehen = new javax.swing.JMenuItem();
        jMenuItemDownloadMediensammlung = new javax.swing.JMenuItem();
        jMenuItemDownloadAbspielen = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator7 = new javax.swing.JPopupMenu.Separator();
        jMenuItemDownloadShutDown = new javax.swing.JMenuItem();
        jMenuAbos = new javax.swing.JMenu();
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
        jCheckBoxMenuItemMediaDb = new javax.swing.JCheckBoxMenuItem();
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

        jMenuFilme.setMnemonic('F');
        jMenuFilme.setText("Filme");

        jMenuItemFilmAbspielen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/film_start_16.png"))); // NOI18N
        jMenuItemFilmAbspielen.setText("Film abspielen");
        jMenuFilme.add(jMenuItemFilmAbspielen);

        jMenuItemFilmAufzeichnen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemFilmAufzeichnen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/film_rec_16.png"))); // NOI18N
        jMenuItemFilmAufzeichnen.setText("Film aufzeichnen");
        jMenuFilme.add(jMenuItemFilmAufzeichnen);

        jMenuItemFilterLoeschen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemFilterLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/clear_16.png"))); // NOI18N
        jMenuItemFilterLoeschen.setText("Filter löschen");
        jMenuFilme.add(jMenuItemFilterLoeschen);

        jMenuItemBlacklist.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_B, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemBlacklist.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/blacklist_16.png"))); // NOI18N
        jMenuItemBlacklist.setText("Blacklist öffnen");
        jMenuFilme.add(jMenuItemBlacklist);
        jMenuFilme.add(jSeparator6);

        jMenuItemFilmeGesehen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemFilmeGesehen.setText("Filme als gesehen markieren");
        jMenuFilme.add(jMenuItemFilmeGesehen);

        jMenuItemFilmeUngesehen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemFilmeUngesehen.setText("Filme als ungesehen markieren");
        jMenuFilme.add(jMenuItemFilmeUngesehen);

        jMenuItemFilmeMediensammlung.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemFilmeMediensammlung.setText("Titel in der Mediensammlung suchen");
        jMenuFilme.add(jMenuItemFilmeMediensammlung);

        jMenuBar.add(jMenuFilme);

        jMenuDownload.setMnemonic('O');
        jMenuDownload.setText("Download");

        jMenuItemDownloadsAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_alleStarten_16.png"))); // NOI18N
        jMenuItemDownloadsAlleStarten.setText("alle Downloads starten");
        jMenuDownload.add(jMenuItemDownloadsAlleStarten);

        jMenuItemDownloadStartTime.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_alleStarten_16.png"))); // NOI18N
        jMenuItemDownloadStartTime.setText("alle Downloads um xx:yy Uhr starten");
        jMenuDownload.add(jMenuItemDownloadStartTime);

        jMenuItemDownloadAlleStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_stop_16.png"))); // NOI18N
        jMenuItemDownloadAlleStoppen.setText("alle stoppen");
        jMenuItemDownloadAlleStoppen.setToolTipText("alle Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadAlleStoppen);

        jMenuItemDownloadWartendeStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_stop_16.png"))); // NOI18N
        jMenuItemDownloadWartendeStoppen.setText("wartende stoppen");
        jMenuItemDownloadWartendeStoppen.setToolTipText("wartende Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadWartendeStoppen);

        jMenuItemDownloadsAktualisieren.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemDownloadsAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/view-refresh_16.png"))); // NOI18N
        jMenuItemDownloadsAktualisieren.setText("Liste der Downloads aktualisieren");
        jMenuDownload.add(jMenuItemDownloadsAktualisieren);

        jMenuItemDownloadsAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/download_clear_16.png"))); // NOI18N
        jMenuItemDownloadsAufraeumen.setText("Liste der Downloads aufräumen");
        jMenuDownload.add(jMenuItemDownloadsAufraeumen);
        jMenuDownload.add(jSeparator3);

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
        jMenuItemDownloadsLoeschen.setText("Downloads aus Liste entfernen");
        jMenuDownload.add(jMenuItemDownloadsLoeschen);

        jMenuItemDownloadAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/configure_16.png"))); // NOI18N
        jMenuItemDownloadAendern.setText("Download ändern");
        jMenuDownload.add(jMenuItemDownloadAendern);
        jMenuDownload.add(jSeparator1);

        jMenuItemDownloadGesehen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemDownloadGesehen.setText("Filme als gesehen markieren");
        jMenuDownload.add(jMenuItemDownloadGesehen);

        jMenuItemDownloadUngesehen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemDownloadUngesehen.setText("Filme als ungesehen markieren");
        jMenuDownload.add(jMenuItemDownloadUngesehen);

        jMenuItemDownloadMediensammlung.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemDownloadMediensammlung.setText("Titel in der Mediensammlung suchen");
        jMenuDownload.add(jMenuItemDownloadMediensammlung);

        jMenuItemDownloadAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/film_start_16.png"))); // NOI18N
        jMenuItemDownloadAbspielen.setText("gespeicherten Film abspielen");
        jMenuDownload.add(jMenuItemDownloadAbspielen);
        jMenuDownload.add(jSeparator7);

        jMenuItemDownloadShutDown.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/beenden_16.png"))); // NOI18N
        jMenuItemDownloadShutDown.setText("Rechner nach Downloads herunterfahren");
        jMenuDownload.add(jMenuItemDownloadShutDown);

        jMenuBar.add(jMenuDownload);

        jMenuAbos.setMnemonic('b');
        jMenuAbos.setText("Abo");

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

        jCheckBoxMenuItemMediaDb.setText("Mediensammlung durchsuchen");
        jMenuAnsicht.add(jCheckBoxMenuItemMediaDb);

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
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addGap(6, 6, 6)
                .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 805, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBoxMenuItem cbBandwidthDisplay;
    protected javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemBeschreibung;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemMediaDb;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemToolBar;
    protected javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemVideoplayer;
    private javax.swing.JMenu jMenuAbos;
    private javax.swing.JMenu jMenuAnsicht;
    private javax.swing.JMenuBar jMenuBar;
    protected javax.swing.JMenu jMenuDatei;
    protected javax.swing.JMenu jMenuDownload;
    private javax.swing.JMenu jMenuFilme;
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
    private javax.swing.JMenuItem jMenuItemDownloadGesehen;
    private javax.swing.JMenuItem jMenuItemDownloadMediensammlung;
    private javax.swing.JMenuItem jMenuItemDownloadShutDown;
    private javax.swing.JMenuItem jMenuItemDownloadStartTime;
    private javax.swing.JMenuItem jMenuItemDownloadStarten;
    private javax.swing.JMenuItem jMenuItemDownloadStoppen;
    private javax.swing.JMenuItem jMenuItemDownloadUngesehen;
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
    private javax.swing.JMenuItem jMenuItemFilmeGesehen;
    private javax.swing.JMenuItem jMenuItemFilmeMediensammlung;
    private javax.swing.JMenuItem jMenuItemFilmeUngesehen;
    private javax.swing.JMenuItem jMenuItemFilmlisteLaden;
    protected javax.swing.JMenuItem jMenuItemFilterLoeschen;
    private javax.swing.JMenuItem jMenuItemSchriftGr;
    private javax.swing.JMenuItem jMenuItemSchriftKl;
    private javax.swing.JMenuItem jMenuItemSchriftNormal;
    private javax.swing.JPanel jPanelInfo;
    protected javax.swing.JPopupMenu.Separator jSeparator2;
    protected javax.swing.JPopupMenu.Separator jSeparator4;
    private javax.swing.JTabbedPane jTabbedPane;
    // End of variables declaration//GEN-END:variables

}
