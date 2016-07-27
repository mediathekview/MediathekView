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
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Functions.OperatingSystemType;
import static mSearch.tool.Functions.getOs;
import mSearch.tool.*;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.CheckUpdate;
import mediathek.controller.ProgStart;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.gui.*;
import mediathek.gui.dialog.*;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.res.GetIcon;
import static mediathek.tool.MVFunctionSys.startMeldungen;
import mediathek.tool.*;

public class MediathekGui extends JFrame {

    private final Daten daten;
    private final SpacerIcon spacerIcon = new SpacerIcon(30);
    private final JSpinner jSpinnerAnzahl = new JSpinner(new SpinnerNumberModel(1, 1, 9, 1));
    private final JLabel jLabelAnzahl = new JLabel("Anzahl gleichzeitige Downloads");
    private final JPanel jPanelAnzahl = new JPanel();
    private final JLabel jLabelBandbreite = new JLabel("Bandbreite pro Download");
    private final JPanel jPanelBandbreite = new JPanel();
    private final JSlider jSliderBandbreite = new JSlider();
    private MVStatusBar statusBar;
    private MVFrame frameDownload = null;
    private MVFrame frameAbo = null;
    private MVFrame frameMeldungen = null;
    private final JCheckBoxMenuItem jCheckBoxFilterAnzeigen = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxFilterExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxDownloadExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxAboExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxMeldungenAnzeigen = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxMeldungenExtrafenster = new JCheckBoxMenuItem();
    private MVTray tray = null;

    public static enum TABS {
        TAB_NIX, TAB_FILME, TAB_DOWNLOADS, TAB_ABOS, TAB_MELDUNGEN
    };
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

        Duration.counterStart("***Programmstart***");

        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE); // soll abgefangen werden
        setIconImage(GetIcon.getIcon("MediathekView.png", "/mediathek/res/", 58, 58).getImage());
        //Hier wird F10 default Funktion unterbunden:
        InputMap im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(KeyStroke.getKeyStroke("F10"), "none");

        updateSplashScreenText("Anwendungsdaten laden...");

        daten = new Daten(pfad, this);

        startMeldungen();
        Duration.staticPing("Start");

        if (daten.allesLaden()) {
            // alles geladen
            updateSplashScreenText("GUI Initialisieren...");
        } else {
            Duration.staticPing("Erster Start");
            // erster Start
            ReplaceList.init(); // einmal ein Muster anlegen, für Linux/OS X ist es bereits aktiv!
            new DialogStarteinstellungen(this, daten).setVisible(true);
            this.pack();
        }

        Duration.staticPing("Start Gui");
        createStatusBar();
        //create the Film Information HUD
        if (SystemInfo.isMacOSX()) {
            Daten.filmInfo = new MVFilmInformation(this, jTabbedPane, daten);
        } else {
            //klappte nicht auf allen Desktops
            Daten.filmInfo = new MVFilmInformationLinux(this, jTabbedPane, daten);
        }

        setOrgTitel();
        setLookAndFeel();
        init();
        setSize();
        Duration.staticPing("Init GUI");

        // Dialog mit den Programmeinstellungen einrichten
        Daten.dialogEinstellungen = new DialogEinstellungen(this, daten);
        Daten.dialogMediaDB = new DialogMediaDB(this);
        Daten.dialogMediaDB.setVis();

        addListener();

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
        Duration.staticPing("Gui steht!");

        ProgStart.loadDataProgStart();

    }

    private void addListener() {
        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIATHEKGUI_ORG_TITEL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setOrgTitel();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIATHEKGUI_PROGRAMM_AKTUELL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitle("Programmversion ist aktuell");
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitle("Ein Programmupdate ist verfügbar");
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_FILM_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setCbBeschreibung();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_DOWNLOAD_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setCbBeschreibung();
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
        Listener.addListener(new Listener(Listener.EREIGNIS_TABS_TOP, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                designTabs();
            }
        });
    }

    private void setFocusSuchfeld() {
        Listener.notify(Listener.EREIGNIS_SUCHFELD_FOCUS_SETZEN, MediathekGui.class.getName());
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

    private void setOrgTitel() {
        this.setTitle(Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION);
    }

    private void setSize() {
        if (Daten.startMaximized || Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_MAX))) {
            this.setExtendedState(Frame.MAXIMIZED_BOTH);
        } else {
            GuiFunktionen.setSize(MVConfig.SYSTEM_GROESSE_GUI, this, null);
        }
    }

    private void setCbBeschreibung() {
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN))
                && Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN))) {
            //dann sind beide an
            cbkBeschreibung.setSelected(true);
            cbkBeschreibung.setForeground(null);
        } else if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN))
                || Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN))) {
            //dann ists nur einer
            cbkBeschreibung.setSelected(false);
            cbkBeschreibung.setForeground(new java.awt.Color(0, 51, 153));
        } else {
            //keiner
            cbkBeschreibung.setSelected(false);
            cbkBeschreibung.setForeground(null);
        }
    }

    private void init() {
        initTabs();
        initMenue();
        Daten.filmeLaden.addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                jMenuItemFilmlisteLaden.setEnabled(false);
                jMenuItemDownloadsAktualisieren.setEnabled(false);
            }

            @Override
            public void progress(ListenerFilmeLadenEvent event) {
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                jMenuItemFilmlisteLaden.setEnabled(true);
                jMenuItemDownloadsAktualisieren.setEnabled(true);
                daten.allesSpeichern(); // damit nichts verlorengeht
                // Prüfen obs ein Programmupdate gibt
                Duration.staticPing("CheckUpdate");
                new CheckUpdate(Daten.mediathekGui, daten).checkProgUpdate();
                Daten.listeMediaDB.createMediaDB();
            }
        });
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                if (tray != null && !SystemInfo.isMacOSX() && Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_USE_TRAY))) {
                    Daten.mediathekGui.setVisible(false);
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

    private static boolean geklickt = false;

    private void initTabs() {
        Daten.guiDownloads = new GuiDownloads(daten, Daten.mediathekGui);
        Daten.guiAbo = new GuiAbo(daten, Daten.mediathekGui);
        Daten.guiMeldungen = new GuiMeldungen(daten, this);
        Daten.guiFilme = new GuiFilme(daten, Daten.mediathekGui);

        //jTabbedPane.addTab("Filme", Icons.ICON_TAB_FILM, Daten.guiFilme);
        jTabbedPane.addTab("Filme", Daten.guiFilme);

        if (Daten.debug) {
            Daten.guiDebug = new GuiDebug(daten);
            //jTabbedPane.addTab("Debug", spacerIcon, Daten.guiDebug);
            jTabbedPane.addTab("Debug", Daten.guiDebug);
        }
        jTabbedPane.addChangeListener(l -> {

            if (!geklickt) {
                geklickt = true;
                Duration.counterStop("***Programmstart***");
            }

            designTabs();
        });

        initFrames();
    }

    public void hideFrame(TABS state) {
        switch (state) {
            case TAB_DOWNLOADS:
                jCheckBoxDownloadExtrafenster.setSelected(false);
                MVConfig.add(MVConfig.SYSTEM_FENSTER_DOWNLOAD, Boolean.toString(false));
                break;
            case TAB_ABOS:
                jCheckBoxAboExtrafenster.setSelected(false);
                MVConfig.add(MVConfig.SYSTEM_FENSTER_ABO, Boolean.toString(false));
                break;
            case TAB_MELDUNGEN:
                jCheckBoxMeldungenAnzeigen.setSelected(true);
                jCheckBoxMeldungenExtrafenster.setSelected(false);
                MVConfig.add(MVConfig.SYSTEM_VIS_MELDUNGEN, Boolean.toString(true));
                MVConfig.add(MVConfig.SYSTEM_FENSTER_MELDUNGEN, Boolean.toString(false));
                break;
        }
        initFrames();
    }

    private void initFrames() {
        // Downloads
        int nr = 1;
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_DOWNLOAD))) {
            frameDownload = setFrame(frameDownload, MVConfig.SYSTEM_GROESSE_DOWNLOAD, Daten.guiDownloads, TABS.TAB_DOWNLOADS);
        } else {
            setTab(frameDownload, Daten.guiDownloads, "Downloads", nr++);
        }

        // Abos
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_ABO))) {
            frameAbo = setFrame(frameAbo, MVConfig.SYSTEM_GROESSE_ABO, Daten.guiAbo, TABS.TAB_ABOS);
        } else {
            setTab(frameAbo, Daten.guiAbo, "Abos", nr++);
        }

        // Meldungen
        if (!Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_MELDUNGEN))) {
            hide(frameMeldungen, Daten.guiMeldungen);
        } else if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_MELDUNGEN))) {
            frameMeldungen = setFrame(frameMeldungen, MVConfig.SYSTEM_GROESSE_MELDUNGEN, Daten.guiMeldungen, TABS.TAB_MELDUNGEN);
        } else {
            setTab(frameMeldungen, Daten.guiMeldungen, "Meldungen", nr);
        }
        jTabbedPane.updateUI();
        designTabs();
        jTabbedPane.setSelectedIndex(0);
        Daten.guiFilme.isShown();
    }

    private void hide(MVFrame frame, PanelVorlage panelVorlage) {
        panelVorlage.solo = true;
        if (frame != null) {
            frame.dispose();
            frame = null;
        }
        if (tabContain(panelVorlage)) {
            jTabbedPane.remove(panelVorlage);
        }
    }

    private MVFrame setFrame(MVFrame frame, String nrGroesse, PanelVorlage panel, TABS sparte) {
        hide(frame, panel);
        panel.solo = true;
        frame = new MVFrame(daten, panel, sparte);
        frame.setSize(nrGroesse);
        frame.setVisible(true);
        return frame;
    }

    private void setTab(MVFrame frame, PanelVorlage panel, String titel, int nrTab) {
        hide(frame, panel);
        //jTabbedPane.add(panel, spacerIcon, nrTab);
        jTabbedPane.add(panel, nrTab);
        jTabbedPane.setTitleAt(nrTab, titel);
        panel.solo = false;
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

    private void designTabs() {
        boolean top = Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_TABS_TOP));
        boolean icon = Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_TABS_ICON));
        if (top) {
            jTabbedPane.setTabPlacement(JTabbedPane.TOP);
        } else {
            jTabbedPane.setTabPlacement(JTabbedPane.LEFT);
        }
        jTabbedPane.updateUI();
        for (int i = 0; i < jTabbedPane.getTabCount(); ++i) {
            Component c = jTabbedPane.getComponentAt(i);
            ImageIcon ic = null;
            if (c.equals(Daten.guiFilme)) {
                if (jTabbedPane.getSelectedIndex() == i) {
                    ic = top ? Icons.ICON_TAB_TOP_FILM : Icons.ICON_TAB_FILM;
                } else {
                    ic = top ? Icons.ICON_TAB_TOP_FILM_SW : Icons.ICON_TAB_FILM_SW;
                }
            }
            if (c.equals(Daten.guiDownloads)) {
                if (jTabbedPane.getSelectedIndex() == i) {
                    ic = top ? Icons.ICON_TAB_TOP_DOWNLOAD : Icons.ICON_TAB_DOWNLOAD;
                } else {
                    ic = top ? Icons.ICON_TAB_TOP_DOWNLOAD_SW : Icons.ICON_TAB_DOWNLOAD_SW;
                }
            }
            if (c.equals(Daten.guiAbo)) {
                if (jTabbedPane.getSelectedIndex() == i) {
                    ic = top ? Icons.ICON_TAB_TOP_ABO : Icons.ICON_TAB_ABO;
                } else {
                    ic = top ? Icons.ICON_TAB_TOP_ABO_SW : Icons.ICON_TAB_ABO_SW;
                }
            }
            if (c.equals(Daten.guiMeldungen)) {
                if (jTabbedPane.getSelectedIndex() == i) {
                    ic = top ? Icons.ICON_TAB_TOP_MELDUNG : Icons.ICON_TAB_MELDUNG;
                } else {
                    ic = top ? Icons.ICON_TAB_TOP_MELDUNG_SW : Icons.ICON_TAB_MELDUNG_SW;
                }
            }
            if (c.equals(Daten.guiDebug)) {
                if (jTabbedPane.getSelectedIndex() == i) {
                    ic = top ? Icons.ICON_TAB_TOP_MELDUNG : Icons.ICON_TAB_MELDUNG;
                } else {
                    ic = top ? Icons.ICON_TAB_TOP_MELDUNG_SW : Icons.ICON_TAB_MELDUNG_SW;
                }
            }
            String s = jTabbedPane.getTitleAt(i);
            JLabel lbl = makeLable(s, ic);
            if (icon) {
                jTabbedPane.setTabComponentAt(i, lbl);
            } else {
                jTabbedPane.setTabComponentAt(i, null);
            }
        }

        jTabbedPane.updateUI();
    }

    private JLabel makeLable(String text, ImageIcon ic) {
        JLabel lbl = new JLabel(text);

        lbl.setBorder(null);
        lbl.setIcon(ic);
        lbl.setOpaque(false);

        if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_TABS_TOP))) {
            lbl.setBorder(new EmptyBorder(10, 5, 10, 5));
            lbl.setVerticalTextPosition(JLabel.CENTER);
            lbl.setVerticalAlignment(JLabel.CENTER);
            lbl.setHorizontalTextPosition(JLabel.RIGHT);
            lbl.setHorizontalAlignment(JLabel.LEFT);
        } else {
            lbl.setBorder(new EmptyBorder(10, 5, 10, 5));
            lbl.setVerticalTextPosition(JLabel.TOP);
            lbl.setVerticalAlignment(JLabel.BOTTOM);
            lbl.setHorizontalTextPosition(JLabel.CENTER);
            lbl.setHorizontalAlignment(JLabel.CENTER);
        }

        return lbl;
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
        jLabelBandbreite.setIcon(Icons.ICON_MENUE_DOWNLOAD_BANDWITH);
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
        jLabelAnzahl.setIcon(Icons.ICON_MENUE_UP_DOWN);
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
        setCbBeschreibung();
        jMenuFilme.addMenuListener(new MenuLST(TABS.TAB_FILME));
        jMenuDownload.addMenuListener(new MenuLST(TABS.TAB_DOWNLOADS));
        jMenuAbos.addMenuListener(new MenuLST(TABS.TAB_ABOS));

        //Icons setzen
        jMenuItemFilmlisteLaden.setIcon(Icons.ICON_MENUE_FILMLISTE_LADEN);
        jMenuItemEinstellungen.setIcon(Icons.ICON_MENUE_EINSTELLUNGEN);
        jMenuItemBeenden.setIcon(Icons.ICON_MENUE_BEENDEN);
        jMenuItemFilmAbspielen.setIcon(Icons.ICON_MENUE_FILM_START);
        jMenuItemFilmAufzeichnen.setIcon(Icons.ICON_MENUE_FILM_REC);
        jMenuItemFilmeGesehen.setIcon(Icons.ICON_MENUE_HISTORY_ADD);
        jMenuItemFilmeUngesehen.setIcon(Icons.ICON_MENUE_HISTORY_REMOVE);
        jMenuItemBlacklist.setIcon(Icons.ICON_MENUE_BLACKLIST);
        jMenuItemFilterLoeschen.setIcon(Icons.ICON_MENUE_CLEAR);
        jMenuItemDownloadsAlleStarten.setIcon(Icons.ICON_MENUE_DOWNLOAD_ALLE_STARTEN);
        jMenuItemDownloadStartTime.setIcon(Icons.ICON_MENUE_DOWNLOAD_ALLE_STARTEN);
        jMenuItemDownloadAlleStoppen.setIcon(Icons.ICON_MENUE_DOWNOAD_STOP);
        jMenuItemDownloadWartendeStoppen.setIcon(Icons.ICON_MENUE_DOWNOAD_STOP);
        jMenuItemDownloadStarten.setIcon(Icons.ICON_MENUE_DOWNOAD_STARTEN);
        jMenuItemDownloadStoppen.setIcon(Icons.ICON_MENUE_DOWNOAD_STOP);
        jMenuItemDownloadVorziehen.setIcon(Icons.ICON_MENUE_VORZIEHEN);
        jMenuItemDownloadsZurueckstellen.setIcon(Icons.ICON_MENUE_DOWNLOAD_ZURUECKSTELLEN);
        jMenuItemDownloadsLoeschen.setIcon(Icons.ICON_MENUE_DOWNOAD_LOESCHEN);
        jMenuItemDownloadAendern.setIcon(Icons.ICON_MENUE_DOWNLOAD_AENDERN);
        jMenuItemDownloadsAktualisieren.setIcon(Icons.ICON_MENUE_AKTUALISIEREN);
        jMenuItemDownloadAbspielen.setIcon(Icons.ICON_MENUE_FILM_START);
        jMenuItemDownloadsAufraeumen.setIcon(Icons.ICON_MENUE_CLEAR);
        jMenuItemDownloadShutDown.setIcon(Icons.ICON_MENUE_BEENDEN);
        jMenuItemDownloadGesehen.setIcon(Icons.ICON_MENUE_HISTORY_ADD);
        jMenuItemDownloadUngesehen.setIcon(Icons.ICON_MENUE_HISTORY_REMOVE);
        jMenuItemAbosEinschalten.setIcon(Icons.ICON_MENUE_EIN);
        jMenuItemAbosAusschalten.setIcon(Icons.ICON_MENUE_AUS);
        jMenuItemAbosLoeschen.setIcon(Icons.ICON_MENUE_ABO_LOESCHEN);
        jMenuItemAbosAendern.setIcon(Icons.ICON_MENUE_ABO_AENDERN);
        jMenuItemAboNeu.setIcon(Icons.ICON_MENUE_ABO_NEU);
        jMenuItemAnleitung.setIcon(Icons.ICON_MENUE_HELP);

        setupMaximumNumberOfDownloadsMenuItem();
        setupBandwidthMenuItem();

        // Datei
        jMenuItemEinstellungen.addActionListener(e -> Daten.dialogEinstellungen.setVisible(true));
        jMenuItemBeenden.addActionListener(e -> beenden(false, false));

        // Filme
        jMenuItemFilmlisteLaden.addActionListener(e -> Daten.filmeLaden.loadFilmlistDialog(daten, false));
        jMenuItemFilmAbspielen.addActionListener(e -> Daten.guiFilme.guiFilmeFilmAbspielen());
        jMenuItemFilmAufzeichnen.addActionListener(e -> Daten.guiFilme.guiFilmeFilmSpeichern());
        jMenuItemFilterLoeschen.addActionListener(e -> Daten.guiFilme.guiFilmeFilterLoeschen());
        jMenuItemBlacklist.addActionListener(e -> {
            DialogLeer dialog = new DialogLeer(Daten.mediathekGui, true);
            dialog.init("Blacklist", new PanelBlacklist(daten, Daten.mediathekGui, PanelBlacklist.class.getName() + "_2"));
            dialog.setVisible(true);
        });
        jMenuItemFilmeGesehen.addActionListener(e -> Daten.guiFilme.filmGesehen());
        jMenuItemFilmeUngesehen.addActionListener(e -> Daten.guiFilme.filmUngesehen());
        jMenuItemFilmeMediensammlung.addActionListener(e -> Daten.guiFilme.guiFilmMediensammlung());

        // Downloads
        jMenuItemDownloadsAktualisieren.addActionListener(e -> Daten.guiDownloads.aktualisieren());
        jMenuItemDownloadAbspielen.addActionListener(e -> Daten.guiDownloads.filmAbspielen());
        jMenuItemDownloadsAufraeumen.addActionListener(e -> Daten.guiDownloads.aufraeumen());
        jMenuItemDownloadsLoeschen.addActionListener(e -> Daten.guiDownloads.loeschen());
        jMenuItemDownloadsAlleStarten.addActionListener(e -> Daten.guiDownloads.starten(true /* alle */));
        jMenuItemDownloadStartTime.addActionListener(e -> Daten.guiDownloads.startAtTime());
        jMenuItemDownloadStarten.addActionListener(e -> Daten.guiDownloads.starten(false /* alle */));
        jMenuItemDownloadsZurueckstellen.addActionListener(e -> Daten.guiDownloads.zurueckstellen());
        jMenuItemDownloadVorziehen.addActionListener(e -> Daten.guiDownloads.vorziehen());
        jMenuItemDownloadAendern.addActionListener(e -> Daten.guiDownloads.aendern());
        jMenuItemDownloadAlleStoppen.addActionListener(e -> Daten.guiDownloads.stoppen(true /* alle */));
        jMenuItemDownloadWartendeStoppen.addActionListener(e -> Daten.guiDownloads.wartendeStoppen());
        jMenuItemDownloadStoppen.addActionListener(e -> Daten.guiDownloads.stoppen(false /* alle */));
        jMenuItemDownloadShutDown.addActionListener(e -> {
            if (Daten.listeDownloads.nochNichtFertigeDownloads() > 0) {
                // ansonsten gibts keine laufenden Downloads auf die man warten sollte
                beenden(true /*Dialog auf "warten" einstellen*/, false /*shutdown computer*/);
            } else {
                MVMessageDialog.showMessageDialog(Daten.mediathekGui, "Die Downloads müssen zuerst gestartet werden.",
                        "Keine laufenden Downloads!", JOptionPane.ERROR_MESSAGE);
            }
        });
        jMenuItemDownloadGesehen.addActionListener(e -> Daten.guiDownloads.filmGesehen());
        jMenuItemDownloadUngesehen.addActionListener(e -> Daten.guiDownloads.filmUngesehen());
        jMenuItemDownloadMediensammlung.addActionListener(e -> Daten.guiDownloads.guiFilmMediensammlung());
        jMenuItemDownloadInvertSelection.addActionListener(e -> Daten.guiDownloads.invertSelection());
        // Abo
        jMenuItemAbosEinschalten.addActionListener(e -> Daten.guiAbo.einAus(true));
        jMenuItemAbosAusschalten.addActionListener(e -> Daten.guiAbo.einAus(false));
        jMenuItemAbosLoeschen.addActionListener(e -> Daten.guiAbo.loeschen());
        jMenuItemAbosAendern.addActionListener(e -> Daten.guiAbo.aendern());
        jMenuItemAboNeu.addActionListener(e -> Daten.guiAbo.neu());
        jMenuItemAboInvertSelection.addActionListener(e -> Daten.guiAbo.invertSelection());

        // Ansicht
        jCheckBoxMenuItemToolBar.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_TOOLBAR_ALLES_ANZEIGEN)));
        jCheckBoxMenuItemToolBar.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_TOOLBAR_ALLES_ANZEIGEN, Boolean.toString(jCheckBoxMenuItemToolBar.isSelected()));
            Listener.notify(Listener.EREIGNIS_TOOLBAR_VIS, MediathekGui.class.getSimpleName());
        });
        jCheckBoxMenuItemVideoplayer.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
        jCheckBoxMenuItemVideoplayer.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, String.valueOf(jCheckBoxMenuItemVideoplayer.isSelected()));
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName());
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxMenuItemVideoplayer.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
            }
        });
        cbkBeschreibung.addActionListener(l -> {
            //Filme
            MVConfig.add(MVConfig.SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN, String.valueOf(cbkBeschreibung.isSelected()));
            Listener.notify(Listener.EREIGNIS_FILM_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName());
            //Downloads
            MVConfig.add(MVConfig.SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN, String.valueOf(cbkBeschreibung.isSelected()));
            Listener.notify(Listener.EREIGNIS_DOWNLOAD_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName());
            setCbBeschreibung();
        });

        jCheckBoxMenuItemMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
        jCheckBoxMenuItemMediaDb.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, String.valueOf(jCheckBoxMenuItemMediaDb.isSelected()));
            Daten.dialogMediaDB.setVis();
        });
        jMenuItemSchriftGr.addActionListener(e -> MVFont.setFontSize(true));
        jMenuItemSchriftKl.addActionListener(e -> MVFont.setFontSize(false));
        jMenuItemSchriftNormal.addActionListener(e -> MVFont.resetFontSize());

        //Ansicht Filter
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

        //Ansicht Downloads
        jMenuAnsicht.add(new JSeparator());

        jCheckBoxDownloadExtrafenster.setText("Downloads in Extrafenster");
        jMenuAnsicht.add(jCheckBoxDownloadExtrafenster);
        jCheckBoxDownloadExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_DOWNLOAD)));
        jCheckBoxDownloadExtrafenster.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_FENSTER_DOWNLOAD, Boolean.toString(jCheckBoxDownloadExtrafenster.isSelected()));
            initFrames();
        });

        //Ansicht Abos
        jCheckBoxAboExtrafenster.setText("Abos in Extrafenster");
        jMenuAnsicht.add(jCheckBoxAboExtrafenster);
        jCheckBoxAboExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_ABO)));
        jCheckBoxAboExtrafenster.addActionListener(e -> {
            MVConfig.add(MVConfig.SYSTEM_FENSTER_ABO, Boolean.toString(jCheckBoxAboExtrafenster.isSelected()));
            initFrames();
        });

        //Ansicht Meldungen
        jCheckBoxMeldungenAnzeigen.setText("Meldungen anzeigen");
        jCheckBoxMeldungenAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_MELDUNGEN)));
        jCheckBoxMeldungenAnzeigen.addActionListener(e -> {
            if (!jCheckBoxMeldungenAnzeigen.isSelected()) {
                jCheckBoxMeldungenExtrafenster.setSelected(false);
            }
            MVConfig.add(MVConfig.SYSTEM_VIS_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenAnzeigen.isSelected()));
            MVConfig.add(MVConfig.SYSTEM_FENSTER_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenExtrafenster.isSelected()));
            initFrames();
        });
        jCheckBoxMeldungenExtrafenster.setText("in Extrafenster");
        jCheckBoxMeldungenExtrafenster.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jCheckBoxMeldungenExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_FENSTER_MELDUNGEN)));
        jCheckBoxMeldungenExtrafenster.addActionListener(e -> {
            if (jCheckBoxMeldungenExtrafenster.isSelected()) {
                jCheckBoxMeldungenAnzeigen.setSelected(true);
            }
            MVConfig.add(MVConfig.SYSTEM_VIS_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenAnzeigen.isSelected()));
            MVConfig.add(MVConfig.SYSTEM_FENSTER_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenExtrafenster.isSelected()));
            initFrames();
        });
        jMenuAnsicht.add(jCheckBoxMeldungenAnzeigen);
        jMenuAnsicht.add(jCheckBoxMeldungenExtrafenster);

        // Hilfe
        jMenuItemAnleitung.addActionListener(e -> {
            HelpDialog dialogOk = new HelpDialog(Daten.mediathekGui, daten);
            dialogOk.setVisible(true);
        });
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
        Daten.guiFilme.tabelleSpeichern();
        Daten.guiDownloads.tabelleSpeichern();
        Daten.guiAbo.tabelleSpeichern();
        Daten.dialogMediaDB.tabelleSpeichern();

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
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_EINSTELLUNGEN, Daten.dialogEinstellungen);
        // Infodialog/Bandwidth
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_INFODIALOG, mvDownloadInfo.getDialog());
        if (mvDownloadInfo != null) {
            mvDownloadInfo.getDividerLocation();
        }
        // MediaDB
        GuiFunktionen.getSize(MVConfig.SYSTEM_MEDIA_DB_DIALOG_GROESSE, Daten.dialogMediaDB);

        // Frames
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_DOWNLOAD, frameDownload);
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_ABO, frameAbo);
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_MELDUNGEN, frameMeldungen);

        // FilterFrame
        GuiFunktionen.getSize(MVConfig.SYSTEM_GROESSE_FILTER, Daten.guiFilme.mVFilterFrame);
        daten.allesSpeichern();
        Log.endMsg();
        Duration.printCounter();

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

        jPanelCont = new javax.swing.JPanel();
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
        jMenuItemDownloadAbspielen = new javax.swing.JMenuItem();
        jMenuItemDownloadMediensammlung = new javax.swing.JMenuItem();
        jMenuItemDownloadInvertSelection = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator7 = new javax.swing.JPopupMenu.Separator();
        jMenuItemDownloadShutDown = new javax.swing.JMenuItem();
        jMenuAbos = new javax.swing.JMenu();
        jMenuItemAbosEinschalten = new javax.swing.JMenuItem();
        jMenuItemAbosAusschalten = new javax.swing.JMenuItem();
        jMenuItemAbosLoeschen = new javax.swing.JMenuItem();
        jMenuItemAbosAendern = new javax.swing.JMenuItem();
        jMenuItemAboNeu = new javax.swing.JMenuItem();
        jMenuItemAboInvertSelection = new javax.swing.JMenuItem();
        jMenuAnsicht = new javax.swing.JMenu();
        jCheckBoxMenuItemToolBar = new javax.swing.JCheckBoxMenuItem();
        cbkBeschreibung = new javax.swing.JCheckBoxMenuItem();
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

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);

        jPanelCont.setLayout(new java.awt.BorderLayout());

        jPanelInfo.setLayout(new java.awt.BorderLayout());
        jPanelCont.add(jPanelInfo, java.awt.BorderLayout.PAGE_END);

        jTabbedPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 1, 1, 1));
        jPanelCont.add(jTabbedPane, java.awt.BorderLayout.CENTER);

        jMenuDatei.setMnemonic('d');
        jMenuDatei.setText("Datei");

        jMenuItemFilmlisteLaden.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F5, 0));
        jMenuItemFilmlisteLaden.setText("neue Filmliste laden");
        jMenuDatei.add(jMenuItemFilmlisteLaden);

        jMenuItemEinstellungen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4, 0));
        jMenuItemEinstellungen.setText("Einstellungen");
        jMenuItemEinstellungen.setToolTipText("allgemeine Programmeinstellungen");
        jMenuDatei.add(jMenuItemEinstellungen);
        jMenuDatei.add(jSeparator2);

        jMenuItemBeenden.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Q, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemBeenden.setText("Beenden");
        jMenuDatei.add(jMenuItemBeenden);

        jMenuBar.add(jMenuDatei);

        jMenuFilme.setMnemonic('F');
        jMenuFilme.setText("Filme");

        jMenuItemFilmAbspielen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemFilmAbspielen.setText("Film abspielen");
        jMenuFilme.add(jMenuItemFilmAbspielen);

        jMenuItemFilmAufzeichnen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemFilmAufzeichnen.setText("Film aufzeichnen");
        jMenuFilme.add(jMenuItemFilmAufzeichnen);

        jMenuItemFilterLoeschen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemFilterLoeschen.setText("Filter löschen");
        jMenuFilme.add(jMenuItemFilterLoeschen);

        jMenuItemBlacklist.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_B, java.awt.event.InputEvent.CTRL_MASK));
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

        jMenuItemDownloadsAlleStarten.setText("alle Downloads starten");
        jMenuDownload.add(jMenuItemDownloadsAlleStarten);

        jMenuItemDownloadStartTime.setText("alle Downloads um xx:yy Uhr starten");
        jMenuDownload.add(jMenuItemDownloadStartTime);

        jMenuItemDownloadAlleStoppen.setText("alle stoppen");
        jMenuItemDownloadAlleStoppen.setToolTipText("alle Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadAlleStoppen);

        jMenuItemDownloadWartendeStoppen.setText("wartende stoppen");
        jMenuItemDownloadWartendeStoppen.setToolTipText("wartende Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadWartendeStoppen);

        jMenuItemDownloadsAktualisieren.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemDownloadsAktualisieren.setText("Liste der Downloads aktualisieren");
        jMenuDownload.add(jMenuItemDownloadsAktualisieren);

        jMenuItemDownloadsAufraeumen.setText("Liste der Downloads aufräumen");
        jMenuDownload.add(jMenuItemDownloadsAufraeumen);
        jMenuDownload.add(jSeparator3);

        jMenuItemDownloadStarten.setText("Downloads starten");
        jMenuDownload.add(jMenuItemDownloadStarten);

        jMenuItemDownloadStoppen.setText("Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadStoppen);

        jMenuItemDownloadVorziehen.setText("Downloads vorziehen");
        jMenuDownload.add(jMenuItemDownloadVorziehen);

        jMenuItemDownloadsZurueckstellen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_BACK_SPACE, 0));
        jMenuItemDownloadsZurueckstellen.setText("Downloads zurückstellen");
        jMenuDownload.add(jMenuItemDownloadsZurueckstellen);

        jMenuItemDownloadsLoeschen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_DELETE, 0));
        jMenuItemDownloadsLoeschen.setText("Downloads aus Liste entfernen");
        jMenuDownload.add(jMenuItemDownloadsLoeschen);

        jMenuItemDownloadAendern.setText("Download ändern");
        jMenuDownload.add(jMenuItemDownloadAendern);
        jMenuDownload.add(jSeparator1);

        jMenuItemDownloadGesehen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemDownloadGesehen.setText("Filme als gesehen markieren");
        jMenuDownload.add(jMenuItemDownloadGesehen);

        jMenuItemDownloadUngesehen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemDownloadUngesehen.setText("Filme als ungesehen markieren");
        jMenuDownload.add(jMenuItemDownloadUngesehen);

        jMenuItemDownloadAbspielen.setText("gespeicherten Film abspielen");
        jMenuDownload.add(jMenuItemDownloadAbspielen);

        jMenuItemDownloadMediensammlung.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M, java.awt.event.InputEvent.CTRL_MASK));
        jMenuItemDownloadMediensammlung.setText("Titel in der Mediensammlung suchen");
        jMenuDownload.add(jMenuItemDownloadMediensammlung);

        jMenuItemDownloadInvertSelection.setText("Auswahl umkehren");
        jMenuDownload.add(jMenuItemDownloadInvertSelection);
        jMenuDownload.add(jSeparator7);

        jMenuItemDownloadShutDown.setText("Rechner nach Downloads herunterfahren");
        jMenuDownload.add(jMenuItemDownloadShutDown);

        jMenuBar.add(jMenuDownload);

        jMenuAbos.setMnemonic('b');
        jMenuAbos.setText("Abo");

        jMenuItemAbosEinschalten.setText("einschalten");
        jMenuAbos.add(jMenuItemAbosEinschalten);

        jMenuItemAbosAusschalten.setText("ausschalten");
        jMenuAbos.add(jMenuItemAbosAusschalten);

        jMenuItemAbosLoeschen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_DELETE, 0));
        jMenuItemAbosLoeschen.setText("löschen");
        jMenuAbos.add(jMenuItemAbosLoeschen);

        jMenuItemAbosAendern.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, 0));
        jMenuItemAbosAendern.setText("ändern");
        jMenuAbos.add(jMenuItemAbosAendern);

        jMenuItemAboNeu.setText("neues Abo anlegen");
        jMenuAbos.add(jMenuItemAboNeu);

        jMenuItemAboInvertSelection.setText("Auswahl umkehren");
        jMenuAbos.add(jMenuItemAboInvertSelection);

        jMenuBar.add(jMenuAbos);

        jMenuAnsicht.setMnemonic('a');
        jMenuAnsicht.setText("Ansicht");

        jCheckBoxMenuItemToolBar.setSelected(true);
        jCheckBoxMenuItemToolBar.setText("Toolbar");
        jMenuAnsicht.add(jCheckBoxMenuItemToolBar);

        cbkBeschreibung.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F10, 0));
        cbkBeschreibung.setForeground(new java.awt.Color(0, 51, 153));
        cbkBeschreibung.setText("Beschreibung anzeigen");
        jMenuAnsicht.add(cbkBeschreibung);

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

        jMenuItemAnleitung.setText("Infos und Hilfe zum Programm");
        jMenuHilfe.add(jMenuItemAnleitung);

        jMenuBar.add(jMenuHilfe);

        setJMenuBar(jMenuBar);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelCont, javax.swing.GroupLayout.DEFAULT_SIZE, 1083, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addGap(6, 6, 6)
                .addComponent(jPanelCont, javax.swing.GroupLayout.DEFAULT_SIZE, 805, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBoxMenuItem cbBandwidthDisplay;
    protected javax.swing.JCheckBoxMenuItem cbkBeschreibung;
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
    private javax.swing.JMenuItem jMenuItemAboInvertSelection;
    private javax.swing.JMenuItem jMenuItemAboNeu;
    private javax.swing.JMenuItem jMenuItemAbosAendern;
    private javax.swing.JMenuItem jMenuItemAbosAusschalten;
    private javax.swing.JMenuItem jMenuItemAbosEinschalten;
    private javax.swing.JMenuItem jMenuItemAbosLoeschen;
    private javax.swing.JMenuItem jMenuItemAnleitung;
    protected javax.swing.JMenuItem jMenuItemBeenden;
    protected javax.swing.JMenuItem jMenuItemBlacklist;
    private javax.swing.JMenuItem jMenuItemDownloadAbspielen;
    private javax.swing.JMenuItem jMenuItemDownloadAendern;
    private javax.swing.JMenuItem jMenuItemDownloadAlleStoppen;
    private javax.swing.JMenuItem jMenuItemDownloadGesehen;
    private javax.swing.JMenuItem jMenuItemDownloadInvertSelection;
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
    private javax.swing.JPanel jPanelCont;
    private javax.swing.JPanel jPanelInfo;
    protected javax.swing.JPopupMenu.Separator jSeparator2;
    private javax.swing.JTabbedPane jTabbedPane;
    // End of variables declaration//GEN-END:variables

    private class MenuLST implements MenuListener {

        private TABS tabs;

        public MenuLST(TABS tabs) {
            this.tabs = tabs;
        }

        @Override
        public void menuSelected(MenuEvent e) {
            findTab(tabs);
        }

        @Override
        public void menuDeselected(MenuEvent e) {
        }

        @Override
        public void menuCanceled(MenuEvent e) {
        }

        private void findTab(TABS state) {
            switch (state) {
                case TAB_NIX:
                    break;
                case TAB_FILME:
                    setTabIfContain(Daten.guiFilme);
                    break;
                case TAB_DOWNLOADS:
                    setTabIfContain(Daten.guiDownloads);
                    break;
                case TAB_ABOS:
                    setTabIfContain(Daten.guiAbo);
                    break;
            }
        }

        private void setTabIfContain(Component check) {
            for (int i = 0; i < jTabbedPane.getTabCount(); ++i) {
                Component c = jTabbedPane.getComponentAt(i);
                if (c.equals(check)) {
                    jTabbedPane.setSelectedIndex(i);
                    return;
                }
            }
        }
    }

}
