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
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.*;
import mSearch.tool.Functions.OperatingSystemType;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.ProgStart;
import mediathek.controller.starter.Start;
import mediathek.daten.ListeDownloads;
import mediathek.gui.*;
import mediathek.gui.actions.ResetSettingsAction;
import mediathek.gui.actions.ShowAboutDialogAction;
import mediathek.gui.actions.ShowOnlineHelpAction;
import mediathek.gui.actions.WriteProtocolFileAction;
import mediathek.gui.bandwidth.IBandwidthMonitor;
import mediathek.gui.bandwidth.MVBandwidthMonitorLWin;
import mediathek.gui.dialog.DialogBeenden;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialog.DialogMediaDB;
import mediathek.gui.dialog.DialogStarteinstellungen;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.gui.filmInformation.IFilmInformation;
import mediathek.gui.filmInformation.MVFilmInformationLWin;
import mediathek.gui.tools.AutomaticTabSwitcherMenuListener;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVFont;
import mediathek.tool.MVFrame;
import mediathek.tool.MVMessageDialog;
import mediathek.update.CheckUpdate;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;

import static mSearch.tool.Functions.getOs;
import static mediathek.tool.MVFunctionSys.startMeldungen;

@SuppressWarnings("serial")
public class MediathekGui extends JFrame {

    private static final String TEXT_LINE = "==========================================";
    private static final String LOG_TEXT_STARTPARAMETER_PATTERN = "Startparameter: %s";
    private static final String ICON_NAME = "MediathekView.png";
    private static final String ICON_PATH = "/mediathek/res/";
    private static final int ICON_WIDTH = 58;
    private static final int ICON_HEIGHT = 58;
    private static final String KEY_F10 = "F10";
    private static final String NONE = "none";
    private static final String LOG_TEXT_PROGRAMMSTART = "***Programmstart***";
    private static final String SPLASHSCREEN_TEXT_ANWENDUNGSDATEN_LADEN = "Anwendungsdaten laden...";
    private static final String LOG_TEXT_START = "Start";
    private static final String SPLASHSCREEN_TEXT_GUI_INITIALISIEREN = "GUI Initialisieren...";
    private static final String LOG_TEXT_ERSTER_START = "Erster Start";
    private static final String LOG_TEXT_START_GUI = "Start Gui";
    private static final String LOG_TEXT_INIT_GUI = "Init GUI";
    private static final String LOG_TEXT_GUI_STEHT = "Gui steht!";
    private static final String ARGUMENT_PREFIX = "-";
    private static final String TITLE_TEXT_PROGRAMMVERSION_IST_AKTUELL = "Programmversion ist aktuell";
    private static final String TITLE_TEXT_EIN_PROGRAMMUPDATE_IST_VERFUEGBAR = "Ein Programmupdate ist verfügbar";
    private static final String LOG_TEXT_CHECK_UPDATE = "CheckUpdate";
    private static final String TABNAME_FILME = "Filme";
    private static final String TABNAME_DEBUG = "Debug";
    private static final String TABNAME_DOWNLOADS = "Downloads";
    private static final String TABNAME_ABOS = "Abos";
    private static final String TABNAME_MELDUNGEN = "Meldungen";
    private static final String LOG_TEXT_DIE_DOWNLOADS_MUESSEN_ZUERST_GESTARTET_WERDEN = "Die Downloads müssen zuerst gestartet werden.";
    private static final String LOG_TEXT_KEINE_LAUFENDEN_DOWNLOADS = "Keine laufenden Downloads!";
    private static final String DIALOG_TITLE_BLACKLIST = "Blacklist";
    private static final String PANEL_BLACKLIST_NAME_POSTFIX = "_2";
    private static final String CHECKBOX_TEXT_FILTER_ANZEIGEN = "Filter anzeigen";
    private static final String CHECKBOX_TEXT_DOWNLOADS_IN_EXTRAFENSTER = "Downloads in Extrafenster";
    private static final String CHECKBOX_TEXT_ABOS_IN_EXTRAFENSTER = "Abos in Extrafenster";
    private static final String CHECKBOX_TEXT_MELDUNGEN_ANZEIGEN = "Meldungen anzeigen";
    private static final String CHECKBOX_TEXT_IN_EXTRAFENSTER = "in Extrafenster";


    protected final Daten daten;
    private final SplashScreenManager splashScreenManager;
    private MVStatusBar statusBar;
    private MVFrame frameDownload;
    private MVFrame frameAbo;
    private MVFrame frameMeldungen;
    private final JCheckBoxMenuItem jCheckBoxFilterAnzeigen = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxFilterExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxDownloadExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxAboExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxMeldungenAnzeigen = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxMeldungenExtrafenster = new JCheckBoxMenuItem();
    private MVTray tray;
    private DialogEinstellungen dialogEinstellungen;

    public void updateSplashScreenText(final String aSplashScreenText)
    {
        splashScreenManager.updateSplashScreenText(aSplashScreenText);
    }

    public void closeSplashScreen()
    {
        splashScreenManager.closeSplashScreen();
    }

    public enum TABS {
        NIX, FILME, DOWNLOADS, ABOS, MELDUNGEN
    }

    /**
     * Bandwidth monitoring for downloads.
     */
    protected IBandwidthMonitor bandwidthMonitor;

    public MVStatusBar getStatusBar() {
        return statusBar;
    }

    private void disableF10Key() {
        //Hier wird F10 default Funktion unterbunden:
        InputMap im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(KeyStroke.getKeyStroke(KEY_F10), NONE);
    }

    public MediathekGui(String... aArguments) {
        super();

        splashScreenManager = new SplashScreenManager();
        splashScreenManager.initializeSplashScreen();

        initComponents();

        String pfad = readPfadFromArguments(aArguments);
        daten = Daten.getInstance(pfad, this);

        aboutAction = new ShowAboutDialogAction(this);
        settingsAction = new ShowSettingsAction();
        loadFilmlistAction = new LoadFilmListAction();


        Duration.counterStart(LOG_TEXT_PROGRAMMSTART);

        setIconImage(GetIcon.getIcon(ICON_NAME, ICON_PATH, ICON_WIDTH, ICON_HEIGHT).getImage());
        disableF10Key();

        splashScreenManager.updateSplashScreenText(SPLASHSCREEN_TEXT_ANWENDUNGSDATEN_LADEN);

        startMeldungen();
        Duration.staticPing(LOG_TEXT_START);

        loadDaten();

        Duration.staticPing(LOG_TEXT_START_GUI);
        createStatusBar();

        createFilmInformationHUD(jTabbedPane, daten);

        setOrgTitel();
        setLookAndFeel();
        init();
        restoreWindowState();
        Duration.staticPing(LOG_TEXT_INIT_GUI);
        initializeSettingsDialog();
        setupMediaDB();

        addListener();

        setFocusSuchfeld();

        createBandwidthMonitor(this);

        Duration.staticPing(LOG_TEXT_GUI_STEHT);

        ProgStart.loadDataProgStart();

        splashScreenManager.closeSplashScreen();
    }

    protected IFilmInformation filmInfo = null; // Infos zum Film

    public IFilmInformation getFilmInformationHud() {
        return filmInfo;
    }

    private void setupMediaDB()
    {
        daten.setDialogMediaDB(new DialogMediaDB(this));
        daten.getDialogMediaDB().setVis();
    }

    private void initializeSettingsDialog()
    {
        // Dialog mit den Programmeinstellungen einrichten
        dialogEinstellungen = new DialogEinstellungen(daten);
    }

    private void loadDaten()
    {
        if (daten.allesLaden()) {
            // alles geladen
            splashScreenManager.updateSplashScreenText(SPLASHSCREEN_TEXT_GUI_INITIALISIEREN);
        } else {
            Duration.staticPing(LOG_TEXT_ERSTER_START);
            // erster Start
            ReplaceList.init(); // einmal ein Muster anlegen, für Linux/OS X ist es bereits aktiv!
            new DialogStarteinstellungen(this, daten).setVisible(true);
            MVConfig.loadSystemParameter();
            this.pack();
        }
    }

    /**
     * Create the status bar item.
     */
    private void createStatusBar() {
        statusBar = new MVStatusBar();
        JScrollPane js = new JScrollPane();
        js.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
        js.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        js.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
        js.setViewportView(statusBar.getComponent());
        jPanelInfo.add(js, BorderLayout.CENTER);
    }

    private String readPfadFromArguments(final String[] aArguments) {
        String pfad;
        if (aArguments == null) {
            pfad = "";
        } else {
            printArguments(aArguments);
            if (aArguments.length > 0) {
                if (!aArguments[0].startsWith(ARGUMENT_PREFIX)) {
                    if (!aArguments[0].endsWith(File.separator)) {
                        aArguments[0] += File.separator;
                    }
                    pfad = aArguments[0];
                } else {
                    pfad = "";
                }
            } else {
                pfad = "";
            }
        }
        return pfad;
    }

    private void printArguments(final String[] aArguments)
    {
        SysMsg.sysMsg("");
        SysMsg.sysMsg(TEXT_LINE);
        for (String argument : aArguments) {
            SysMsg.sysMsg(String.format(LOG_TEXT_STARTPARAMETER_PATTERN, argument));
        }
        SysMsg.sysMsg(TEXT_LINE);
        SysMsg.sysMsg("");
    }

    protected void createBandwidthMonitor(JFrame parent)
    {
        //klappte nicht auf allen Desktops
        bandwidthMonitor = new MVBandwidthMonitorLWin(parent);
    }

    /**
     * Create the film information tool window.
     */
    protected void createFilmInformationHUD(JTabbedPane tabPane, Daten daten) {
            //klappte nicht auf allen Desktops
        filmInfo = new MVFilmInformationLWin(this);
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
                setTitle(TITLE_TEXT_PROGRAMMVERSION_IST_AKTUELL);
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitle(TITLE_TEXT_EIN_PROGRAMMUPDATE_IST_VERFUEGBAR);
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
                jCheckBoxMenuItemMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxFilterAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER)));
                jCheckBoxFilterExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_FILTER)));
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_TABS_TOP, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                designTabs();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_BANDWIDTH_MONITOR, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE)));
            }
        });
    }

    protected void setFocusSuchfeld() {
        Listener.notify(Listener.EREIGNIS_SUCHFELD_FOCUS_SETZEN, MediathekGui.class.getName());
    }

    /**
     * This will set the Look&Feel based on Application Preferences. In case of
     * error it will always reset to system LAF.
     */
    private void setLookAndFeel() {
        //FIXME not sure if this stuff will work with JDK9!
        try {
            String laf = MVConfig.get(MVConfig.Configs.SYSTEM_LOOK);
            //if we have the old values, reset to System LAF
            if (laf.isEmpty() || laf.length() == 1) {
                if (getOs() != OperatingSystemType.LINUX) {
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                }
            } else {
                //otherwise set the requested UI
                laf = MVConfig.get(MVConfig.Configs.SYSTEM_LOOK);
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
            MVConfig.add(MVConfig.Configs.SYSTEM_LOOK, UIManager.getSystemLookAndFeelClassName());
        }
    }

    private void setOrgTitel() {
        setTitle(Konstanten.PROGRAMMNAME + ' ' + Functions.getBuildNr());
    }

    private void setCbBeschreibung() {
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN))
                && Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN))) {
            //dann sind beide an
            cbkBeschreibung.setSelected(true);
            cbkBeschreibung.setForeground(null);
        } else if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN))
                || Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN))) {
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
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                jMenuItemDownloadsAktualisieren.setEnabled(false);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                jMenuItemDownloadsAktualisieren.setEnabled(true);
                daten.allesSpeichern(); // damit nichts verlorengeht
            }

            @Override
            public void fertigOnlyOne(ListenerFilmeLadenEvent event) {
                // Prüfen obs ein Programmupdate gibt
                Duration.staticPing(LOG_TEXT_CHECK_UPDATE);
                new CheckUpdate(daten.getMediathekGui(), daten).checkProgUpdate();
                daten.getListeMediaDB().loadSavedList();
                daten.getListeMediaDB().createMediaDB("");
            }
        });
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                if (tray != null && !SystemInfo.isMacOSX() && Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
                    daten.getMediathekGui().setVisible(false);
                } else {
                    beenden(false, false);
                }
            }
        });
        setTray();
    }

    public void setTray() {
        if (tray == null && Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
            tray = new MVTray().systemTray();
        } else if (tray != null && !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
            tray.beenden();
            tray = null;
        }
    }

    private static boolean geklickt;

    /**
     * Points to the GuiDebug panel instance.
     */
    public GuiDebug guiDebug = null;

    private void initTabs() {
        Daten.guiDownloads = new GuiDownloads(daten, this);
        Daten.guiAbo = new GuiAbo(daten, this);
        Daten.guiMeldungen = new GuiMeldungen(daten, this);
        Daten.guiFilme = new GuiFilme(daten, this);

        jTabbedPane.addTab(TABNAME_FILME, Daten.guiFilme);

        if (daten.isDebug()) {
            guiDebug = new GuiDebug(daten);
            jTabbedPane.addTab(TABNAME_DEBUG, guiDebug);
        }
        initFrames();
        jTabbedPane.addChangeListener(l -> {
            designTabs(); //damit das sel. Tab das richtige Icon bekommt
            if (!geklickt) {
                geklickt = true;
                Duration.counterStop(LOG_TEXT_PROGRAMMSTART);
            }
        });
    }

    public void hideFrame(TABS state) {
        switch (state) {
            case DOWNLOADS:
                jCheckBoxDownloadExtrafenster.setSelected(false);
                MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_DOWNLOAD, Boolean.toString(false));
                break;
            case ABOS:
                jCheckBoxAboExtrafenster.setSelected(false);
                MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_ABO, Boolean.toString(false));
                break;
            case MELDUNGEN:
                jCheckBoxMeldungenAnzeigen.setSelected(true);
                jCheckBoxMeldungenExtrafenster.setSelected(false);
                MVConfig.add(MVConfig.Configs.SYSTEM_VIS_MELDUNGEN, Boolean.toString(true));
                MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_MELDUNGEN, Boolean.toString(false));
                break;
        }
        initFrames();
    }

    private void initFrames() {
        // Downloads
        int nr = 1;
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_DOWNLOAD))) {
            frameDownload = setFrame(frameDownload, MVConfig.Configs.SYSTEM_GROESSE_DOWNLOAD, Daten.guiDownloads, TABS.DOWNLOADS);
        } else {
            setTab(frameDownload, Daten.guiDownloads, TABNAME_DOWNLOADS, nr++);
        }

        // Abos
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_ABO))) {
            frameAbo = setFrame(frameAbo, MVConfig.Configs.SYSTEM_GROESSE_ABO, Daten.guiAbo, TABS.ABOS);
        } else {
            setTab(frameAbo, Daten.guiAbo, TABNAME_ABOS, nr++);
        }

        // Meldungen
        if (!Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_MELDUNGEN))) {
            hide(frameMeldungen, Daten.guiMeldungen);
        } else if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_MELDUNGEN))) {
            frameMeldungen = setFrame(frameMeldungen, MVConfig.Configs.SYSTEM_GROESSE_MELDUNGEN, Daten.guiMeldungen, TABS.MELDUNGEN);
        } else {
            setTab(frameMeldungen, Daten.guiMeldungen, TABNAME_MELDUNGEN, nr);
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
        }
        if (tabContain(panelVorlage)) {
            jTabbedPane.remove(panelVorlage);
        }
    }

    private MVFrame setFrame(MVFrame frame, MVConfig.Configs nrGroesse, PanelVorlage panel, TABS sparte) {
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
        boolean top = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_TOP));
        boolean icon = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_ICON));
        if (top) {
            jTabbedPane.setTabPlacement(JTabbedPane.TOP);
        } else {
            jTabbedPane.setTabPlacement(JTabbedPane.LEFT);
        }

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
            if (c.equals(daten.getMediathekGui().guiDebug)) {
                if (jTabbedPane.getSelectedIndex() == i) {
                    ic = top ? Icons.ICON_TAB_TOP_MELDUNG : Icons.ICON_TAB_MELDUNG;
                } else {
                    ic = top ? Icons.ICON_TAB_TOP_MELDUNG_SW : Icons.ICON_TAB_MELDUNG_SW;
                }
            }
            String s = jTabbedPane.getTitleAt(i);
            JLabel lbl = makeTabbedPaneLabel(s, ic);
            if (icon) {
                jTabbedPane.setTabComponentAt(i, lbl);
            } else {
                jTabbedPane.setTabComponentAt(i, null);
            }
        }
    }

    /**
     * Create {@link JLabel} with an icon and text for the {@link JTabbedPane} header.
     *
     * @param text label text.
     * @param ic   provided icon.
     * @return the ready {@link JLabel}
     */
    private JLabel makeTabbedPaneLabel(String text, ImageIcon ic) {
        JLabel lbl = new JLabel(text);

        lbl.setBorder(null);
        lbl.setIcon(ic);
        lbl.setOpaque(false);

        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_TOP))) {
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

    /**
     * Add menu listeners which switch to the appropriate Tab when a menu is selected.
     */
    protected void installAutomaticTabSwitcher() {
        jMenuFilme.addMenuListener(new AutomaticTabSwitcherMenuListener(jTabbedPane, TABS.FILME));
        jMenuDownload.addMenuListener(new AutomaticTabSwitcherMenuListener(jTabbedPane, TABS.DOWNLOADS));
        jMenuAbos.addMenuListener(new AutomaticTabSwitcherMenuListener(jTabbedPane, TABS.ABOS));
    }

    protected void initMenue() {
        setCbBeschreibung();
        installAutomaticTabSwitcher();
        setMenuIcons();

        initializeDateiMenu();
        initializeFilmeMenu();
        initializeDownloadsMenu();
        initializeAboMenu();
        initializeAnsichtMenu();

        // Hilfe
        initializeHelpMenu();
    }

    private void initializeAnsichtFilter()
    {
        //Ansicht Filter
        jMenuAnsicht.add(new JSeparator());
        jCheckBoxFilterAnzeigen.setText(CHECKBOX_TEXT_FILTER_ANZEIGEN);
        jCheckBoxFilterExtrafenster.setText(CHECKBOX_TEXT_IN_EXTRAFENSTER);
        jCheckBoxFilterExtrafenster.setBorder(BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jMenuAnsicht.add(jCheckBoxFilterAnzeigen);
        jMenuAnsicht.add(jCheckBoxFilterExtrafenster);
        jCheckBoxFilterAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER)));
        jCheckBoxFilterAnzeigen.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_VIS_FILTER, Boolean.toString(jCheckBoxFilterAnzeigen.isSelected()));
            Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getSimpleName());
        });
        jCheckBoxFilterExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_FILTER)));
        jCheckBoxFilterExtrafenster.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_FILTER, Boolean.toString(jCheckBoxFilterExtrafenster.isSelected()));
            Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getSimpleName());
        });
    }

    private void initializeAnsichtMenu()
    {
        // Ansicht
        jCheckBoxMenuItemToolBar.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TOOLBAR_ALLES_ANZEIGEN)));
        jCheckBoxMenuItemToolBar.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_TOOLBAR_ALLES_ANZEIGEN, Boolean.toString(jCheckBoxMenuItemToolBar.isSelected()));
            Listener.notify(Listener.EREIGNIS_TOOLBAR_VIS, MediathekGui.class.getSimpleName());
        });
        jCheckBoxMenuItemVideoplayer.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
        jCheckBoxMenuItemVideoplayer.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, String.valueOf(jCheckBoxMenuItemVideoplayer.isSelected()));
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName());
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxMenuItemVideoplayer.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
            }
        });
        cbkBeschreibung.addActionListener(l -> {
            //Filme
            MVConfig.add(MVConfig.Configs.SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN, String.valueOf(cbkBeschreibung.isSelected()));
            Listener.notify(Listener.EREIGNIS_FILM_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName());
            //Downloads
            MVConfig.add(MVConfig.Configs.SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN, String.valueOf(cbkBeschreibung.isSelected()));
            Listener.notify(Listener.EREIGNIS_DOWNLOAD_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName());
            setCbBeschreibung();
        });

        jCheckBoxMenuItemMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
        jCheckBoxMenuItemMediaDb.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, String.valueOf(jCheckBoxMenuItemMediaDb.isSelected()));
            daten.getDialogMediaDB().setVis();
        });
        jMenuItemSchriftGr.addActionListener(e -> MVFont.setFontSize(true));
        jMenuItemSchriftKl.addActionListener(e -> MVFont.setFontSize(false));
        jMenuItemSchriftNormal.addActionListener(e -> MVFont.resetFontSize());

        initializeAnsichtFilter();
        initializeAnsichtDownloads();
        initializeAnsichtAbos();
        initializeAnsichtMeldungen();
    }

    private void initializeAnsichtMeldungen()
    {
        //Ansicht Meldungen
        jCheckBoxMeldungenAnzeigen.setText(CHECKBOX_TEXT_MELDUNGEN_ANZEIGEN);
        jCheckBoxMeldungenAnzeigen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_MELDUNGEN)));
        jCheckBoxMeldungenAnzeigen.addActionListener(e -> {
            if (!jCheckBoxMeldungenAnzeigen.isSelected()) {
                jCheckBoxMeldungenExtrafenster.setSelected(false);
            }
            MVConfig.add(MVConfig.Configs.SYSTEM_VIS_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenAnzeigen.isSelected()));
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenExtrafenster.isSelected()));
            initFrames();
        });
        jCheckBoxMeldungenExtrafenster.setText(CHECKBOX_TEXT_IN_EXTRAFENSTER);
        jCheckBoxMeldungenExtrafenster.setBorder(BorderFactory.createEmptyBorder(1, 10, 5, 1));
        jCheckBoxMeldungenExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_MELDUNGEN)));
        jCheckBoxMeldungenExtrafenster.addActionListener(e -> {
            if (jCheckBoxMeldungenExtrafenster.isSelected()) {
                jCheckBoxMeldungenAnzeigen.setSelected(true);
            }
            MVConfig.add(MVConfig.Configs.SYSTEM_VIS_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenAnzeigen.isSelected()));
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenExtrafenster.isSelected()));
            initFrames();
        });
        jMenuAnsicht.add(jCheckBoxMeldungenAnzeigen);
        jMenuAnsicht.add(jCheckBoxMeldungenExtrafenster);

        cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE)));
        cbBandwidthDisplay.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(cbBandwidthDisplay.isSelected()));
            Listener.notify(Listener.EREIGNIS_BANDWIDTH_MONITOR, MediathekGui.class.getSimpleName());
        });
    }

    protected void installAboutMenuItem() {
        jMenuHilfe.add(new JSeparator());
        jMenuHilfe.add(aboutAction);
    }

    protected void initializeHelpMenu()
    {
        jMenuItemShowOnlineHelp.setAction(new ShowOnlineHelpAction(this));
        jMenuItemCreateProtocolFile.setAction(new WriteProtocolFileAction(this));
        jMenuItemResetSettings.setAction(new ResetSettingsAction(this, daten));
        installAboutMenuItem();
    }

    private void initializeAnsichtAbos()
    {
        //Ansicht Abos
        jCheckBoxAboExtrafenster.setText(CHECKBOX_TEXT_ABOS_IN_EXTRAFENSTER);
        jMenuAnsicht.add(jCheckBoxAboExtrafenster);
        jCheckBoxAboExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_ABO)));
        jCheckBoxAboExtrafenster.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_ABO, Boolean.toString(jCheckBoxAboExtrafenster.isSelected()));
            initFrames();
        });
    }

    protected Action aboutAction = null;
    protected Action settingsAction = null;
    public Action loadFilmlistAction = null;

    private void initializeAnsichtDownloads()
    {
        jMenuAnsicht.add(new JSeparator());

        jCheckBoxDownloadExtrafenster.setText(CHECKBOX_TEXT_DOWNLOADS_IN_EXTRAFENSTER);
        jMenuAnsicht.add(jCheckBoxDownloadExtrafenster);
        jCheckBoxDownloadExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_DOWNLOAD)));
        jCheckBoxDownloadExtrafenster.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_DOWNLOAD, Boolean.toString(jCheckBoxDownloadExtrafenster.isSelected()));
            initFrames();
        });
    }

    private void initializeAboMenu()
    {
        // Abo
        jMenuItemAbosEinschalten.addActionListener(e -> Daten.guiAbo.einAus(true));
        jMenuItemAbosAusschalten.addActionListener(e -> Daten.guiAbo.einAus(false));
        jMenuItemAbosLoeschen.addActionListener(e -> Daten.guiAbo.loeschen());
        jMenuItemAbosAendern.addActionListener(e -> Daten.guiAbo.aendern());
        jMenuItemAboNeu.addActionListener(e -> Daten.guiAbo.neu());
        jMenuItemAboInvertSelection.addActionListener(e -> Daten.guiAbo.invertSelection());
    }

    private void initializeDownloadsMenu()
    {
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
            if (daten.getListeDownloads().nochNichtFertigeDownloads() > 0) {
                // ansonsten gibts keine laufenden Downloads auf die man warten sollte
                beenden(true /*Dialog auf "warten" einstellen*/, false /*shutdown computer*/);
            } else {
                MVMessageDialog.showMessageDialog(daten.getMediathekGui(), LOG_TEXT_DIE_DOWNLOADS_MUESSEN_ZUERST_GESTARTET_WERDEN,
                        LOG_TEXT_KEINE_LAUFENDEN_DOWNLOADS, JOptionPane.ERROR_MESSAGE);
            }
        });
        jMenuItemDownloadGesehen.addActionListener(e -> Daten.guiDownloads.filmGesehen());
        jMenuItemDownloadUngesehen.addActionListener(e -> Daten.guiDownloads.filmUngesehen());
        jMenuItemDownloadMediensammlung.addActionListener(e -> Daten.guiDownloads.guiFilmMediensammlung());
        jMenuItemDownloadInvertSelection.addActionListener(e -> Daten.guiDownloads.invertSelection());
    }

    private void initializeFilmeMenu()
    {
        jMenuItemFilmAbspielen.addActionListener(e -> Daten.guiFilme.guiFilmeFilmAbspielen());
        jMenuItemFilmAufzeichnen.addActionListener(e -> Daten.guiFilme.guiFilmeFilmSpeichern());
        jMenuItemFilterLoeschen.addActionListener(e -> Daten.guiFilme.guiFilmeFilterLoeschen());
        jMenuItemBlacklist.addActionListener(e -> {
            DialogLeer dialog = new DialogLeer(daten.getMediathekGui(), true);
            dialog.init(DIALOG_TITLE_BLACKLIST, new PanelBlacklist(daten, daten.getMediathekGui(), PanelBlacklist.class.getName() + PANEL_BLACKLIST_NAME_POSTFIX));
            dialog.setVisible(true);
        });
        jMenuItemFilmeGesehen.addActionListener(e -> Daten.guiFilme.filmGesehen());
        jMenuItemFilmeUngesehen.addActionListener(e -> Daten.guiFilme.filmUngesehen());
        jMenuItemFilmeMediensammlung.addActionListener(e -> Daten.guiFilme.guiFilmMediensammlung());
    }

    protected void installSettingsAndTerminateMenuItems()
    {
        jMenuDatei.add(settingsAction);
        jMenuDatei.add(new JSeparator());
        jMenuDatei.add(new TerminateApplicationAction());
    }

    private void initializeDateiMenu() {
        jMenuItemFilmlisteLaden.setAction(loadFilmlistAction);

        installSettingsAndTerminateMenuItems();
    }

    class ShowSettingsAction extends AbstractAction {

        public ShowSettingsAction() {
            super("Einstellungen...", Icons.ICON_MENUE_EINSTELLUNGEN);
            putValue(SHORT_DESCRIPTION, "Allgemeine Programmeinstellungen");
            putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            showSettingsDialog();
        }
    }

    public void showSettingsDialog()
    {
        dialogEinstellungen.setVisible(true);
    }

    class TerminateApplicationAction extends AbstractAction {
        public TerminateApplicationAction() {
            super("Beenden", Icons.ICON_MENUE_BEENDEN);
            putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_MASK));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            beenden(false, false);
        }
    }

    class LoadFilmListAction extends AbstractAction {
        public LoadFilmListAction() {
            super("Neue Filmliste laden...", Icons.ICON_MENUE_FILMLISTE_LADEN);
            putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0));
            putValue(SHORT_DESCRIPTION, "Lade eine neue Filmliste");

            daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
                @Override
                public void start(ListenerFilmeLadenEvent event) {
                    setEnabled(false);
                }

                @Override
                public void fertig(ListenerFilmeLadenEvent event) {
                    setEnabled(true);
                }
            });

        }

        @Override
        public void actionPerformed(ActionEvent e) {
            daten.getFilmeLaden().loadFilmlistDialog(daten, false);
        }
    }

    /**
     * Set the icons for menu items.
     */
    private void setMenuIcons() {
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
    }

    private void stopAllDownloads() {
        final ListeDownloads downloadList = daten.getListeDownloads();
        downloadList.forEach(dl -> {
            Start s = dl.start;
            if (s != null)
                s.stoppen = true;
        });
    }

    private void saveWindowState() {
        if (getExtendedState() == JFrame.MAXIMIZED_BOTH) {
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_MAX, Boolean.TRUE.toString());
        } else {
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_MAX, Boolean.FALSE.toString());
        }
    }

    private void restoreWindowState() {
        if (Daten.isStartMaximized() || Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_MAX))) {
            setExtendedState(Frame.MAXIMIZED_BOTH);
        } else {
            GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_GUI, this, null);
        }
    }

    private void writeWindowSizes() {
        // Hauptfenster
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_GUI, this);
        // Dialog Einstellungen
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_EINSTELLUNGEN, dialogEinstellungen);
        // Infodialog/Bandwidth
        bandwidthMonitor.writeConfig();
        // MediaDB
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_GROESSE, daten.getDialogMediaDB());

        // Frames
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_DOWNLOAD, frameDownload);
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_ABO, frameAbo);
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_MELDUNGEN, frameMeldungen);

        // FilterFrame
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_FILTER, Daten.guiFilme.mVFilterFrame);
    }

    private void writeTabSettings() {
        // Tabelleneinstellungen merken
        daten.guiFilme.tabelleSpeichern();
        daten.guiDownloads.tabelleSpeichern();
        daten.guiAbo.tabelleSpeichern();
        daten.getDialogMediaDB().tabelleSpeichern();
    }

    /**
     * Quit the MediathekView application
     *
     * @param showOptionTerminate show options dialog when downloads are running
     * @param shutDown            try to shutdown the computer if requested
     * @return true if the user wants to terminate
     */
    public boolean beenden(boolean showOptionTerminate, boolean shutDown) {
        if (daten.getListeDownloads().nochNichtFertigeDownloads() > 0) {
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

        writeTabSettings();

        stopAllDownloads();

        saveWindowState();

        writeWindowSizes();

        daten.allesSpeichern();

        Log.endMsg();
        Duration.printCounter();

        if (shutDown)
            shutdownComputer();

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
                strShutdownCommand = MVConfig.get(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN);
                if (strShutdownCommand.isEmpty()) {
                    // sicherheitshalber
                    strShutdownCommand = Konstanten.SHUTDOWN_LINUX;
                    MVConfig.add(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN, Konstanten.SHUTDOWN_LINUX);
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

        javax.swing.JPanel jPanelCont = new javax.swing.JPanel();
        jPanelInfo = new javax.swing.JPanel();
        jTabbedPane = new javax.swing.JTabbedPane();
        jMenuBar = new javax.swing.JMenuBar();
        jMenuDatei = new javax.swing.JMenu();
        jMenuItemFilmlisteLaden = new javax.swing.JMenuItem();
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
        jMenuItemShowOnlineHelp = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator4 = new javax.swing.JPopupMenu.Separator();
        jMenuItemCreateProtocolFile = new javax.swing.JMenuItem();
        jMenuItemResetSettings = new javax.swing.JMenuItem();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);

        jPanelCont.setLayout(new java.awt.BorderLayout());

        jPanelInfo.setLayout(new java.awt.BorderLayout());
        jPanelCont.add(jPanelInfo, java.awt.BorderLayout.PAGE_END);
        jPanelCont.add(jTabbedPane, java.awt.BorderLayout.CENTER);

        getContentPane().add(jPanelCont, java.awt.BorderLayout.CENTER);

        jMenuDatei.setMnemonic('d');
        jMenuDatei.setText("Datei");

        jMenuItemFilmlisteLaden.setText("Neue Filmliste laden");
        jMenuDatei.add(jMenuItemFilmlisteLaden);

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
        jMenuDownload.setText("Downloads");

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
        jMenuAbos.setText("Abos");

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

        jMenuItemShowOnlineHelp.setText("Online-Hilfe anzeigen");
        jMenuHilfe.add(jMenuItemShowOnlineHelp);
        jMenuHilfe.add(jSeparator4);

        jMenuItemCreateProtocolFile.setText("Protokolldatei erstellen...");
        jMenuHilfe.add(jMenuItemCreateProtocolFile);

        jMenuItemResetSettings.setText("Einstellungen zurücksetzen...");
        jMenuHilfe.add(jMenuItemResetSettings);

        jMenuBar.add(jMenuHilfe);

        setJMenuBar(jMenuBar);

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
    protected javax.swing.JMenuItem jMenuItemBlacklist;
    private javax.swing.JMenuItem jMenuItemCreateProtocolFile;
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
    protected javax.swing.JMenuItem jMenuItemFilmAbspielen;
    protected javax.swing.JMenuItem jMenuItemFilmAufzeichnen;
    private javax.swing.JMenuItem jMenuItemFilmeGesehen;
    private javax.swing.JMenuItem jMenuItemFilmeMediensammlung;
    private javax.swing.JMenuItem jMenuItemFilmeUngesehen;
    private javax.swing.JMenuItem jMenuItemFilmlisteLaden;
    protected javax.swing.JMenuItem jMenuItemFilterLoeschen;
    protected javax.swing.JMenuItem jMenuItemResetSettings;
    private javax.swing.JMenuItem jMenuItemSchriftGr;
    private javax.swing.JMenuItem jMenuItemSchriftKl;
    private javax.swing.JMenuItem jMenuItemSchriftNormal;
    private javax.swing.JMenuItem jMenuItemShowOnlineHelp;
    private javax.swing.JPanel jPanelInfo;
    private javax.swing.JTabbedPane jTabbedPane;
    // End of variables declaration//GEN-END:variables


}
