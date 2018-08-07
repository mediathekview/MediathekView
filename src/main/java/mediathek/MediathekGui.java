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
import javafx.application.Platform;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.embed.swing.JFXPanel;
import javafx.event.Event;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ProgressIndicator;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import mSearch.Config;
import mSearch.daten.DatenFilm;
import mSearch.daten.PooledDatabaseConnection;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.FilmlistenSuchen;
import mSearch.filmlisten.writer.FilmListWriter;
import mSearch.tool.*;
import mSearch.tool.Functions.OperatingSystemType;
import mSearch.tool.javafx.FXErrorDialog;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.ListeMediaDB;
import mediathek.filmlisten.FilmeLaden;
import mediathek.gui.*;
import mediathek.gui.bandwidth.IBandwidthMonitor;
import mediathek.gui.bandwidth.MVBandwidthMonitorLWin;
import mediathek.gui.dialog.*;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.gui.filmInformation.InfoDialog;
import mediathek.gui.messages.*;
import mediathek.javafx.LivestreamTab;
import mediathek.javafx.MemoryMonitor;
import mediathek.javafx.StartupProgressPanel;
import mediathek.javafx.StatusBarController;
import mediathek.res.GetIcon;
import mediathek.tool.*;
import mediathek.tool.threads.IndicatorThread;
import mediathek.tool.threads.UIFilmlistLoaderThread;
import mediathek.update.CheckUpdate;
import mediathek.update.ProgrammUpdateSuchen;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.tbee.javafx.scene.layout.MigPane;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.HashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static mSearch.tool.Functions.getOs;
import static mediathek.tool.MVFunctionSys.startMeldungen;

@SuppressWarnings("serial")
public class MediathekGui extends JFrame {

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
    private static final String TITLE_TEXT_EIN_PROGRAMMUPDATE_IST_VERFUEGBAR = "Ein Programmupdate ist verfügbar";
    private static final String TABNAME_FILME = "Filme";
    private static final String TABNAME_DOWNLOADS = "Downloads";
    private static final String TABNAME_ABOS = "Abos";
    private static final String TABNAME_MELDUNGEN = "Meldungen";
    private static final String LOG_TEXT_DIE_DOWNLOADS_MUESSEN_ZUERST_GESTARTET_WERDEN = "Die Downloads müssen zuerst gestartet werden.";
    private static final String LOG_TEXT_KEINE_LAUFENDEN_DOWNLOADS = "Keine laufenden Downloads!";
    private static final String DIALOG_TITLE_BLACKLIST = "Blacklist";
    private static final String PANEL_BLACKLIST_NAME_POSTFIX = "_2";
    //private static final String CHECKBOX_TEXT_DOWNLOADS_IN_EXTRAFENSTER = "Downloads in Extrafenster";
    //private static final String CHECKBOX_TEXT_ABOS_IN_EXTRAFENSTER = "Abos in Extrafenster";
    private static final String CHECKBOX_TEXT_MELDUNGEN_ANZEIGEN = "Meldungen anzeigen";


    private final Daten daten;
    private final SplashScreenManager splashScreenManager;
    private MVFrame frameDownload;
    private MVFrame frameAbo;
    //private final JCheckBoxMenuItem jCheckBoxDownloadExtrafenster = new JCheckBoxMenuItem();
    //private final JCheckBoxMenuItem jCheckBoxAboExtrafenster = new JCheckBoxMenuItem();
    private final JCheckBoxMenuItem jCheckBoxMeldungenAnzeigen = new JCheckBoxMenuItem();
    private MVTray tray;
    private DialogEinstellungen dialogEinstellungen;
    private final MVSenderIconCache senderIconCache;

    public void updateSplashScreenText(final String aSplashScreenText)
    {
        splashScreenManager.updateSplashScreenText(aSplashScreenText);
    }

    public void closeSplashScreen()
    {
        splashScreenManager.closeSplashScreen();
    }

    public enum TABS {
        TAB_NIX, TAB_FILME, TAB_DOWNLOADS, TAB_ABOS, TAB_MELDUNGEN
    }

    /**
     * Bandwidth monitoring for downloads.
     */
    protected IBandwidthMonitor bandwidthMonitor;

    private void remapF10Key() {
        //Hier wird F10 default Funktion unterbunden:
        InputMap im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(KeyStroke.getKeyStroke(KEY_F10), NONE);
    }

    public MVSenderIconCache getSenderIconCache() {
        return senderIconCache;
    }

    public MediathekGui(String... aArguments) {
        super();

        splashScreenManager = new SplashScreenManager();
        splashScreenManager.initializeSplashScreen();

        initComponents();
        String pfad = readPfadFromArguments(aArguments);

        Duration.counterStart(LOG_TEXT_PROGRAMMSTART);

        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE); // soll abgefangen werden
        setIconImage(GetIcon.getIcon(ICON_NAME, ICON_PATH, ICON_WIDTH, ICON_HEIGHT).getImage());

        senderIconCache = new MVSenderIconCache();

        remapF10Key();

        splashScreenManager.updateSplashScreenText(SPLASHSCREEN_TEXT_ANWENDUNGSDATEN_LADEN);

        daten = Daten.getInstance(pfad,this);

        startMeldungen();
        Duration.staticPing(LOG_TEXT_START);

        loadDaten();

        Duration.staticPing(LOG_TEXT_START_GUI);
        createStatusBar();

        createFilmInformationHUD();

        setOrgTitel();
        setLookAndFeel();
        init();
        setSize();
        Duration.staticPing(LOG_TEXT_INIT_GUI);
        initializeSettingsDialog();


        addListener();
        setupSearchKeyForMac();

        //register message bus handler
        daten.getMessageBus().subscribe(this);

        setFocusOnSearchField();

        createMemoryMonitor();

        if (Config.isDebuggingEnabled())
            Platform.runLater(() -> memoryMonitor.show());

        createBandwidthMonitor(this);

        Duration.staticPing(LOG_TEXT_GUI_STEHT);

        splashScreenManager.closeSplashScreen();

        loadFilmlist();
    }

    /**
     * Memory display for debugging purposes.
     * Only visible when debug mode is enabled
     */
    private MemoryMonitor memoryMonitor;

    private void createMemoryMonitor() {
        Platform.runLater(() -> memoryMonitor = new MemoryMonitor());
    }

    private void loadFilmlist() {
        Thread programStart = new UIFilmlistLoaderThread();
        programStart.start();
    }

    @Handler
    protected void handleFilmlistReadStartEvent(FilmListReadStartEvent msg) {
        //do not use javafx in low mem environment...
        if (!MemoryUtils.isLowMemoryEnvironment()) {
            SwingUtilities.invokeLater(() -> {
                //activate glass pane
                panel = new StartupProgressPanel();
                setGlassPane(panel);
                getGlassPane().setVisible(true);
            });
        }
    }

    private StartupProgressPanel panel = null;

    @Handler
    protected void handleFilmlistReadStopEvent(FilmListReadStopEvent msg) {
        if (!MemoryUtils.isLowMemoryEnvironment()) {
            //set to complete and wait a little bit...
            Platform.runLater(() -> {
                if (panel != null)
                    panel.increaseProgress(1.0);
            });

            SwingUtilities.invokeLater(() -> {
                //deactivate glass pane
                getGlassPane().setVisible(false);

                //reset the glass pane to free memory
                setGlassPane(new JPanel());
                panel = null;

                //save the filmlist size for next time
                config.setProperty(StartupProgressPanel.CONFIG_STRING, daten.getListeFilme().size());
            });
        }
    }

    /**
     * Setup the keyboard for search field on macOS.
     * Unused on other platforms.
     */
    protected void setupSearchKeyForMac()
    {
    }

    private void initializeSettingsDialog()
    {
        // Dialog mit den Programmeinstellungen einrichten
        dialogEinstellungen = new DialogEinstellungen(daten);
        daten.setDialogMediaDB(new DialogMediaDB(this));
        daten.getDialogMediaDB().setVis();
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

    private StatusBarController statusBarController;

    /**
     * this property keeps track how many items are currently selected in the active table view
     */
    private final IntegerProperty selectedItemsProperty = new SimpleIntegerProperty(0);

    public IntegerProperty getSelectedItemsProperty() {
        return selectedItemsProperty;
    }

    /**
     * Create the status bar item.
     */
    private void createStatusBar() {
        JFXPanel statusBarPanel = new JFXPanel();
        statusBarController = new StatusBarController(daten, memoryMonitor, selectedItemsProperty);
        statusBarController.installStatusBar(statusBarPanel);

        jPanelInfo.add(statusBarPanel, BorderLayout.CENTER);
    }

    public enum TabPaneIndex {
        NONE, FILME, DOWNLOAD, ABO
    }

    /**
     * Helper to determine what tab is currently active
     */
    private ObjectProperty<TabPaneIndex> tabPaneIndexProperty = new SimpleObjectProperty<>(TabPaneIndex.NONE);

    public ObjectProperty<TabPaneIndex> tabPaneIndexProperty() {
        return tabPaneIndexProperty;
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
        for (String argument : aArguments) {
            logger.info("Startparameter: {}", argument);
        }
    }

    private static final Logger logger = LogManager.getLogger(MediathekGui.class);

    protected void createBandwidthMonitor(JFrame parent)
    {
        //klappte nicht auf allen Desktops
        bandwidthMonitor = new MVBandwidthMonitorLWin(parent);
    }

    /**
     * Create the film information tool window.
     */
    private void createFilmInformationHUD() {
        Daten.filmInfo = new InfoDialog(this, senderIconCache);
    }

    private void addListener() {
        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitle(TITLE_TEXT_EIN_PROGRAMMUPDATE_IST_VERFUEGBAR);
                // after ten seconds, restore the original window title
                Timer restoreTitleTimer = new Timer((int) TimeUnit.MILLISECONDS.convert(10, TimeUnit.SECONDS),
                        e -> setOrgTitel());
                restoreTitleTimer.setRepeats(false); //run once only...
                restoreTitleTimer.start();
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

    protected void setFocusOnSearchField() {
        Listener.notify(Listener.EREIGNIS_SUCHFELD_FOCUS_SETZEN, MediathekGui.class.getName());
    }

    /**
     * This will set the Look&Feel based on Application Preferences. In case of
     * error it will always reset to system LAF.
     */
    private void setLookAndFeel() {
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
        setTitle(Konstanten.PROGRAMMNAME + ' ' + Konstanten.MVVERSION);
    }

    private void setSize() {
        if (Daten.isStartMaximized() || Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_MAX))) {
            this.setExtendedState(Frame.MAXIMIZED_BOTH);
        } else {
            GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_GUI, this, null);
        }
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
            }

            @Override
            public void fertigOnlyOne(ListenerFilmeLadenEvent event) {
                setupUpdateCheck();
                prepareMediaDb();
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

    private void prepareMediaDb() {
        final ListeMediaDB mediaDb = daten.getListeMediaDB();
        mediaDb.loadSavedList();
        mediaDb.createMediaDB("");
    }

    /**
     * 24 hour timer for repeating update checks
     */
    private Timer updateCheckTimer;

    /**
     * This will setup a repeating update check every 24 hours.
     */
    private void setupUpdateCheck() {
        updateCheckTimer = new Timer(500, e -> {
            // Prüfen obs ein Programmupdate gibt
            new CheckUpdate(daten.getMediathekGui(), daten).start();
        });
        updateCheckTimer.setRepeats(true);
        updateCheckTimer.setDelay((int) TimeUnit.MILLISECONDS.convert(24, TimeUnit.HOURS));
        updateCheckTimer.start();
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

    private LivestreamTab livestreamTab;

    private void initTabs() {
        Daten.guiDownloads = new GuiDownloads(daten, this);
        Daten.guiAbo = new GuiAbo(daten, this);
        Daten.guiMeldungen = new GuiMeldungen(daten, this);
        Daten.guiFilme = new GuiFilme(daten, this);

        jTabbedPane.addTab(TABNAME_FILME, Daten.guiFilme);

        if (Config.isDebuggingEnabled()) {
            livestreamTab = new LivestreamTab(daten.getLivestreamList());
            jTabbedPane.addTab("Livestreams", livestreamTab);
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

//    public void hideFrame(TABS state) {
//        switch (state) {
//            case TAB_DOWNLOADS:
//                jCheckBoxDownloadExtrafenster.setSelected(false);
//                MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_DOWNLOAD, Boolean.toString(false));
//                break;
//            case TAB_ABOS:
//                jCheckBoxAboExtrafenster.setSelected(false);
//                MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_ABO, Boolean.toString(false));
//                break;
//            case TAB_MELDUNGEN:
//                jCheckBoxMeldungenAnzeigen.setSelected(true);
//                MVConfig.add(MVConfig.Configs.SYSTEM_VIS_MELDUNGEN, Boolean.toString(true));
//                break;
//        }
//        initFrames();
//    }

    /**
     * Enable/Disable the update related menu item.
     *
     * @param enable Shall the menu item be enabled?
     */
    public void enableUpdateMenuItem(boolean enable) {
        miSearchForProgramUpdate.setEnabled(enable);
    }

//    private void initAboFrame() {
//        // Abos
//        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_ABO))) {
//            frameAbo = setFrame(frameAbo, MVConfig.Configs.SYSTEM_GROESSE_ABO, Daten.guiAbo, TABS.TAB_ABOS);
//        } else {
//            setTab(frameAbo, Daten.guiAbo, TABNAME_ABOS, 2);
//        }
//    }

//    private void initDownloadFrame() {
//        // Downloads
//        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_DOWNLOAD))) {
//            frameDownload = setFrame(frameDownload, MVConfig.Configs.SYSTEM_GROESSE_DOWNLOAD, Daten.guiDownloads, TABS.TAB_DOWNLOADS);
//        } else {
//            setTab(frameDownload, Daten.guiDownloads, TABNAME_DOWNLOADS, 1);
//        }
//    }

    private void showOrHideMeldungenTab() {
        final boolean visible = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_MELDUNGEN));
        if (visible) {
            setTab(null, Daten.guiMeldungen, TABNAME_MELDUNGEN, 3);
            designTabs();
        } else {
            hide(null, Daten.guiMeldungen);
        }
    }

    private void initFrames() {
        setTab(frameDownload, Daten.guiDownloads, TABNAME_DOWNLOADS, 1);
        setTab(frameAbo, Daten.guiAbo, TABNAME_ABOS, 2);
        showOrHideMeldungenTab();

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
//            jTabbedPane.updateUI();
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

            String s = jTabbedPane.getTitleAt(i);
            JLabel lbl = makeLable(s, ic);
            if (icon) {
                jTabbedPane.setTabComponentAt(i, lbl);
            } else {
                jTabbedPane.setTabComponentAt(i, null);
            }
        }

//            jTabbedPane.updateUI();
    }

    private JLabel makeLable(String text, ImageIcon ic) {
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
     * Number of active downloads
     */
    protected final AtomicInteger numDownloadsStarted = new AtomicInteger(0);

    /**
     * Progress indicator thread for OS X and windows.
     */
    private IndicatorThread progressIndicatorThread = null;

    /**
     * Create the platform-specific instance of the progress indicator thread.
     *
     * @return {@link IndicatorThread} instance for the running platform.
     */
    protected IndicatorThread createProgressIndicatorThread() throws Exception {
        throw new Exception("Unsupported Platform");
    }

    /**
     * Message bus handler which gets called when a download is started.
     *
     * @param msg Information about the download
     */
    @Handler
    protected void handleDownloadStart(DownloadStartEvent msg) {
        numDownloadsStarted.incrementAndGet();

        if (progressIndicatorThread == null) {
            try {
                progressIndicatorThread = createProgressIndicatorThread();
                progressIndicatorThread.start();
            } catch (Exception ignored) {
                //ignore if we have an unsupported platform, ie. linux.
            }
        }
    }

    /**
     * Message bus handler which gets called when a download is stopped.
     *
     * @param msg Information about the download
     */
    @Handler
    protected void handleDownloadFinishedEvent(DownloadFinishedEvent msg) {
        final int numDL = numDownloadsStarted.decrementAndGet();

        if (numDL == 0 && progressIndicatorThread != null) {
            progressIndicatorThread.interrupt();
            progressIndicatorThread = null;
        }
    }

    private final HashMap<JMenu, MenuLST> menuListeners = new HashMap<>();

    /**
     * Install the listeners which will cause automatic tab switching based on associated Menu item.
     */
    protected void installMenuTabSwitchListener() {
        //initial setup
        menuListeners.put(jMenuFilme, new MenuLST(TABS.TAB_FILME));
        menuListeners.put(jMenuDownload, new MenuLST(TABS.TAB_DOWNLOADS));
        menuListeners.put(jMenuAbos, new MenuLST(TABS.TAB_ABOS));

        //now assign if really necessary
        if (config.getBoolean(ApplicationConfiguration.APPLICATION_INSTALL_TAB_SWITCH_LISTENER, true)) {
            jMenuFilme.addMenuListener(menuListeners.get(jMenuFilme));
            jMenuDownload.addMenuListener(menuListeners.get(jMenuDownload));
            jMenuAbos.addMenuListener(menuListeners.get(jMenuAbos));
        }
    }

    /**
     * Handle the install/or remove event sent from settings dialog
     *
     */
    @Handler
    protected void handleInstallTabSwitchListenerEvent(InstallTabSwitchListenerEvent msg) {
        switch (msg.event) {
            case INSTALL:
                SwingUtilities.invokeLater(() -> {
                    jMenuFilme.addMenuListener(menuListeners.get(jMenuFilme));
                    jMenuDownload.addMenuListener(menuListeners.get(jMenuDownload));
                    jMenuAbos.addMenuListener(menuListeners.get(jMenuAbos));
                });
                break;

            case REMOVE:
                SwingUtilities.invokeLater(() -> {
                    jMenuFilme.removeMenuListener(menuListeners.get(jMenuFilme));
                    jMenuDownload.removeMenuListener(menuListeners.get(jMenuDownload));
                    jMenuAbos.removeMenuListener(menuListeners.get(jMenuAbos));
                });
                break;
        }
    }

    protected void initMenue() {
        setCbBeschreibung();
        installMenuTabSwitchListener();

        setMenuIcons();

        initializeDateiMenu();
        initializeFilmeMenu();
        initializeDownloadsMenu();
        initializeAboMenu();
        initializeAnsichtMenu();

        // Hilfe
        setupHelpMenu();
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
            MVConfig.add(MVConfig.Configs.SYSTEM_VIS_MELDUNGEN, Boolean.toString(jCheckBoxMeldungenAnzeigen.isSelected()));
            showOrHideMeldungenTab();
        });
        jMenuAnsicht.add(jCheckBoxMeldungenAnzeigen);

        cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE)));
        cbBandwidthDisplay.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(cbBandwidthDisplay.isSelected()));
            Listener.notify(Listener.EREIGNIS_BANDWIDTH_MONITOR, MediathekGui.class.getSimpleName());
        });
    }

    protected void setupHelpMenu() {
        jMenuItemResetSettings.addActionListener(e -> resetSettings());

        miSearchForProgramUpdate.addActionListener(e -> searchForUpdateOrShowProgramInfos(false));
        miShowProgramInfos.addActionListener(e -> searchForUpdateOrShowProgramInfos(true));

        miUpdateServers.addActionListener(e -> updateFilmListServers());
    }

    /**
     * "Force" update the list of filmlist servers.
     */
    private void updateFilmListServers() {
        final FilmeLaden filmeLaden = daten.getFilmeLaden();
        final FilmlistenSuchen list = filmeLaden.getFilmlistenSuchen();

        filmeLaden.getDownloadUrlsFilmlisten_akt().clear();
        filmeLaden.getDownloadUrlsFilmlisten_diff().clear();

        list.updateURLsFilmlisten(true);
        list.updateURLsFilmlisten(false);

        JOptionPane.showMessageDialog(this, "Aktualisierung wurde durchgeführt.", "Update-Server aktualisieren", JOptionPane.INFORMATION_MESSAGE);
    }

    private void resetSettings() {
        ResetSettingsDialog dialog = new ResetSettingsDialog(this, daten);
        GuiFunktionen.centerOnScreen(dialog, false);
        dialog.setVisible(true);
    }

    private void initializeAnsichtAbos()
    {
        //Ansicht Abos
//        jCheckBoxAboExtrafenster.setText(CHECKBOX_TEXT_ABOS_IN_EXTRAFENSTER);
//        jMenuAnsicht.add(jCheckBoxAboExtrafenster);
//        jCheckBoxAboExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_ABO)));
//        jCheckBoxAboExtrafenster.addActionListener(e -> {
//            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_ABO, Boolean.toString(jCheckBoxAboExtrafenster.isSelected()));
//            initFrames();
//        });

        jMenuItemShowOnlineHelp.setIcon(Icons.ICON_MENUE_HELP);
        jMenuItemShowOnlineHelp.addActionListener(e -> {
            if (Desktop.isDesktopSupported()) {
                Desktop d = Desktop.getDesktop();
                try {
                    if (d.isSupported(Desktop.Action.BROWSE)) {
                        d.browse(new URI(Konstanten.ADRESSE_ONLINE_HELP));
                    }
                } catch (Exception ex) {
                    FXErrorDialog.showErrorDialog("Online-Hilfe",
                            "Fehler beim Öffnen der Online-Hilfe",
                            "Es trat ein Fehler beim Öffnen der Online-Hilfe auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                            ex);
                }
            }
        });

        jMenuItemCreateProtocolFile.addActionListener(e -> {
            DialogZiel dialog = new DialogZiel(this, true, GuiFunktionen.getHomePath() + File.separator + "Mediathek.log", "Logdatei speichern");
            dialog.setVisible(true);
            if (!dialog.ok) {
                return;
            }
            if (!Logfile.LogDateiSchreiben(dialog.ziel, MVFunctionSys.getProgVersionString(), Daten.getSettingsDirectory_String(), Daten.listePset.getListProg(), MVConfig.getAll())) {
                MVMessageDialog.showMessageDialog(null, "Datei konnte nicht geschrieben werden!", "Fehler beim Schreiben", JOptionPane.ERROR_MESSAGE);
            }
        });

        jMenuItemAboutApplication.addActionListener(e -> showAboutDialog());
    }

    /**
     * Display the About Box
     */
    protected void showAboutDialog() {
        AboutDialog aboutDialog = new AboutDialog(this);
        GuiFunktionen.centerOnScreen(aboutDialog, false);
        aboutDialog.setVisible(true);
        aboutDialog.dispose();
    }

    private void initializeAnsichtDownloads()
    {
//        jMenuAnsicht.add(new JSeparator());
//
//        jCheckBoxDownloadExtrafenster.setText(CHECKBOX_TEXT_DOWNLOADS_IN_EXTRAFENSTER);
//        jMenuAnsicht.add(jCheckBoxDownloadExtrafenster);
//        jCheckBoxDownloadExtrafenster.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_DOWNLOAD)));
//        jCheckBoxDownloadExtrafenster.addActionListener(e -> {
//            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_DOWNLOAD, Boolean.toString(jCheckBoxDownloadExtrafenster.isSelected()));
//            initFrames();
//        });
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
        // Filme
        jMenuItemFilmlisteLaden.addActionListener(e -> daten.getFilmeLaden().loadFilmlistDialog(daten, false));
        jMenuItemFilmAbspielen.addActionListener(Daten.guiFilme.playAction);
        jMenuItemFilmAufzeichnen.addActionListener(Daten.guiFilme.saveFilmAction);
        jMenuItemBlacklist.addActionListener(e -> {
            DialogLeer dialog = new DialogLeer(daten.getMediathekGui(), true);
            dialog.init(DIALOG_TITLE_BLACKLIST, new PanelBlacklist(daten, daten.getMediathekGui(), PanelBlacklist.class.getName() + PANEL_BLACKLIST_NAME_POSTFIX));
            dialog.setVisible(true);
        });
        jMenuItemFilmeGesehen.addActionListener(Daten.guiFilme.markFilmAsSeenAction);
        jMenuItemFilmeUngesehen.addActionListener(Daten.guiFilme.markFilmAsUnseenAction);
        jMenuItemFilmeMediensammlung.addActionListener(Daten.guiFilme.mediensammlungAction);
    }

    private void initializeDateiMenu() {
        jMenuItemEinstellungen.addActionListener(e -> showSettingsDialog());
        jMenuItemBeenden.addActionListener(e -> beenden(false, false));

        jMenuItemExportFilmlist.addActionListener(e -> exportFilmList());
    }

    /**
     * Export the current filmlist to a user-specified file.
     */
    private void exportFilmList() {
        Platform.runLater(() -> {
            FileChooser fileChooser = new FileChooser();
            fileChooser.setTitle("Datei sichern");
            fileChooser.setInitialFileName("filme");
            fileChooser.getExtensionFilters().addAll(
                    new FileChooser.ExtensionFilter("Unkomprimiert", "*.json"),
                    new FileChooser.ExtensionFilter("XZ Komprimiert (Standard)", "*.xz")
            );
            File selectedFile = fileChooser.showSaveDialog(null);
            if (selectedFile != null) {
                setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                new FilmListWriter().writeFilmList(selectedFile.getAbsolutePath(), daten.getListeFilme());
                setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(this,
                        "Filmliste erfolgreich exportiert.",
                        "Filmliste exportieren", JOptionPane.INFORMATION_MESSAGE));
            }
        });
    }

    public void showSettingsDialog()
    {
        dialogEinstellungen.setVisible(true);
    }

    private void setMenuIcons()
    {
        //Icons setzen
        jMenuItemFilmlisteLaden.setIcon(Icons.ICON_MENUE_FILMLISTE_LADEN);
        jMenuItemEinstellungen.setIcon(Icons.ICON_MENUE_EINSTELLUNGEN);
        jMenuItemBeenden.setIcon(Icons.ICON_MENUE_BEENDEN);
        jMenuItemFilmAbspielen.setIcon(Icons.ICON_MENUE_FILM_START);
        jMenuItemFilmAufzeichnen.setIcon(Icons.ICON_MENUE_FILM_REC);
        jMenuItemFilmeGesehen.setIcon(Icons.ICON_MENUE_HISTORY_ADD);
        jMenuItemFilmeUngesehen.setIcon(Icons.ICON_MENUE_HISTORY_REMOVE);
        jMenuItemBlacklist.setIcon(Icons.ICON_MENUE_BLACKLIST);
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

    /**
     * the global configuration for this app.
     */
    protected Configuration config = ApplicationConfiguration.getConfiguration();

    /**
     * Display a wait dialog with some status message to inform user what is happening currently.
     */
    private class ShutdownDialog {
        private Label lblStatusText;
        private Stage stage;
        private ProgressBar progress;
        private final double maxTasks;

        ShutdownDialog(int maxTasks) {
            this.maxTasks = maxTasks;

            Platform.runLater(() -> {
                stage = new Stage();
                stage.setAlwaysOnTop(true);
                stage.setResizable(false);
                stage.setOnCloseRequest(Event::consume);
                stage.initStyle(StageStyle.UNDECORATED);
                stage.setTitle("Programm beenden");
                stage.setScene(createScene());
            });
        }

        void show() {
            Platform.runLater(() -> {
                stage.show();
                stage.centerOnScreen();
            });
            setEnabled(false);
        }

        void hide() {
            Platform.runLater(() -> stage.hide());
            setEnabled(true);
        }

        void setStatusText(int task, String text) {
            Platform.runLater(() -> {
                final double percent = task / maxTasks;
                progress.setProgress(percent);
                String message = "(" + Integer.toString(task) + "/" + Integer.toString((int) maxTasks) + ") "
                        + text;
                lblStatusText.setText(message);
            });
            //give the user some time to read the messages
            try {
                Thread.sleep(250);
            } catch (InterruptedException ignored) {
            }
        }

        private Scene createScene() {
            MigPane migPane = new MigPane(
                    "hidemode 3",
                    "[fill]" +
                            "[fill]",
                    "[]" +
                            "[]" +
                            "[]");

            progress = new ProgressBar();
            progress.setProgress(0d);
            progress.setPrefWidth(450d);
            progress.setMinWidth(350d);

            migPane.add(new ProgressIndicator(), "cell 0 0 1 3");
            lblStatusText = new Label("Offene Operationen müssen noch beendet werden.");
            migPane.add(lblStatusText, "cell 1 0");
            migPane.add(progress, "cell 1 1");
            migPane.add(new Label(""), "cell 1 2");

            return new Scene(migPane);
        }
    }

    public boolean beenden(boolean showOptionTerminate, boolean shutDown) {
        //write all settings if not done already...
        ApplicationConfiguration.getInstance().writeConfiguration();

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

        //do not search for updates anymore
        updateCheckTimer.stop();

        ShutdownDialog dialog = new ShutdownDialog(12);
        dialog.show();

        dialog.setStatusText(1, "Warte auf das Schreiben der Filmliste");
        waitForFilmListWriterToComplete();

        dialog.setStatusText(2, "Warte auf commonPool()");
        waitForCommonPoolToComplete();

        dialog.setStatusText(3, "Warte auf Abschluss der Datenbank-Operationen");
        waitForDatabasePoolToComplete();

        // Tabelleneinstellungen merken
        dialog.setStatusText(4, "Film-Daten sichern");
        Daten.guiFilme.tabelleSpeichern();

        dialog.setStatusText(5, "Download-Daten sichern");
        Daten.guiDownloads.tabelleSpeichern();

        dialog.setStatusText(6, "Abo-Daten sichern");
        Daten.guiAbo.tabelleSpeichern();

        dialog.setStatusText(7, "MediaDB sichern");
        daten.getDialogMediaDB().tabelleSpeichern();

        dialog.setStatusText(8, "Downloads anhalten");
        stopDownloads();

        dialog.setStatusText(9, "Programmkonfiguration schreiben");
        if (getExtendedState() == JFrame.MAXIMIZED_BOTH) {
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_MAX, Boolean.TRUE.toString());
        } else {
            MVConfig.add(MVConfig.Configs.SYSTEM_FENSTER_MAX, Boolean.FALSE.toString());
        }

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

        dialog.setStatusText(10, "Datenbank schließen");
        DatenFilm.Database.closeDatabase();

        dialog.setStatusText(11, "Programmdaten sichern");
        daten.allesSpeichern();

        dialog.setStatusText(12, "Fertig.");
        dialog.hide();

        Log.endMsg();
        Duration.printCounter();

        if (shutDown) {
            shutdownComputer();
        }

        dispose();

        //shutdown JavaFX
        Platform.exit();

        System.exit(0);

        return false;
    }

    private void stopDownloads() {
        if (daten.getListeDownloads() != null) {
            // alle laufenden Downloads/Programme stoppen
            for (DatenDownload download : daten.getListeDownloads()) {
                Start s = download.start;
                if (s != null) {
                    s.stoppen = true;
                }
            }
        }
    }

    private void waitForDatabasePoolToComplete() {
        logger.debug("waiting for database pool to complete");

        ExecutorService pool = PooledDatabaseConnection.getInstance().getDatabaseExecutor();
        pool.shutdown();
        try {
            if (!pool.awaitTermination(120, TimeUnit.SECONDS)) {
                pool.shutdownNow();
                if (!pool.awaitTermination(60, TimeUnit.SECONDS))
                    logger.error("Pool did not terminate");
            }
        } catch (InterruptedException ie) {
            // (Re-)Cancel if current thread also interrupted
            pool.shutdownNow();
            // Preserve interrupt status
            Thread.currentThread().interrupt();
        }

        logger.debug("done waiting database pool");
    }

    private void waitForFilmListWriterToComplete() {
        daten.getFilmListWriterFuture().ifPresent(future -> {
            logger.debug("waiting for filmlist completion");
            try {
                future.get();
            } catch (InterruptedException | ExecutionException e) {
                logger.error(e);
            }
            logger.debug("done waiting");
        });
    }

    private void waitForCommonPoolToComplete() {
        while (ForkJoinPool.commonPool().hasQueuedSubmissions()) {
            try {
                logger.debug("POOL SUBMISSIONS: {}", ForkJoinPool.commonPool().getQueuedSubmissionCount());
                TimeUnit.MILLISECONDS.sleep(500);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

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
                logger.error("Shutdown unsupported operating system ...");
                break;
        }

        //only run if we have a proper shutdown command...
        if (!strShutdownCommand.isEmpty()) {
            try {
                logger.info("Shutdown: {}", strShutdownCommand);
                Runtime.getRuntime().exec(strShutdownCommand);
            } catch (IOException ex) {
                logger.error(ex);
            }
        }
    }

    private void searchForUpdateOrShowProgramInfos(boolean infos) {
        new ProgrammUpdateSuchen().checkVersion(!infos, infos, true);
    }

    private class MenuLST implements MenuListener {

        private final TABS tabs;

        MenuLST(TABS tabs) {
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

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanelCont = new javax.swing.JPanel();
        jPanelInfo = new javax.swing.JPanel();
        jTabbedPane = new javax.swing.JTabbedPane();
        jMenuBar = new javax.swing.JMenuBar();
        jMenuDatei = new javax.swing.JMenu();
        jMenuItemFilmlisteLaden = new javax.swing.JMenuItem();
        jMenuItemExportFilmlist = new javax.swing.JMenuItem();
        jMenuItemEinstellungen = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JPopupMenu.Separator();
        jMenuItemBeenden = new javax.swing.JMenuItem();
        jMenuFilme = new javax.swing.JMenu();
        jMenuItemFilmAbspielen = new javax.swing.JMenuItem();
        jMenuItemFilmAufzeichnen = new javax.swing.JMenuItem();
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
        javax.swing.JPopupMenu.Separator jSeparator8 = new javax.swing.JPopupMenu.Separator();
        miSearchForProgramUpdate = new javax.swing.JMenuItem();
        miShowProgramInfos = new javax.swing.JMenuItem();
        javax.swing.JPopupMenu.Separator jSeparator9 = new javax.swing.JPopupMenu.Separator();
        miUpdateServers = new javax.swing.JMenuItem();
        jSeparatorAboutApplication = new javax.swing.JPopupMenu.Separator();
        jMenuItemAboutApplication = new javax.swing.JMenuItem();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);

        jPanelCont.setLayout(new java.awt.BorderLayout());

        jPanelInfo.setLayout(new java.awt.BorderLayout());
        jPanelCont.add(jPanelInfo, java.awt.BorderLayout.PAGE_END);

        jTabbedPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 1, 1, 1));
        jPanelCont.add(jTabbedPane, java.awt.BorderLayout.CENTER);

        jMenuDatei.setMnemonic('d');
        jMenuDatei.setText("Datei");

        jMenuItemFilmlisteLaden.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F5, 0));
        jMenuItemFilmlisteLaden.setText("Neue Filmliste laden");
        jMenuDatei.add(jMenuItemFilmlisteLaden);

        jMenuItemExportFilmlist.setText("Filmliste exportieren...");
        jMenuDatei.add(jMenuItemExportFilmlist);

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

        jMenuItemDownloadsZurueckstellen.setText("Downloads zurückstellen");
        jMenuDownload.add(jMenuItemDownloadsZurueckstellen);

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

        jMenuItemAbosLoeschen.setText("löschen");
        jMenuAbos.add(jMenuItemAbosLoeschen);

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
        jMenuHilfe.add(jSeparator8);

        miSearchForProgramUpdate.setText("Nach Update suchen...");
        jMenuHilfe.add(miSearchForProgramUpdate);

        miShowProgramInfos.setText("Programminfos anzeigen...");
        jMenuHilfe.add(miShowProgramInfos);
        jMenuHilfe.add(jSeparator9);

        miUpdateServers.setText("Update-Server aktualisieren...");
        jMenuHilfe.add(miUpdateServers);
        jMenuHilfe.add(jSeparatorAboutApplication);

        jMenuItemAboutApplication.setText("Über dieses Programm...");
        jMenuHilfe.add(jMenuItemAboutApplication);

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
                .addComponent(jPanelCont, javax.swing.GroupLayout.DEFAULT_SIZE, 827, Short.MAX_VALUE))
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
    protected javax.swing.JMenuItem jMenuItemAboutApplication;
    protected javax.swing.JMenuItem jMenuItemBeenden;
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
    protected javax.swing.JMenuItem jMenuItemEinstellungen;
    private javax.swing.JMenuItem jMenuItemExportFilmlist;
    protected javax.swing.JMenuItem jMenuItemFilmAbspielen;
    protected javax.swing.JMenuItem jMenuItemFilmAufzeichnen;
    private javax.swing.JMenuItem jMenuItemFilmeGesehen;
    private javax.swing.JMenuItem jMenuItemFilmeMediensammlung;
    private javax.swing.JMenuItem jMenuItemFilmeUngesehen;
    private javax.swing.JMenuItem jMenuItemFilmlisteLaden;
    protected javax.swing.JMenuItem jMenuItemResetSettings;
    private javax.swing.JMenuItem jMenuItemSchriftGr;
    private javax.swing.JMenuItem jMenuItemSchriftKl;
    private javax.swing.JMenuItem jMenuItemSchriftNormal;
    private javax.swing.JMenuItem jMenuItemShowOnlineHelp;
    private javax.swing.JPanel jPanelInfo;
    protected javax.swing.JPopupMenu.Separator jSeparator2;
    protected javax.swing.JPopupMenu.Separator jSeparatorAboutApplication;
    private javax.swing.JTabbedPane jTabbedPane;
    private javax.swing.JMenuItem miSearchForProgramUpdate;
    private javax.swing.JMenuItem miShowProgramInfos;
    private javax.swing.JMenuItem miUpdateServers;
    // End of variables declaration//GEN-END:variables
}
