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

import javafx.application.Platform;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mSearch.daten.DatenFilm;
import mSearch.daten.PooledDatabaseConnection;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.*;
import mSearch.tool.Functions.OperatingSystemType;
import mediathek.config.*;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.ListeMediaDB;
import mediathek.gui.*;
import mediathek.gui.actions.*;
import mediathek.gui.actions.export.FilmListExportAction;
import mediathek.gui.bandwidth.MVBandwidthMonitor;
import mediathek.gui.dialog.AboutDialog;
import mediathek.gui.dialog.DialogBeenden;
import mediathek.gui.dialog.DialogMediaDB;
import mediathek.gui.dialog.DialogStarteinstellungen;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.filmInformation.InfoDialog;
import mediathek.gui.messages.*;
import mediathek.javafx.*;
import mediathek.res.GetIcon;
import mediathek.tool.*;
import mediathek.tool.threads.IndicatorThread;
import mediathek.update.ProgramUpdateCheck;
import mediathek.update.ProgrammUpdateSuchen;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashMap;
import java.util.concurrent.CompletableFuture;
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
    private static final String SPLASHSCREEN_TEXT_ANWENDUNGSDATEN_LADEN = "Anwendungsdaten laden...";
    private static final String SPLASHSCREEN_TEXT_GUI_INITIALISIEREN = "GUI Initialisieren...";
    private static final String LOG_TEXT_DIE_DOWNLOADS_MUESSEN_ZUERST_GESTARTET_WERDEN = "Die Downloads müssen zuerst gestartet werden.";
    private static final String LOG_TEXT_KEINE_LAUFENDEN_DOWNLOADS = "Keine laufenden Downloads!";


    private final Daten daten;
    private final SplashScreenManager splashScreenManager;
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

    /**
     * Bandwidth monitoring for downloads.
     */
    protected MVBandwidthMonitor bandwidthMonitor;

    private void remapF10Key() {
        //Hier wird F10 default Funktion unterbunden:
        InputMap im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(KeyStroke.getKeyStroke(KEY_F10), NONE);
    }

    public MVSenderIconCache getSenderIconCache() {
        return senderIconCache;
    }

    /**
     * setup the system wide shutdown handler
     */
    protected void setupShutdownCommand() {
        shutdownCommand = new GenericShutdownComputerCommand();
    }

    /**
     * Initialize JavaFX runtime by calling swing interop class.
     * This will start JavaFX thread in case no window has been started yet.
     * Necessary in case no config is found.
     */
    @SuppressWarnings("unused")
    private void fakeInitializeJavaFXRuntime() {
        final JFXPanel dummyPanel = new JFXPanel();
    }

    /**
     * "Pointer" to UI
     */
    private static MediathekGui ui = null;

    public MediathekGui() {
        super();
        ui = this;

        setupShutdownCommand();

        splashScreenManager = new SplashScreenManager();
        splashScreenManager.initializeSplashScreen();

        initComponents();

        setWindowTitle();

        setIconImage(GetIcon.getIcon(ICON_NAME, ICON_PATH, ICON_WIDTH, ICON_HEIGHT).getImage());

        senderIconCache = new MVSenderIconCache();

        remapF10Key();

        splashScreenManager.updateSplashScreenText(SPLASHSCREEN_TEXT_ANWENDUNGSDATEN_LADEN);

        daten = Daten.getInstance();

        startMeldungen();

        fakeInitializeJavaFXRuntime();

        loadDaten();

        createStatusBar();

        createFilmInformationHUD();

        setLookAndFeel();
        init();
        setSize();

        initializeSettingsDialog();


        addListener();
        setupSearchKeyForMac();

        //register message bus handler
        daten.getMessageBus().subscribe(this);

        setFocusOnSearchField();

        createMemoryMonitor();

        bandwidthMonitor = new MVBandwidthMonitor(this);

        splashScreenManager.closeSplashScreen();

        if (!SystemUtils.IS_OS_WINDOWS)
            workaroundControlsFxNotificationBug();

        loadFilmlist();
    }

    /**
     * Return the user interface instance
     * @return the class instance or null.
     */
    public static MediathekGui ui() {
        return ui;
    }

    protected Stage controlsFxWorkaroundStage;

    /**
     * ControlsFX Notifications expect a stage to be open.
     * Create a utility window hidden and transparent as a stage for them.
     */
    protected void workaroundControlsFxNotificationBug() {
        //does not work on windows and linux
    }

    /**
     * Memory display for debugging purposes.
     * Only visible when debug mode is enabled
     */
    private MemoryMonitor memoryMonitor;

    private void createMemoryMonitor() {
        Platform.runLater(() -> {
            if (Config.isDebuggingEnabled()) {
                memoryMonitor = new MemoryMonitor();
                memoryMonitor.show();
            }
        });
    }

    private void loadFilmlist() {
        Platform.runLater(() -> {
            HBox hb = new HBox();
            hb.setSpacing(4d);
            javafx.scene.control.Label lb = new Label("");
            ProgressBar prog = new ProgressBar();
            prog.setProgress(ProgressIndicator.INDETERMINATE_PROGRESS);
            hb.getChildren().addAll(
                    new VerticalSeparator(),
                    new CenteredBorderPane(lb),
                    new CenteredBorderPane(prog)
            );

            PerformFilmListFilterOperationsTask task = new PerformFilmListFilterOperationsTask();
            task.setOnRunning(e -> {
                getStatusBarController().getStatusBar().getRightItems().add(hb);
                lb.textProperty().bind(task.messageProperty());
                prog.progressProperty().bind(task.progressProperty());
            });

            FilmListFilterTask filterTask = new FilmListFilterTask(true);
            filterTask.setOnRunning(e -> {
                lb.textProperty().bind(filterTask.messageProperty());
                prog.progressProperty().bind(filterTask.progressProperty());
            });
            filterTask.setOnSucceeded(e -> getStatusBarController().getStatusBar().getRightItems().remove(hb));
            filterTask.setOnFailed(e -> getStatusBarController().getStatusBar().getRightItems().remove(hb));

            CompletableFuture<Void> loaderTask = CompletableFuture.runAsync(task);
            loaderTask.thenRun(filterTask);
        });
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

    private StartupProgressPanel panel;

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
     * Ununsed on other platforms.
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
            // erster Start
            ReplaceList.init(); // einmal ein Muster anlegen, für Linux/OS X ist es bereits aktiv!
            new DialogStarteinstellungen(this, daten).setVisible(true);
            MVConfig.loadSystemParameter();
            this.pack();
        }
    }

    /**
     * this property keeps track how many items are currently selected in the active table view
     */
    private final IntegerProperty selectedItemsProperty = new SimpleIntegerProperty(0);

    public IntegerProperty getSelectedItemsProperty() {
        return selectedItemsProperty;
    }

    private StatusBarController statusBarController;

    /**
     * Create the status bar item.
     */
    private void createStatusBar() {
        statusBarController = new StatusBarController(daten);

        JFXPanel statusBarPanel = new JFXPanel();
        Platform.runLater(() -> {
            statusBarPanel.setScene(new Scene(statusBarController.createStatusBar()));
            installSelectedItemsLabel();
        });

        jPanelInfo.add(statusBarPanel, BorderLayout.CENTER);
    }

    private void installSelectedItemsLabel() {
        ObservableList<Node> leftItems = statusBarController.getStatusBar().getLeftItems();
        leftItems.add(0, new SelectedItemsLabel(selectedItemsProperty));
        leftItems.add(1, new VerticalSeparator());
    }

    public StatusBarController getStatusBarController() {
        return statusBarController;
    }

    /**
     * Helper to determine what tab is currently active
     */
    private final ObjectProperty<TabPaneIndex> tabPaneIndexProperty = new SimpleObjectProperty<>(TabPaneIndex.NONE);

    public ObjectProperty<TabPaneIndex> tabPaneIndexProperty() {
        return tabPaneIndexProperty;
    }

    private static final Logger logger = LogManager.getLogger(MediathekGui.class);

    private InfoDialog filmInfo; // Infos zum Film

    public InfoDialog getFilmInfoDialog() {
        return filmInfo;
    }

    /**
     * Create the film information tool window.
     */
    private void createFilmInformationHUD() {
        filmInfo = new InfoDialog(this, senderIconCache);
    }

    @Handler
    private void handleTabVisualSettingsChangedEvent(TabVisualSettingsChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            configureTabPlacement();
            configureTabIcons();
        });
    }

    private void addListener() {
        Listener.addListener(new Listener(Listener.EREIGNIS_DIALOG_MEDIA_DB, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxMenuItemMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
            }
        });
    }

    @Handler
    private void handleBandwidthMonitorStateChangedEvent(BandwidthMonitorStateChangedEvent e) {
        SwingUtilities.invokeLater(() -> cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE))));
    }

    protected void setFocusOnSearchField() {
        Platform.runLater(() -> tabFilme.fap.getSearchField().requestFocus());
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

    private void setWindowTitle() {
        setTitle(Konstanten.PROGRAMMNAME + ' ' + Konstanten.MVVERSION);
    }

    private void setSize() {
        if (Daten.isStartMaximized() || Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_FENSTER_MAX))) {
            this.setExtendedState(Frame.MAXIMIZED_BOTH);
        } else {
            GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_GUI, this, null);
        }
    }

    private void setupFilmListListener() {
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                jMenuItemFilmlisteLaden.setEnabled(false);
                jMenuItemDownloadsAktualisieren.setEnabled(false);
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
    }

    private void init() {
        initTabs();
        initMenus();

        setupFilmListListener();

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                if (tray != null && !SystemUtils.IS_OS_MAC_OSX && Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
                    setVisible(false);
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
     * This will setup a repeating update check every 24 hours.
     */
    private void setupUpdateCheck() {
        programUpdateChecker = new ProgramUpdateCheck(daten);
        programUpdateChecker.start();
    }

    private ProgramUpdateCheck programUpdateChecker;

    public void setTray() {
        if (tray == null && Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
            tray = new MVTray().systemTray();
        } else if (tray != null && !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
            tray.beenden();
            tray = null;
        }
    }

    public GuiFilme tabFilme;
    public GuiDownloads tabDownloads;
    public GuiAbo tabAbos;

    private void initTabs() {
        tabDownloads = new GuiDownloads(daten, this);
        tabAbos = new GuiAbo(daten, this);
        tabFilme = new GuiFilme(daten, this);

        jTabbedPane.addTab(GuiFilme.NAME, tabFilme);
        jTabbedPane.addTab(GuiDownloads.NAME, tabDownloads);
        jTabbedPane.addTab(GuiAbo.NAME,tabAbos);
        jTabbedPane.setSelectedIndex(0);

        configureTabPlacement();
        configureTabIcons();
    }

    /**
     * Enable/Disable the update related menu item.
     *
     * @param enable Shall the menu item be enabled?
     */
    public void enableUpdateMenuItem(boolean enable) {
        miSearchForProgramUpdate.setEnabled(enable);
    }

    /**
     * Change placement of tabs based on settings
     */
    private void configureTabPlacement() {
        final boolean top = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_TOP));
        if (top)
            jTabbedPane.setTabPlacement(JTabbedPane.TOP);
        else
            jTabbedPane.setTabPlacement(JTabbedPane.LEFT);
    }

    private void configureTabIcons() {
        final boolean icon = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_ICON));

        //no icons...
        if (!icon) {
            setTabIcon(tabFilme, null);
            setTabIcon(tabDownloads, null);
            setTabIcon(tabAbos, null);
        } else {
            //setup icons for each tab here
            setTabIcon(tabFilme, Icons.ICON_TAB_FILM);
            setTabIcon(tabDownloads, Icons.ICON_TAB_DOWNLOAD);
            setTabIcon(tabAbos, Icons.ICON_TAB_ABO);
        }
    }

    private void setTabIcon(Component tab, Icon icon) {
        final int index = jTabbedPane.indexOfComponent(tab);
        jTabbedPane.setIconAt(index,icon);
    }
    /**
     * Number of active downloads
     */
    protected final AtomicInteger numDownloadsStarted = new AtomicInteger(0);

    /**
     * Progress indicator thread for OS X and windows.
     */
    private IndicatorThread progressIndicatorThread;

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

    private final HashMap<JMenu, MenuTabSwitchListener> menuListeners = new HashMap<>();

    /**
     * Install the listeners which will cause automatic tab switching based on associated Menu item.
     */
    protected void installMenuTabSwitchListener() {
        //initial setup
        menuListeners.put(jMenuFilme, new MenuTabSwitchListener(TABS.TAB_FILME));
        menuListeners.put(jMenuDownload, new MenuTabSwitchListener(TABS.TAB_DOWNLOADS));
        menuListeners.put(jMenuAbos, new MenuTabSwitchListener(TABS.TAB_ABOS));

        //now assign if really necessary
        if (config.getBoolean(ApplicationConfiguration.APPLICATION_INSTALL_TAB_SWITCH_LISTENER, true)) {
            jMenuFilme.addMenuListener(menuListeners.get(jMenuFilme));
            jMenuDownload.addMenuListener(menuListeners.get(jMenuDownload));
            jMenuAbos.addMenuListener(menuListeners.get(jMenuAbos));
        }
    }

    public JCheckBoxMenuItem getFilmDescriptionMenuItem() {
        return cbkBeschreibung;
    }

    public JCheckBoxMenuItem getDownloadFilmDescriptionMenuItem() {
        return miShowDownloadDescription;
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

    protected void initMenus() {
        installMenuTabSwitchListener();

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

        jCheckBoxMenuItemMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
        jCheckBoxMenuItemMediaDb.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, String.valueOf(jCheckBoxMenuItemMediaDb.isSelected()));
            daten.getDialogMediaDB().setVis();
        });
        jMenuItemSchriftGr.addActionListener(e -> MVFont.setFontSize(true));
        jMenuItemSchriftKl.addActionListener(e -> MVFont.setFontSize(false));
        jMenuItemSchriftNormal.addActionListener(e -> MVFont.resetFontSize());

        initializeAnsichtAbos();
        initializeAnsicht();

        miShowMemoryMonitor.addActionListener(e -> showMemoryMonitor());
    }

    private void showMemoryMonitor() {
        Platform.runLater(() -> {
            if (memoryMonitor == null) {
                memoryMonitor = new MemoryMonitor();
            }

            memoryMonitor.show();
        });
    }

    private void initializeAnsicht()
    {
        cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE)));
        cbBandwidthDisplay.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(cbBandwidthDisplay.isSelected()));
            daten.getMessageBus().publishAsync(new BandwidthMonitorStateChangedEvent());
        });
    }

    protected void setupHelpMenu() {
        jMenuItemResetSettings.setAction(new ResetSettingsAction(this, daten));

        miSearchForProgramUpdate.addActionListener(e -> searchForUpdateOrShowProgramInfos(false));
        miShowProgramInfos.addActionListener(e -> searchForUpdateOrShowProgramInfos(true));

        miUpdateServers.setAction(new UpdateFilmListServersAction());
    }

    private void initializeAnsichtAbos()
    {
        jMenuItemShowOnlineHelp.setAction(new ShowOnlineHelpAction());

        jMenuItemCreateProtocolFile.setAction(new CreateProtocolFileAction());

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

    private void initializeAboMenu()
    {
        jMenuItemAbosEinschalten.setIcon(Icons.ICON_MENUE_EIN);
        jMenuItemAbosEinschalten.addActionListener(e -> tabAbos.einAus(true));

        jMenuItemAbosAusschalten.setIcon(Icons.ICON_MENUE_AUS);
        jMenuItemAbosAusschalten.addActionListener(e -> tabAbos.einAus(false));

        jMenuItemAbosLoeschen.setIcon(Icons.ICON_MENUE_ABO_LOESCHEN);
        jMenuItemAbosLoeschen.addActionListener(e -> tabAbos.loeschen());

        jMenuItemAbosAendern.setIcon(Icons.ICON_MENUE_ABO_AENDERN);
        jMenuItemAbosAendern.addActionListener(e -> tabAbos.aendern());

        jMenuItemAboNeu.addActionListener(e -> tabAbos.neu());
        jMenuItemAboInvertSelection.addActionListener(e -> tabAbos.invertSelection());
    }

    private void initializeDownloadsMenu()
    {
        jMenuItemDownloadsAktualisieren.setIcon(Icons.ICON_MENUE_AKTUALISIEREN);
        jMenuItemDownloadsAktualisieren.addActionListener(e -> tabDownloads.aktualisieren());

        jMenuItemDownloadAbspielen.setIcon(Icons.ICON_MENUE_FILM_START);
        jMenuItemDownloadAbspielen.addActionListener(e -> tabDownloads.filmAbspielen());

        jMenuItemDownloadsAufraeumen.setIcon(Icons.ICON_MENUE_CLEAR);
        jMenuItemDownloadsAufraeumen.addActionListener(e -> tabDownloads.aufraeumen());

        jMenuItemDownloadsLoeschen.setIcon(Icons.ICON_MENUE_DOWNOAD_LOESCHEN);
        jMenuItemDownloadsLoeschen.addActionListener(e -> tabDownloads.loeschen());

        jMenuItemDownloadsAlleStarten.setIcon(Icons.ICON_MENUE_DOWNLOAD_ALLE_STARTEN);
        jMenuItemDownloadsAlleStarten.addActionListener(e -> tabDownloads.starten(true));

        jMenuItemDownloadStartTime.setIcon(Icons.ICON_MENUE_DOWNLOAD_ALLE_STARTEN);
        jMenuItemDownloadStartTime.addActionListener(e -> tabDownloads.startAtTime());

        jMenuItemDownloadStarten.setIcon(Icons.ICON_MENUE_DOWNOAD_STARTEN);
        jMenuItemDownloadStarten.addActionListener(e -> tabDownloads.starten(false));

        jMenuItemDownloadsZurueckstellen.setIcon(Icons.ICON_MENUE_DOWNLOAD_ZURUECKSTELLEN);
        jMenuItemDownloadsZurueckstellen.addActionListener(e -> tabDownloads.zurueckstellen());

        jMenuItemDownloadVorziehen.setIcon(Icons.ICON_MENUE_VORZIEHEN);
        jMenuItemDownloadVorziehen.addActionListener(e -> tabDownloads.vorziehen());

        jMenuItemDownloadAendern.setIcon(Icons.ICON_MENUE_DOWNLOAD_AENDERN);
        jMenuItemDownloadAendern.addActionListener(e -> tabDownloads.aendern());

        jMenuItemDownloadAlleStoppen.setIcon(Icons.ICON_MENUE_DOWNOAD_STOP);
        jMenuItemDownloadAlleStoppen.addActionListener(e -> tabDownloads.stoppen(true ));

        jMenuItemDownloadWartendeStoppen.setIcon(Icons.ICON_MENUE_DOWNOAD_STOP);
        jMenuItemDownloadWartendeStoppen.addActionListener(e -> tabDownloads.wartendeStoppen());

        jMenuItemDownloadStoppen.setIcon(Icons.ICON_MENUE_DOWNOAD_STOP);
        jMenuItemDownloadStoppen.addActionListener(e -> tabDownloads.stoppen(false));

        jMenuItemDownloadShutDown.setIcon(Icons.ICON_MENUE_BEENDEN);
        jMenuItemDownloadShutDown.addActionListener(e -> {
            if (daten.getListeDownloads().nochNichtFertigeDownloads() > 0) {
                // ansonsten gibts keine laufenden Downloads auf die man warten sollte
                beenden(true, false);
            } else {
                MVMessageDialog.showMessageDialog(this, LOG_TEXT_DIE_DOWNLOADS_MUESSEN_ZUERST_GESTARTET_WERDEN,
                        LOG_TEXT_KEINE_LAUFENDEN_DOWNLOADS, JOptionPane.ERROR_MESSAGE);
            }
        });

        jMenuItemDownloadGesehen.setIcon(Icons.ICON_MENUE_HISTORY_ADD);
        jMenuItemDownloadGesehen.addActionListener(e -> tabDownloads.filmGesehen());

        jMenuItemDownloadUngesehen.setIcon(Icons.ICON_MENUE_HISTORY_REMOVE);
        jMenuItemDownloadUngesehen.addActionListener(e -> tabDownloads.filmUngesehen());

        jMenuItemDownloadMediensammlung.addActionListener(e -> tabDownloads.guiFilmMediensammlung());
        jMenuItemDownloadInvertSelection.addActionListener(e -> tabDownloads.invertSelection());
    }

    private void initializeFilmeMenu()
    {
        jMenuItemFilmAbspielen.setIcon(Icons.ICON_MENUE_FILM_START);
        jMenuItemFilmAbspielen.addActionListener(tabFilme.playAction);

        jMenuItemFilmAufzeichnen.setIcon(Icons.ICON_MENUE_FILM_REC);
        jMenuItemFilmAufzeichnen.addActionListener(tabFilme.saveFilmAction);

        jMenuItemBlacklist.setAction(new ShowBlacklistDialogAction(this, daten));

        jMenuItemFilmeGesehen.setIcon(Icons.ICON_MENUE_HISTORY_ADD);
        jMenuItemFilmeGesehen.addActionListener(tabFilme.markFilmAsSeenAction);

        jMenuItemFilmeUngesehen.setIcon(Icons.ICON_MENUE_HISTORY_REMOVE);
        jMenuItemFilmeUngesehen.addActionListener(tabFilme.markFilmAsUnseenAction);

        jMenuItemFilmeMediensammlung.addActionListener(tabFilme.mediensammlungAction);
    }

    private void initializeDateiMenu() {
        jMenuItemFilmlisteLaden.addActionListener(e -> daten.getFilmeLaden().loadFilmlistDialog(daten, false));
        jMenuItemFilmlisteLaden.setIcon(IconFontSwing.buildIcon(FontAwesome.CLOUD_DOWNLOAD, 16));

        jMenuItemEinstellungen.addActionListener(e -> showSettingsDialog());
        jMenuItemBeenden.addActionListener(e -> beenden(false, false));

        jMenuItemExportFilmlist.setAction(new FilmListExportAction(this));
    }

    public void showSettingsDialog()
    {
        dialogEinstellungen.setVisible(true);
    }

    /**
     * the global configuration for this app.
     */
    protected Configuration config = ApplicationConfiguration.getConfiguration();

    private void closeMemoryMonitor() {
            if (memoryMonitor != null)
                Platform.runLater(() -> memoryMonitor.close());
    }

    private void writeOldConfiguration() {
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
    }

    private void closeControlsFxWorkaroundStage() {
        Platform.runLater(() -> {
            if (controlsFxWorkaroundStage != null)
                controlsFxWorkaroundStage.close();
        });
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

        closeMemoryMonitor();

        closeControlsFxWorkaroundStage();

        programUpdateChecker.close();

        ShutdownDialog dialog = new ShutdownDialog(this, 11);
        dialog.show();

        dialog.setStatusText(1, "Warte auf commonPool()");
        waitForCommonPoolToComplete();

        dialog.setStatusText(2, "Warte auf Abschluss der Datenbank-Operationen");
        waitForDatabasePoolToComplete();

        // Tabelleneinstellungen merken
        dialog.setStatusText(3, "Film-Daten sichern");
        tabFilme.tabelleSpeichern();

        dialog.setStatusText(4, "Download-Daten sichern");
        tabDownloads.tabelleSpeichern();

        dialog.setStatusText(5, "Abo-Daten sichern");
        tabAbos.tabelleSpeichern();

        dialog.setStatusText(6, "MediaDB sichern");
        daten.getDialogMediaDB().tabelleSpeichern();

        dialog.setStatusText(7, "Downloads anhalten");
        stopDownloads();

        dialog.setStatusText(8, "Programmkonfiguration schreiben");
        writeOldConfiguration();

        dialog.setStatusText(9, "Datenbank schließen");
        DatenFilm.Database.closeDatabase();

        dialog.setStatusText(10, "Programmdaten sichern");
        daten.allesSpeichern();

        dialog.setStatusText(11, "Fertig.");
        dialog.hide();

        Log.endMsg();

        if (shutDown) {
            shutdownComputer();
        }

        dispose();

        //shutdown JavaFX
        Platform.runLater(Platform::exit);

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
    private void shutdownComputer() {
        shutdownCommand.execute();
    }

    /**
     * Used for implementing shutting down the system.
     */
    protected ShutdownComputerCommand shutdownCommand;

    private void searchForUpdateOrShowProgramInfos(boolean infos) {
        new ProgrammUpdateSuchen().checkVersion(!infos, infos, true);
    }

    private class MenuTabSwitchListener implements MenuListener {

        private final TABS tabs;

        MenuTabSwitchListener(TABS tabs) {
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
                case TAB_FILME:
                    setTabIfContain(tabFilme);
                    break;
                case TAB_DOWNLOADS:
                    setTabIfContain(tabDownloads);
                    break;
                case TAB_ABOS:
                    setTabIfContain(tabAbos);
                    break;

                default:
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
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jMenuBar = new JMenuBar();
        jMenuDatei = new JMenu();
        jMenuItemFilmlisteLaden = new JMenuItem();
        jMenuItemExportFilmlist = new JMenuItem();
        jMenuItemEinstellungen = new JMenuItem();
        jSeparator2 = new JSeparator();
        jMenuItemBeenden = new JMenuItem();
        jMenuFilme = new JMenu();
        jMenuItemFilmAbspielen = new JMenuItem();
        jMenuItemFilmAufzeichnen = new JMenuItem();
        jMenuItemBlacklist = new JMenuItem();
        JSeparator separator1 = new JSeparator();
        cbkBeschreibung = new JCheckBoxMenuItem();
        jMenuItemFilmeGesehen = new JMenuItem();
        jMenuItemFilmeUngesehen = new JMenuItem();
        jMenuItemFilmeMediensammlung = new JMenuItem();
        jMenuDownload = new JMenu();
        jMenuItemDownloadsAlleStarten = new JMenuItem();
        jMenuItemDownloadStartTime = new JMenuItem();
        jMenuItemDownloadAlleStoppen = new JMenuItem();
        jMenuItemDownloadWartendeStoppen = new JMenuItem();
        jMenuItemDownloadsAktualisieren = new JMenuItem();
        jMenuItemDownloadsAufraeumen = new JMenuItem();
        jMenuItemDownloadStarten = new JMenuItem();
        jMenuItemDownloadStoppen = new JMenuItem();
        jMenuItemDownloadVorziehen = new JMenuItem();
        jMenuItemDownloadsZurueckstellen = new JMenuItem();
        jMenuItemDownloadsLoeschen = new JMenuItem();
        jMenuItemDownloadAendern = new JMenuItem();
        miShowDownloadDescription = new JCheckBoxMenuItem();
        jMenuItemDownloadGesehen = new JMenuItem();
        jMenuItemDownloadUngesehen = new JMenuItem();
        jMenuItemDownloadAbspielen = new JMenuItem();
        jMenuItemDownloadMediensammlung = new JMenuItem();
        jMenuItemDownloadInvertSelection = new JMenuItem();
        jMenuItemDownloadShutDown = new JMenuItem();
        jMenuAbos = new JMenu();
        jMenuItemAbosEinschalten = new JMenuItem();
        jMenuItemAbosAusschalten = new JMenuItem();
        jMenuItemAbosLoeschen = new JMenuItem();
        jMenuItemAbosAendern = new JMenuItem();
        jMenuItemAboNeu = new JMenuItem();
        jMenuItemAboInvertSelection = new JMenuItem();
        JMenu jMenuAnsicht = new JMenu();
        jCheckBoxMenuItemVideoplayer = new JCheckBoxMenuItem();
        JMenu jMenu1 = new JMenu();
        jMenuItemSchriftGr = new JMenuItem();
        jMenuItemSchriftKl = new JMenuItem();
        jMenuItemSchriftNormal = new JMenuItem();
        miShowMemoryMonitor = new JMenuItem();
        cbBandwidthDisplay = new JCheckBoxMenuItem();
        jCheckBoxMenuItemMediaDb = new JCheckBoxMenuItem();
        jMenuHilfe = new JMenu();
        jMenuItemShowOnlineHelp = new JMenuItem();
        jMenuItemCreateProtocolFile = new JMenuItem();
        jMenuItemResetSettings = new JMenuItem();
        miSearchForProgramUpdate = new JMenuItem();
        miShowProgramInfos = new JMenuItem();
        miUpdateServers = new JMenuItem();
        jSeparatorAboutApplication = new JSeparator();
        jMenuItemAboutApplication = new JMenuItem();
        JPanel jPanelCont = new JPanel();
        jPanelInfo = new JPanel();
        jTabbedPane = new JTabbedPane();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        Container contentPane = getContentPane();

        //======== jMenuBar ========
        {

            //======== jMenuDatei ========
            {
                jMenuDatei.setMnemonic('d');
                jMenuDatei.setText("Datei");

                //---- jMenuItemFilmlisteLaden ----
                jMenuItemFilmlisteLaden.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0));
                jMenuItemFilmlisteLaden.setText("Neue Filmliste laden");
                jMenuDatei.add(jMenuItemFilmlisteLaden);

                //---- jMenuItemExportFilmlist ----
                jMenuItemExportFilmlist.setText("export");
                jMenuDatei.add(jMenuItemExportFilmlist);

                //---- jMenuItemEinstellungen ----
                jMenuItemEinstellungen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0));
                jMenuItemEinstellungen.setText("Einstellungen");
                jMenuItemEinstellungen.setToolTipText("allgemeine Programmeinstellungen");
                jMenuItemEinstellungen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/programm/menue-einstellungen.png")));
                jMenuDatei.add(jMenuItemEinstellungen);
                jMenuDatei.add(jSeparator2);

                //---- jMenuItemBeenden ----
                jMenuItemBeenden.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, KeyEvent.CTRL_MASK));
                jMenuItemBeenden.setText("Beenden");
                jMenuDatei.add(jMenuItemBeenden);
            }
            jMenuBar.add(jMenuDatei);

            //======== jMenuFilme ========
            {
                jMenuFilme.setMnemonic('F');
                jMenuFilme.setText("Filme");

                //---- jMenuItemFilmAbspielen ----
                jMenuItemFilmAbspielen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, KeyEvent.CTRL_MASK));
                jMenuItemFilmAbspielen.setText("Film abspielen");
                jMenuFilme.add(jMenuItemFilmAbspielen);

                //---- jMenuItemFilmAufzeichnen ----
                jMenuItemFilmAufzeichnen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D, KeyEvent.CTRL_MASK));
                jMenuItemFilmAufzeichnen.setText("Film aufzeichnen");
                jMenuFilme.add(jMenuItemFilmAufzeichnen);

                //---- jMenuItemBlacklist ----
                jMenuItemBlacklist.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B, KeyEvent.CTRL_MASK));
                jMenuItemBlacklist.setText("Blacklist \u00f6ffnen");
                jMenuFilme.add(jMenuItemBlacklist);
                jMenuFilme.add(separator1);

                //---- cbkBeschreibung ----
                cbkBeschreibung.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F10, 0));
                cbkBeschreibung.setText("Beschreibung anzeigen");
                jMenuFilme.add(cbkBeschreibung);
                jMenuFilme.addSeparator();

                //---- jMenuItemFilmeGesehen ----
                jMenuItemFilmeGesehen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, KeyEvent.CTRL_MASK));
                jMenuItemFilmeGesehen.setText("Filme als gesehen markieren");
                jMenuFilme.add(jMenuItemFilmeGesehen);

                //---- jMenuItemFilmeUngesehen ----
                jMenuItemFilmeUngesehen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_MASK));
                jMenuItemFilmeUngesehen.setText("Filme als ungesehen markieren");
                jMenuFilme.add(jMenuItemFilmeUngesehen);

                //---- jMenuItemFilmeMediensammlung ----
                jMenuItemFilmeMediensammlung.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M, KeyEvent.CTRL_MASK));
                jMenuItemFilmeMediensammlung.setText("Titel in der Mediensammlung suchen");
                jMenuFilme.add(jMenuItemFilmeMediensammlung);
            }
            jMenuBar.add(jMenuFilme);

            //======== jMenuDownload ========
            {
                jMenuDownload.setMnemonic('O');
                jMenuDownload.setText("Downloads");

                //---- jMenuItemDownloadsAlleStarten ----
                jMenuItemDownloadsAlleStarten.setText("alle Downloads starten");
                jMenuDownload.add(jMenuItemDownloadsAlleStarten);

                //---- jMenuItemDownloadStartTime ----
                jMenuItemDownloadStartTime.setText("alle Downloads um xx:yy Uhr starten");
                jMenuDownload.add(jMenuItemDownloadStartTime);

                //---- jMenuItemDownloadAlleStoppen ----
                jMenuItemDownloadAlleStoppen.setText("alle stoppen");
                jMenuItemDownloadAlleStoppen.setToolTipText("alle Downloads stoppen");
                jMenuDownload.add(jMenuItemDownloadAlleStoppen);

                //---- jMenuItemDownloadWartendeStoppen ----
                jMenuItemDownloadWartendeStoppen.setText("wartende stoppen");
                jMenuItemDownloadWartendeStoppen.setToolTipText("wartende Downloads stoppen");
                jMenuDownload.add(jMenuItemDownloadWartendeStoppen);

                //---- jMenuItemDownloadsAktualisieren ----
                jMenuItemDownloadsAktualisieren.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W, KeyEvent.CTRL_MASK));
                jMenuItemDownloadsAktualisieren.setText("Liste der Downloads aktualisieren");
                jMenuDownload.add(jMenuItemDownloadsAktualisieren);

                //---- jMenuItemDownloadsAufraeumen ----
                jMenuItemDownloadsAufraeumen.setText("Liste der Downloads aufr\u00e4umen");
                jMenuDownload.add(jMenuItemDownloadsAufraeumen);
                jMenuDownload.addSeparator();

                //---- jMenuItemDownloadStarten ----
                jMenuItemDownloadStarten.setText("Downloads starten");
                jMenuDownload.add(jMenuItemDownloadStarten);

                //---- jMenuItemDownloadStoppen ----
                jMenuItemDownloadStoppen.setText("Downloads stoppen");
                jMenuDownload.add(jMenuItemDownloadStoppen);

                //---- jMenuItemDownloadVorziehen ----
                jMenuItemDownloadVorziehen.setText("Downloads vorziehen");
                jMenuDownload.add(jMenuItemDownloadVorziehen);

                //---- jMenuItemDownloadsZurueckstellen ----
                jMenuItemDownloadsZurueckstellen.setText("Downloads zur\u00fcckstellen");
                jMenuDownload.add(jMenuItemDownloadsZurueckstellen);

                //---- jMenuItemDownloadsLoeschen ----
                jMenuItemDownloadsLoeschen.setText("Downloads aus Liste entfernen");
                jMenuDownload.add(jMenuItemDownloadsLoeschen);

                //---- jMenuItemDownloadAendern ----
                jMenuItemDownloadAendern.setText("Download \u00e4ndern");
                jMenuDownload.add(jMenuItemDownloadAendern);
                jMenuDownload.addSeparator();

                //---- miShowDownloadDescription ----
                miShowDownloadDescription.setText("Filmbeschreibung anzeigen");
                jMenuDownload.add(miShowDownloadDescription);
                jMenuDownload.addSeparator();

                //---- jMenuItemDownloadGesehen ----
                jMenuItemDownloadGesehen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, KeyEvent.CTRL_MASK));
                jMenuItemDownloadGesehen.setText("Filme als gesehen markieren");
                jMenuDownload.add(jMenuItemDownloadGesehen);

                //---- jMenuItemDownloadUngesehen ----
                jMenuItemDownloadUngesehen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_MASK));
                jMenuItemDownloadUngesehen.setText("Filme als ungesehen markieren");
                jMenuDownload.add(jMenuItemDownloadUngesehen);

                //---- jMenuItemDownloadAbspielen ----
                jMenuItemDownloadAbspielen.setText("gespeicherten Film abspielen");
                jMenuDownload.add(jMenuItemDownloadAbspielen);

                //---- jMenuItemDownloadMediensammlung ----
                jMenuItemDownloadMediensammlung.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M, KeyEvent.CTRL_MASK));
                jMenuItemDownloadMediensammlung.setText("Titel in der Mediensammlung suchen");
                jMenuDownload.add(jMenuItemDownloadMediensammlung);

                //---- jMenuItemDownloadInvertSelection ----
                jMenuItemDownloadInvertSelection.setText("Auswahl umkehren");
                jMenuDownload.add(jMenuItemDownloadInvertSelection);
                jMenuDownload.addSeparator();

                //---- jMenuItemDownloadShutDown ----
                jMenuItemDownloadShutDown.setText("Rechner nach Downloads herunterfahren");
                jMenuDownload.add(jMenuItemDownloadShutDown);
            }
            jMenuBar.add(jMenuDownload);

            //======== jMenuAbos ========
            {
                jMenuAbos.setMnemonic('b');
                jMenuAbos.setText("Abos");

                //---- jMenuItemAbosEinschalten ----
                jMenuItemAbosEinschalten.setText("einschalten");
                jMenuAbos.add(jMenuItemAbosEinschalten);

                //---- jMenuItemAbosAusschalten ----
                jMenuItemAbosAusschalten.setText("ausschalten");
                jMenuAbos.add(jMenuItemAbosAusschalten);

                //---- jMenuItemAbosLoeschen ----
                jMenuItemAbosLoeschen.setText("l\u00f6schen");
                jMenuAbos.add(jMenuItemAbosLoeschen);

                //---- jMenuItemAbosAendern ----
                jMenuItemAbosAendern.setText("\u00e4ndern");
                jMenuAbos.add(jMenuItemAbosAendern);

                //---- jMenuItemAboNeu ----
                jMenuItemAboNeu.setText("neues Abo anlegen");
                jMenuItemAboNeu.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/programm/menue-abo-neu.png")));
                jMenuAbos.add(jMenuItemAboNeu);

                //---- jMenuItemAboInvertSelection ----
                jMenuItemAboInvertSelection.setText("Auswahl umkehren");
                jMenuAbos.add(jMenuItemAboInvertSelection);
            }
            jMenuBar.add(jMenuAbos);

            //======== jMenuAnsicht ========
            {
                jMenuAnsicht.setMnemonic('a');
                jMenuAnsicht.setText("Ansicht");

                //---- jCheckBoxMenuItemVideoplayer ----
                jCheckBoxMenuItemVideoplayer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, 0));
                jCheckBoxMenuItemVideoplayer.setText("Buttons anzeigen");
                jMenuAnsicht.add(jCheckBoxMenuItemVideoplayer);

                //======== jMenu1 ========
                {
                    jMenu1.setText("Schriftgr\u00f6\u00dfe");

                    //---- jMenuItemSchriftGr ----
                    jMenuItemSchriftGr.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_ADD, KeyEvent.CTRL_MASK));
                    jMenuItemSchriftGr.setText("vergr\u00f6\u00dfern");
                    jMenu1.add(jMenuItemSchriftGr);

                    //---- jMenuItemSchriftKl ----
                    jMenuItemSchriftKl.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_SUBTRACT, KeyEvent.CTRL_MASK));
                    jMenuItemSchriftKl.setText("verkleinern");
                    jMenu1.add(jMenuItemSchriftKl);

                    //---- jMenuItemSchriftNormal ----
                    jMenuItemSchriftNormal.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_0, KeyEvent.CTRL_MASK));
                    jMenuItemSchriftNormal.setText("Normalgr\u00f6\u00dfe");
                    jMenu1.add(jMenuItemSchriftNormal);
                }
                jMenuAnsicht.add(jMenu1);
                jMenuAnsicht.addSeparator();

                //---- miShowMemoryMonitor ----
                miShowMemoryMonitor.setText("Speicherverbrauch anzeigen");
                jMenuAnsicht.add(miShowMemoryMonitor);

                //---- cbBandwidthDisplay ----
                cbBandwidthDisplay.setText("Bandbreitennutzung");
                jMenuAnsicht.add(cbBandwidthDisplay);

                //---- jCheckBoxMenuItemMediaDb ----
                jCheckBoxMenuItemMediaDb.setText("Mediensammlung durchsuchen");
                jMenuAnsicht.add(jCheckBoxMenuItemMediaDb);
            }
            jMenuBar.add(jMenuAnsicht);

            //======== jMenuHilfe ========
            {
                jMenuHilfe.setMnemonic('h');
                jMenuHilfe.setText("Hilfe");

                //---- jMenuItemShowOnlineHelp ----
                jMenuItemShowOnlineHelp.setText("Online-Hilfe anzeigen");
                jMenuHilfe.add(jMenuItemShowOnlineHelp);
                jMenuHilfe.addSeparator();

                //---- jMenuItemCreateProtocolFile ----
                jMenuItemCreateProtocolFile.setText("Protokolldatei erstellen...");
                jMenuHilfe.add(jMenuItemCreateProtocolFile);

                //---- jMenuItemResetSettings ----
                jMenuItemResetSettings.setText("Einstellungen zur\u00fccksetzen...");
                jMenuHilfe.add(jMenuItemResetSettings);
                jMenuHilfe.addSeparator();

                //---- miSearchForProgramUpdate ----
                miSearchForProgramUpdate.setText("Nach Update suchen...");
                jMenuHilfe.add(miSearchForProgramUpdate);

                //---- miShowProgramInfos ----
                miShowProgramInfos.setText("Programminfos anzeigen...");
                jMenuHilfe.add(miShowProgramInfos);
                jMenuHilfe.addSeparator();

                //---- miUpdateServers ----
                miUpdateServers.setText("Update-Server aktualisieren...");
                jMenuHilfe.add(miUpdateServers);
                jMenuHilfe.add(jSeparatorAboutApplication);

                //---- jMenuItemAboutApplication ----
                jMenuItemAboutApplication.setText("\u00dcber dieses Programm...");
                jMenuHilfe.add(jMenuItemAboutApplication);
            }
            jMenuBar.add(jMenuHilfe);
        }
        setJMenuBar(jMenuBar);

        //======== jPanelCont ========
        {
            jPanelCont.setLayout(new BorderLayout());

            //======== jPanelInfo ========
            {
                jPanelInfo.setLayout(new BorderLayout());
            }
            jPanelCont.add(jPanelInfo, BorderLayout.PAGE_END);

            //======== jTabbedPane ========
            {
                jTabbedPane.setBorder(new EmptyBorder(5, 1, 1, 1));
            }
            jPanelCont.add(jTabbedPane, BorderLayout.CENTER);
        }

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addComponent(jPanelCont, GroupLayout.DEFAULT_SIZE, 398, Short.MAX_VALUE)
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                    .addGap(6, 6, 6)
                    .addComponent(jPanelCont, GroupLayout.DEFAULT_SIZE, 247, Short.MAX_VALUE))
        );
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JMenuBar jMenuBar;
    protected JMenu jMenuDatei;
    private JMenuItem jMenuItemFilmlisteLaden;
    private JMenuItem jMenuItemExportFilmlist;
    protected JMenuItem jMenuItemEinstellungen;
    protected JSeparator jSeparator2;
    protected JMenuItem jMenuItemBeenden;
    private JMenu jMenuFilme;
    protected JMenuItem jMenuItemFilmAbspielen;
    protected JMenuItem jMenuItemFilmAufzeichnen;
    protected JMenuItem jMenuItemBlacklist;
    protected JCheckBoxMenuItem cbkBeschreibung;
    private JMenuItem jMenuItemFilmeGesehen;
    private JMenuItem jMenuItemFilmeUngesehen;
    private JMenuItem jMenuItemFilmeMediensammlung;
    protected JMenu jMenuDownload;
    private JMenuItem jMenuItemDownloadsAlleStarten;
    private JMenuItem jMenuItemDownloadStartTime;
    private JMenuItem jMenuItemDownloadAlleStoppen;
    private JMenuItem jMenuItemDownloadWartendeStoppen;
    private JMenuItem jMenuItemDownloadsAktualisieren;
    private JMenuItem jMenuItemDownloadsAufraeumen;
    private JMenuItem jMenuItemDownloadStarten;
    private JMenuItem jMenuItemDownloadStoppen;
    private JMenuItem jMenuItemDownloadVorziehen;
    private JMenuItem jMenuItemDownloadsZurueckstellen;
    private JMenuItem jMenuItemDownloadsLoeschen;
    private JMenuItem jMenuItemDownloadAendern;
    private JCheckBoxMenuItem miShowDownloadDescription;
    private JMenuItem jMenuItemDownloadGesehen;
    private JMenuItem jMenuItemDownloadUngesehen;
    private JMenuItem jMenuItemDownloadAbspielen;
    private JMenuItem jMenuItemDownloadMediensammlung;
    private JMenuItem jMenuItemDownloadInvertSelection;
    private JMenuItem jMenuItemDownloadShutDown;
    private JMenu jMenuAbos;
    private JMenuItem jMenuItemAbosEinschalten;
    private JMenuItem jMenuItemAbosAusschalten;
    private JMenuItem jMenuItemAbosLoeschen;
    private JMenuItem jMenuItemAbosAendern;
    private JMenuItem jMenuItemAboNeu;
    private JMenuItem jMenuItemAboInvertSelection;
    protected JCheckBoxMenuItem jCheckBoxMenuItemVideoplayer;
    private JMenuItem jMenuItemSchriftGr;
    private JMenuItem jMenuItemSchriftKl;
    private JMenuItem jMenuItemSchriftNormal;
    private JMenuItem miShowMemoryMonitor;
    private JCheckBoxMenuItem cbBandwidthDisplay;
    private JCheckBoxMenuItem jCheckBoxMenuItemMediaDb;
    protected JMenu jMenuHilfe;
    private JMenuItem jMenuItemShowOnlineHelp;
    private JMenuItem jMenuItemCreateProtocolFile;
    protected JMenuItem jMenuItemResetSettings;
    private JMenuItem miSearchForProgramUpdate;
    private JMenuItem miShowProgramInfos;
    private JMenuItem miUpdateServers;
    protected JSeparator jSeparatorAboutApplication;
    protected JMenuItem jMenuItemAboutApplication;
    private JPanel jPanelInfo;
    private JTabbedPane jTabbedPane;
    // End of variables declaration//GEN-END:variables
}
