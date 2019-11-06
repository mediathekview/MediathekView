package mediathek.mainwindow;

import javafx.application.Platform;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;
import javafx.concurrent.WorkerStateEvent;
import javafx.embed.swing.JFXPanel;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.Scene;
import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.Main;
import mediathek.config.*;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeMediaDB;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.filmlisten.FilmeLaden;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.gui.MVTray;
import mediathek.gui.TabPaneIndex;
import mediathek.gui.actions.*;
import mediathek.gui.actions.export.FilmListExportAction;
import mediathek.gui.actions.import_actions.ImportOldAbosAction;
import mediathek.gui.actions.import_actions.ImportOldBlacklistAction;
import mediathek.gui.actions.import_actions.ImportOldReplacementListAction;
import mediathek.gui.bandwidth.BandwidthMonitorController;
import mediathek.gui.dialog.DialogBeenden;
import mediathek.gui.dialog.DialogMediaDB;
import mediathek.gui.dialog.LoadFilmListDialog;
import mediathek.gui.dialog.about.AboutDialog;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.filmInformation.InfoDialog;
import mediathek.gui.messages.*;
import mediathek.gui.messages.mediadb.MediaDbDialogVisibleEvent;
import mediathek.javafx.*;
import mediathek.javafx.tool.FXProgressPane;
import mediathek.res.GetIcon;
import mediathek.tool.*;
import mediathek.tool.threads.IndicatorThread;
import mediathek.update.AutomaticFilmlistUpdate;
import mediathek.update.ProgramUpdateCheck;
import mediathek.update.ProgrammUpdateSuchen;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashMap;
import java.util.NoSuchElementException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

@SuppressWarnings("serial")
public class MediathekGui extends JFrame {

    private static final String ICON_NAME = "MediathekView.png";
    private static final String ICON_PATH = "/mediathek/res/";
    private static final int ICON_WIDTH = 58;
    private static final int ICON_HEIGHT = 58;
    private static final String KEY_F10 = "F10";
    private static final String NONE = "none";
    private static final Logger logger = LogManager.getLogger(MediathekGui.class);
    /**
     * "Pointer" to UI
     */
    private static MediathekGui ui = null;
    /**
     * Number of active downloads
     */
    protected final AtomicInteger numDownloadsStarted = new AtomicInteger(0);
    private final Daten daten = Daten.getInstance();
    private final JMenu jMenuDatei = new JMenu();
    private final JMenu jMenuFilme = new JMenu();
    private final JMenuBar jMenuBar = new JMenuBar();
    private final JMenu jMenuDownload = new JMenu();
    private final JMenu jMenuAbos = new JMenu();
    private final JMenu jMenuAnsicht = new JMenu();
    private final JMenu jMenuHilfe = new JMenu();
    /**
     * this property keeps track how many items are currently selected in the active table view
     */
    private final IntegerProperty selectedItemsProperty = new SimpleIntegerProperty(0);
    /**
     * Helper to determine what tab is currently active
     */
    private final ObjectProperty<TabPaneIndex> tabPaneIndexProperty = new SimpleObjectProperty<>(TabPaneIndex.NONE);
    private final JTabbedPane tabbedPane = new JTabbedPane();
    private final HashMap<JMenu, MenuTabSwitchListener> menuListeners = new HashMap<>();
    private final JCheckBoxMenuItem cbBandwidthDisplay = new JCheckBoxMenuItem("Bandbreitennutzung");
    private final JCheckBoxMenuItem cbSearchMediaDb = new JCheckBoxMenuItem("Mediensammlung durchsuchen");
    private final JFXPanel statusBarPanel = new JFXPanel();
    private final LoadFilmListAction loadFilmListAction;
    private final SearchProgramUpdateAction searchProgramUpdateAction;
    private final MemoryMonitorAction showMemoryMonitorAction = new MemoryMonitorAction();
    public GuiFilme tabFilme;
    public GuiDownloads tabDownloads;
    /**
     * the global configuration for this app.
     */
    protected Configuration config = ApplicationConfiguration.getConfiguration();
    /**
     * Bandwidth monitoring for downloads.
     */
    private BandwidthMonitorController bandwidthMonitor;
    private MVTray tray;
    private DialogEinstellungen dialogEinstellungen;
    private StatusBarController statusBarController;
    private InfoDialog filmInfo; // Infos zum Film
    private ProgramUpdateCheck programUpdateChecker;
    /**
     * Progress indicator thread for macOS and windows.
     */
    private IndicatorThread progressIndicatorThread;
    private DialogMediaDB dialogMediaDB;
    private ManageAboAction manageAboAction;

    public MediathekGui() {
        ui = this;
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

        IconFontSwing.register(FontAwesome.getIconFont());

        loadFilmListAction = new LoadFilmListAction(this);
        searchProgramUpdateAction = new SearchProgramUpdateAction(this);

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_MAINWINDOW));

        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(statusBarPanel, BorderLayout.PAGE_END);

        setIconAndWindowImage();

        createMenuBar();

        remapF10Key();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.WAIT_FOR_HISTORY_DATA));
        try {
            daten.waitForHistoryDataLoadingToComplete();
        } catch (ExecutionException | InterruptedException e) {
            logger.error("waitForHistoryDataLoadingToComplete()", e);
        }

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.CREATE_STATUS_BAR));
        createStatusBar();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.SETUP_FILM_LISTENERS));
        setupFilmListListener();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_TABS));
        initTabs();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.INIT_MENUS));
        initMenus();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_MEDIADB_DIALOG));
        initializeMediaDbDialog();

        //register message bus handler
        daten.getMessageBus().subscribe(this);

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_MEMORY_MONITOR));
        createMemoryMonitor();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_BANDWIDTH_MONITOR));
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE))) {
            getBandwidthMonitorController().setVisibility();
        }

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.FINISHED));

        workaroundControlsFxNotificationBug();

        SwingUtilities.invokeLater(() -> {
            if (Taskbar.isTaskbarSupported())
                setupTaskbarMenu();
        });

        setupSystemTray();

        SwingUtilities.invokeLater(this::setApplicationWindowSize);

        loadFilmlist();

        setupUpdateCheck();

        showVlcHintForAustrianUsers();
    }

    /**
     * Return the user interface instance
     *
     * @return the class instance or null.
     */
    public static MediathekGui ui() {
        return ui;
    }

    private void setupSystemTray() {
        SwingUtilities.invokeLater(() -> {
            initializeSystemTray();
            initWindowListenerForTray();
        });
    }

    public JTabbedPane getTabbedPane() {
        return tabbedPane;
    }

    private BandwidthMonitorController getBandwidthMonitorController() {
        if (bandwidthMonitor == null) {
            bandwidthMonitor = new BandwidthMonitorController(this);
        }

        return bandwidthMonitor;
    }

    private void showVlcHintForAustrianUsers() {
        var thread = new OrfSetupInformationThread();
        daten.getTimerPool().schedule(thread, 10L, TimeUnit.SECONDS);
    }

    private void setupTaskbarMenu() {
        var taskbar = Taskbar.getTaskbar();
        if (taskbar.isSupported(Taskbar.Feature.MENU)) {
            PopupMenu popupMenu = taskbar.getMenu();
            if (popupMenu == null)
                popupMenu = new PopupMenu();

            MenuItem miLoadNewFilmlist = new MenuItem("Neue Filmliste laden");
            miLoadNewFilmlist.addActionListener(e -> performFilmListLoadOperation(false));

            popupMenu.addSeparator();
            popupMenu.add(miLoadNewFilmlist);

            taskbar.setMenu(popupMenu);
        }
    }

    private void setIconAndWindowImage() {
        setWindowTitle();
        setIconImage(GetIcon.getIcon(ICON_NAME, ICON_PATH, ICON_WIDTH, ICON_HEIGHT).getImage());
    }

    private void remapF10Key() {
        //Hier wird F10 default Funktion unterbunden:
        InputMap im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(KeyStroke.getKeyStroke(KEY_F10), NONE);
    }

    private void createMenuBar() {
        jMenuDatei.setMnemonic('d');
        jMenuDatei.setText("Datei");
        jMenuBar.add(jMenuDatei);

        jMenuFilme.setMnemonic('F');
        jMenuFilme.setText("Filme");
        jMenuBar.add(jMenuFilme);

        jMenuDownload.setMnemonic('O');
        jMenuDownload.setText("Downloads");
        jMenuBar.add(jMenuDownload);

        jMenuAbos.setMnemonic('b');
        jMenuAbos.setText("Abos");
        jMenuBar.add(jMenuAbos);

        jMenuAnsicht.setMnemonic('a');
        jMenuAnsicht.setText("Ansicht");
        jMenuBar.add(jMenuAnsicht);

        jMenuHilfe.setMnemonic('h');
        jMenuHilfe.setText("Hilfe");
        jMenuBar.add(jMenuHilfe);

        setJMenuBar(jMenuBar);
    }

    /**
     * ControlsFX Notifications expect a stage to be open.
     * Create a utility window hidden and transparent as a stage for them.
     */
    protected void workaroundControlsFxNotificationBug() {
        //does not work on windows and linux
    }

    private void createMemoryMonitor() {
        if (Config.isDebugModeEnabled())
            showMemoryMonitorAction.showMemoryMonitor();
    }

    /**
     * Read a local filmlist or load a new one in auto mode.
     */
    private void loadFilmlist() {
        Platform.runLater(() -> {
            //don´t write filmlist when we are reading only...
            if (GuiFunktionen.getImportArtFilme() == FilmListUpdateType.AUTOMATIC && daten.getListeFilme().isTooOld()) {
                Daten.dontWriteFilmlistOnStartup.set(false);
            } else
                Daten.dontWriteFilmlistOnStartup.set(true);

            FXProgressPane progressPane = new FXProgressPane();

            FilmListReaderTask filmListReaderTask = new FilmListReaderTask();
            filmListReaderTask.setOnRunning(e -> {
                getStatusBarController().getStatusBar().getRightItems().add(progressPane);
                progressPane.bindTask(filmListReaderTask);
            });

            FilmListNetworkReaderTask networkTask = new FilmListNetworkReaderTask();
            networkTask.setOnRunning(e -> progressPane.bindTask(networkTask));

            FilmListFilterTask filterTask = new FilmListFilterTask(true);
            filterTask.setOnRunning(e -> progressPane.bindTask(filterTask));
            final EventHandler<WorkerStateEvent> workerStateEventEventHandler = e -> getStatusBarController().getStatusBar().getRightItems().remove(progressPane);
            filterTask.setOnSucceeded(workerStateEventEventHandler);
            filterTask.setOnFailed(workerStateEventEventHandler);

            CompletableFuture.runAsync(filmListReaderTask)
                    .thenRun(networkTask)
                    .thenRun(filterTask);

            //reset after first load has happened
            Daten.dontWriteFilmlistOnStartup.set(false);
        });
    }

    public DialogMediaDB getMediaDatabaseDialog() {
        if (dialogMediaDB == null) {
            dialogMediaDB = new DialogMediaDB(this);
        }
        return dialogMediaDB;
    }

    private void initializeMediaDbDialog() {
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN))) {
            getMediaDatabaseDialog().setVis();
        }
    }

    public IntegerProperty getSelectedItemsProperty() {
        return selectedItemsProperty;
    }

    /**
     * Create the status bar item.
     */
    private void createStatusBar() {
        statusBarController = new StatusBarController(daten);

        Platform.runLater(() -> {
            statusBarPanel.setScene(new Scene(statusBarController.createStatusBar()));
            installSelectedItemsLabel();
        });
    }

    private void installSelectedItemsLabel() {
        ObservableList<Node> leftItems = statusBarController.getStatusBar().getLeftItems();
        leftItems.add(0, new SelectedItemsLabel(selectedItemsProperty));
        leftItems.add(1, new VerticalSeparator());
    }

    public StatusBarController getStatusBarController() {
        return statusBarController;
    }

    public ObjectProperty<TabPaneIndex> tabPaneIndexProperty() {
        return tabPaneIndexProperty;
    }

    public InfoDialog getFilmInfoDialog() {
        if (filmInfo == null)
            filmInfo = new InfoDialog(this);

        return filmInfo;
    }

    @Handler
    private void handleMediaDbDialogEvent(MediaDbDialogVisibleEvent e) {
        SwingUtilities.invokeLater(() -> cbSearchMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN))));
    }

    @Handler
    private void handleTabVisualSettingsChangedEvent(TabVisualSettingsChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            configureTabPlacement();
            configureTabIcons();
        });
    }

    @Handler
    private void handleBandwidthMonitorStateChangedEvent(BandwidthMonitorStateChangedEvent e) {
        SwingUtilities.invokeLater(() -> cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE))));
    }

    private void setWindowTitle() {
        setTitle(Konstanten.PROGRAMMNAME + ' ' + Konstanten.MVVERSION);
    }

    /**
     * load window size and position from config file.
     * If values aren´t found just maximize the window.
     */
    private void restoreSizeFromConfig() {
        /*
        We are not in maximized mode, so just read all the settings and restore...
         */
        var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.READ);
            int width = config.getInt(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_WIDTH);
            int height = config.getInt(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_HEIGHT);
            int x = config.getInt(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_LOCATION_X);
            int y = config.getInt(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_LOCATION_Y);

            setBounds(x, y, width, height);
        } catch (NoSuchElementException e) {
            //in case of any error, just make the window maximized
            setExtendedState(JFrame.MAXIMIZED_BOTH);
        } finally {
            config.unlock(LockMode.READ);
        }
    }

    private void setApplicationWindowSize() {
        if (Config.isStartMaximized() ||
                ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_MAXIMIZED, true)) {
            setExtendedState(JFrame.MAXIMIZED_BOTH);
        } else
            restoreSizeFromConfig();

        SwingUtilities.invokeLater(() -> addComponentListener(new WindowLocationConfigSaverListener()));
    }

    private void setupFilmListListener() {
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                loadFilmListAction.setEnabled(false);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                loadFilmListAction.setEnabled(true);
                daten.allesSpeichern(); // damit nichts verlorengeht
            }

            @Override
            public void fertigOnlyOne(ListenerFilmeLadenEvent event) {
                setupAutomaticFilmlistReload();
                prepareMediaDb();
            }
        });
    }

    /**
     * Reload filmlist every 24h when in automatic mode.
     */
    private void setupAutomaticFilmlistReload() {
        final AutomaticFilmlistUpdate.IUpdateAction performUpdate = () -> {
            if (GuiFunktionen.getImportArtFilme() == FilmListUpdateType.AUTOMATIC) {
                //if downloads are running, don´t update
                if (daten.getListeDownloads().unfinishedDownloads() == 0) {
                    FilmeLaden filmeLaden = new FilmeLaden(daten);
                    filmeLaden.loadFilmlist("");
                }
            }
        };

        AutomaticFilmlistUpdate automaticFilmlistUpdate = new AutomaticFilmlistUpdate(performUpdate);
        automaticFilmlistUpdate.start();
    }

    protected void initWindowListenerForTray() {
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                if (tray != null && Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))) {
                    setVisible(false);
                } else {
                    beenden(false, false);
                }
            }
        });
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

    public void initializeSystemTray() {
        final var useTray = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY));
        if (tray == null && useTray) {
            tray = new MVTray().systemTray();
        } else if (tray != null && !useTray) {
            tray.beenden();
            tray = null;
        }
    }

    private void initTabs() {
        Container contentPane = getContentPane();
        contentPane.add(tabbedPane, BorderLayout.CENTER);

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_DOWNLOAD_TAB));
        tabDownloads = new GuiDownloads(daten, this);

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_FILM_TAB));
        tabFilme = new GuiFilme(daten, this);

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.ADD_TABS_TO_UI));
        tabbedPane.addTab(GuiFilme.NAME, tabFilme);
        tabbedPane.addTab(GuiDownloads.NAME, tabDownloads);
        tabbedPane.setSelectedIndex(0);

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.CONFIGURE_TABS));
        configureTabPlacement();
        configureTabIcons();
    }

    /**
     * Enable/Disable the update related menu item.
     *
     * @param enable Shall the menu item be enabled?
     */
    public void enableUpdateMenuItem(boolean enable) {
        searchProgramUpdateAction.setEnabled(enable);
    }

    /**
     * Change placement of tabs based on settings
     */
    private void configureTabPlacement() {
        final boolean topPosition = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_TAB_POSITION_TOP, true);
        if (topPosition)
            tabbedPane.setTabPlacement(JTabbedPane.TOP);
        else
            tabbedPane.setTabPlacement(JTabbedPane.LEFT);
    }

    private void configureTabIcons() {
        final boolean icon = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_TAB_ICONS,false);

        //no icons...
        if (!icon) {
            setTabIcon(tabFilme, null);
            setTabIcon(tabDownloads, null);
        } else {
            //setup icons for each tab here
            setTabIcon(tabFilme, Icons.ICON_TAB_FILM);
            setTabIcon(tabDownloads, Icons.ICON_TAB_DOWNLOAD);
        }
    }

    private void setTabIcon(Component tab, Icon icon) {
        final int index = tabbedPane.indexOfComponent(tab);
        tabbedPane.setIconAt(index, icon);
    }

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

    /**
     * Install the listeners which will cause automatic tab switching based on associated Menu item.
     */
    protected void installMenuTabSwitchListener() {
        //initial setup
        menuListeners.put(jMenuFilme, new MenuTabSwitchListener(this, TABS.TAB_FILME));
        menuListeners.put(jMenuDownload, new MenuTabSwitchListener(this, TABS.TAB_DOWNLOADS));

        //now assign if really necessary
        if (config.getBoolean(ApplicationConfiguration.APPLICATION_INSTALL_TAB_SWITCH_LISTENER, true)) {
            jMenuFilme.addMenuListener(menuListeners.get(jMenuFilme));
            jMenuDownload.addMenuListener(menuListeners.get(jMenuDownload));
        }
    }

    /**
     * Handle the install/or remove event sent from settings dialog
     */
    @Handler
    protected void handleInstallTabSwitchListenerEvent(InstallTabSwitchListenerEvent msg) {
        switch (msg.event) {
            case INSTALL:
                SwingUtilities.invokeLater(() -> {
                    jMenuFilme.addMenuListener(menuListeners.get(jMenuFilme));
                    jMenuDownload.addMenuListener(menuListeners.get(jMenuDownload));
                });
                break;

            case REMOVE:
                SwingUtilities.invokeLater(() -> {
                    jMenuFilme.removeMenuListener(menuListeners.get(jMenuFilme));
                    jMenuDownload.removeMenuListener(menuListeners.get(jMenuDownload));
                });
                break;
        }
    }

    private void createFileMenu() {
        jMenuDatei.add(loadFilmListAction);
        jMenuDatei.addSeparator();
        var exportMenu = new JMenu("Export");
        exportMenu.add(new FilmListExportAction(this));

        var importMenu = new JMenu("Import");
        importMenu.add(new ImportOldAbosAction());
        importMenu.add(new ImportOldBlacklistAction());
        importMenu.add(new ImportOldReplacementListAction());

        jMenuDatei.add(exportMenu);
        jMenuDatei.add(importMenu);

        //on macOS we will use native handlers instead...
        if (!SystemUtils.IS_OS_MAC_OSX) {
            jMenuDatei.addSeparator();
            jMenuDatei.add(new SettingsAction(this));
            jMenuDatei.addSeparator();
            jMenuDatei.add(new QuitAction(this));
        }
    }

    private void createViewMenu() {
        JCheckBoxMenuItem cbShowButtons = new JCheckBoxMenuItem("Buttons anzeigen");
        if (!SystemUtils.IS_OS_MAC_OSX)
            cbShowButtons.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F11, 0));
        cbShowButtons.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
        cbShowButtons.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN, String.valueOf(cbShowButtons.isSelected()));
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName());
        });

        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                cbShowButtons.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN)));
            }
        });

        cbBandwidthDisplay.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE)));
        cbBandwidthDisplay.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(cbBandwidthDisplay.isSelected()));
            getBandwidthMonitorController().setVisibility();
        });

        cbSearchMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
        cbSearchMediaDb.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, String.valueOf(cbSearchMediaDb.isSelected()));
            getMediaDatabaseDialog().setVis();
        });

        jMenuAnsicht.add(cbShowButtons);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(showMemoryMonitorAction);
        jMenuAnsicht.add(cbBandwidthDisplay);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(new ShowFilmInformationAction(true));
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(cbSearchMediaDb);
    }

    private void createHelpMenu() {
        jMenuHilfe.add(new ShowOnlineHelpAction());
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new CreateProtocolFileAction());
        jMenuHilfe.add(new ResetSettingsAction(this, daten));
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(searchProgramUpdateAction);
        jMenuHilfe.add(new ShowProgramInfosAction(this));
        if (!SystemUtils.IS_OS_MAC_OSX) {
            jMenuHilfe.addSeparator();
            jMenuHilfe.add(new ShowAboutAction(this));
        }
    }

    protected void initMenus() {
        installMenuTabSwitchListener();

        createFileMenu();
        tabFilme.installMenuEntries(jMenuFilme);
        tabDownloads.installMenuEntries(jMenuDownload);

        createViewMenu();
        createAboMenu();
        createHelpMenu();
    }

    private void createAboMenu() {
        jMenuAbos.add(new CreateNewAboAction(daten.getListeAbo()));
        jMenuAbos.add(new ShowAboHistoryAction(MediathekGui.ui(), daten));
        jMenuAbos.addSeparator();
        manageAboAction = new ManageAboAction(daten);
        tabFilme.fap.manageAboAction = manageAboAction;
        jMenuAbos.add(manageAboAction);
    }

    /**
     * Display the About Box
     */
    public void showAboutDialog() {
        AboutDialog dialog = new AboutDialog(this);
        GuiFunktionen.centerOnScreen(dialog, false);
        dialog.setVisible(true);
        dialog.dispose();
    }

    public void performFilmListLoadOperation(boolean manualMode) {
        if (manualMode || GuiFunktionen.getImportArtFilme() == FilmListUpdateType.MANUAL) {
            // Dialog zum Laden der Filme anzeigen
            LoadFilmListDialog dlg = new LoadFilmListDialog(this);
            dlg.setVisible(true);
        } else {
            // Filme werden automatisch geladen
            FilmeLaden filmeLaden = new FilmeLaden(daten);
            filmeLaden.loadFilmlist("");
        }
    }

    private DialogEinstellungen getSettingsDialog() {
        if (dialogEinstellungen == null) {
            dialogEinstellungen = new DialogEinstellungen();
        }

        return dialogEinstellungen;
    }

    public void showSettingsDialog() {
        getSettingsDialog().setVisible(true);
    }

    private void writeOldConfiguration() {
        // Infodialog/Bandwidth
        if (bandwidthMonitor != null)
            bandwidthMonitor.writeConfig();

        // MediaDB
        if (dialogMediaDB != null)
            GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_GROESSE, getMediaDatabaseDialog());
    }

    protected void closeControlsFxWorkaroundStage() {
        //not used on windows and linux
    }

    public boolean beenden(boolean showOptionTerminate, boolean shutDown) {
        if (daten.getListeDownloads().unfinishedDownloads() > 0) {
            // erst mal prüfen ob noch Downloads laufen
            DialogBeenden dialogBeenden = new DialogBeenden(this);
            if (showOptionTerminate) {
                dialogBeenden.setComboWaitAndTerminate();
            }

            dialogBeenden.setVisible(true);
            if (!dialogBeenden.applicationCanTerminate()) {
                return false;
            }
            shutDown = dialogBeenden.isShutdownRequested();
        }

        showMemoryMonitorAction.closeMemoryMonitor();

        closeControlsFxWorkaroundStage();

        programUpdateChecker.close();

        ShutdownDialog dialog = new ShutdownDialog(this, 9);
        dialog.show();

        manageAboAction.closeDialog();

        dialog.setStatusText(1, "Beende Threadpools");
        shutdownTimerPool();
        waitForCommonPoolToComplete();

        // Tabelleneinstellungen merken
        dialog.setStatusText(2, "Film-Daten sichern");
        tabFilme.tabelleSpeichern();

        dialog.setStatusText(3, "Download-Daten sichern");
        tabDownloads.tabelleSpeichern();

        dialog.setStatusText(4, "MediaDB sichern");
        if (dialogMediaDB != null)
            getMediaDatabaseDialog().tabelleSpeichern();

        dialog.setStatusText(5, "Downloads anhalten");
        stopDownloads();

        dialog.setStatusText(6, "Programmkonfiguration schreiben");
        writeOldConfiguration();

        if (MemoryUtils.isLowMemoryEnvironment()) {
            dialog.setStatusText(7, "Datenbank schließen");
            DatenFilm.Database.closeDatabase();
        }

        dialog.setStatusText(8, "Programmdaten sichern");
        daten.allesSpeichern();

        dialog.setStatusText(9, "Fertig.");
        dialog.hide();

        tabFilme.fap.filterDialog.dispose();

        if (bandwidthMonitor != null)
            bandwidthMonitor.close();

        Log.endMsg();

        if (shutDown) {
            shutdownComputer();
        }

        dispose();

        //write all settings if not done already...
        ApplicationConfiguration.getInstance().writeConfiguration();

        //shutdown JavaFX
        Platform.exit();

        System.exit(0);

        return false;
    }

    private void shutdownTimerPool() {
        var timerPool = daten.getTimerPool();
        timerPool.shutdown();
        try {
            timerPool.awaitTermination(1, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            logger.warn("timerPool shutdown exception", e);
        }
        var taskList = timerPool.shutdownNow();
        if (!taskList.isEmpty()) {
            logger.trace("timerPool taskList was not empty");
            logger.trace(taskList.toString());
        }
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
        //default is none
    }

    public void searchForUpdateOrShowProgramInfos(boolean infos) {
        new ProgrammUpdateSuchen().checkVersion(!infos, infos, false);
    }
}
