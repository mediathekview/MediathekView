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
import javafx.scene.control.Alert;
import javafx.stage.Modality;
import javafx.stage.Stage;
import mediathek.Main;
import mediathek.config.*;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeMediaDB;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.filmlisten.FilmeLaden;
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
import mediathek.gui.history.ResetAboHistoryAction;
import mediathek.gui.history.ResetDownloadHistoryAction;
import mediathek.gui.messages.*;
import mediathek.gui.messages.mediadb.MediaDbDialogVisibleEvent;
import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.javafx.*;
import mediathek.javafx.tool.FXProgressPane;
import mediathek.javafx.tool.JFXHiddenApplication;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.res.GetIcon;
import mediathek.tool.*;
import mediathek.tool.notification.GenericNotificationCenter;
import mediathek.tool.notification.NullNotificationCenter;
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
import org.jetbrains.annotations.NotNull;

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

import static mediathek.tool.ApplicationConfiguration.CONFIG_AUTOMATIC_UPDATE_CHECK;

public class MediathekGui extends JFrame {

    private static final String ICON_NAME = "MediathekView.png";
    private static final String ICON_PATH = "/mediathek/res/";
    private static final int ICON_WIDTH = 58;
    private static final int ICON_HEIGHT = 58;
    private static final String KEY_F10 = "F10";
    private static final String NONE = "none";
    private static final int MIN_WINDOW_WIDTH = 800;
    private static final int MIN_WINDOW_HEIGHT = 600;
    protected static Logger logger = LogManager.getLogger(MediathekGui.class);
    /**
     * "Pointer" to UI
     */
    private static MediathekGui ui;
    /**
     * Number of active downloads
     */
    protected final AtomicInteger numDownloadsStarted = new AtomicInteger(0);
    protected final Daten daten = Daten.getInstance();
    protected final JTabbedPane tabbedPane = new JTabbedPane();
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
     * Progress indicator thread for OS X and windows.
     */
    private IndicatorThread progressIndicatorThread;
    private DialogMediaDB dialogMediaDB;
    private ManageAboAction manageAboAction;
    private AutomaticFilmlistUpdate automaticFilmlistUpdate;

    public MediathekGui() {
        ui = this;
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

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
        if (config.getBoolean(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, false)) {
            getBandwidthMonitorController().setVisibility();
        }

        setupNotificationCenter();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.FINISHED));

        workaroundJavaFxInitializationBug();

        SwingUtilities.invokeLater(() -> {
            if (Taskbar.isTaskbarSupported())
                setupTaskbarMenu();
        });

        setupSystemTray();

        SwingUtilities.invokeLater(this::setApplicationWindowSize);

        loadFilmlist();

        setupUpdateCheck(config.getBoolean(CONFIG_AUTOMATIC_UPDATE_CHECK, true));

        showVlcHintForAustrianUsers();

        setupShutdownHook();

        checkInvalidRegularExpressions();
    }

    /**
     * Return the user interface instance
     *
     * @return the class instance or null.
     */
    public static MediathekGui ui() {
        return ui;
    }

    /**
     * Check if we encountered invalid regexps and warn user if necessary.
     * This needs to be delayed unfortunately as we can see result only after table has been filled.
     * So we simply wait 30 seconds until we check.
     */
    private void checkInvalidRegularExpressions() {
        Daten.getInstance().getTimerPool().schedule(() -> {
            if (Filter.regExpErrorsOccured()) {
                final var regexStr = Filter.regExpErrorList.stream()
                        .reduce("", (p, e) ->
                                p + "\n" + e);
                final var message = String.format("""
                        Während des Starts wurden ungültige reguläre Ausdrücke (RegExp) in Ihrer Blacklist und/oder Abos entdeckt.
                        Sie müssen diese korrigieren, ansonsten funktioniert das Programm nicht fehlerfrei!
                                                
                        Nachfolgende Ausdrücke sind fehlerbehaftet: %s
                        """, regexStr);
                Filter.regExpErrorList.clear();

                Platform.runLater(() -> {
                    Alert alert = new Alert(Alert.AlertType.ERROR);
                    alert.setTitle(Konstanten.PROGRAMMNAME);
                    alert.setHeaderText("Ungültige reguläre Ausdrücke gefunden");
                    alert.setContentText(message);
                    alert.initModality(Modality.APPLICATION_MODAL);
                    alert.show();
                });
            }
        }, 30, TimeUnit.SECONDS);
    }

    /**
     * Create either a native or a javafx notification center depending on platform
     */
    protected void setupNotificationCenter() {
        final boolean showNotifications = config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS, true);

        config.setProperty(ApplicationConfiguration.APPLICATION_NATIVE_NOTIFICATIONS_SUPPORT, false);
        config.setProperty(ApplicationConfiguration.APPLICATION_SHOW_NATIVE_NOTIFICATIONS, false);

        if (!showNotifications) {
            daten.setNotificationCenter(new NullNotificationCenter());
        } else {
            daten.setNotificationCenter(new GenericNotificationCenter());
        }
    }

    @Handler
    protected void handleNotificationCenterChangeEvent(NotificationCenterChangeEvent e) {
        setupNotificationCenter();
    }

    protected void closeNotificationCenter() {
        try {
            var center = daten.notificationCenter();
            if (center != null)
                center.close();
        } catch (Exception ignored) {
        }
    }

    /**
     * This shutdown hook will try to save both log messages and write config changes to disk before app terminates.
     */
    private void setupShutdownHook() {
        Runtime.getRuntime().addShutdownHook(new ShutdownHookThread());
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
     * JavaFX seems to need at least one window shown in order to function without further problems.
     * This is imminent on macOS, but seems to affect windows as well.
     */
    protected void workaroundJavaFxInitializationBug() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            /*
            For some unknown reason JavaFX seems to get confused on macOS when no stage was at least once
            really visible. This will cause swing/javafx mixed windows to have focus trouble and/or use 100%
            cpu when started in background.
            Workaround for now is to open a native javafx stage, display it for the shortest time possible and
            then close it as we don´t need it. On my machine this fixes the focus and cpu problems.
             */
            var window = new Stage();
            window.setWidth(10d);
            window.setHeight(10d);
            window.show();
            window.hide();
        });
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
            if (GuiFunktionen.getImportArtFilme() == FilmListUpdateType.AUTOMATIC && daten.getListeFilme().needsUpdate()) {
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

        JavaFxUtils.invokeInFxThreadAndWait(() -> {
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
        final var vis = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, false);
        SwingUtilities.invokeLater(() -> cbBandwidthDisplay.setSelected(vis));
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
            int width = config.getInt(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_WIDTH, MIN_WINDOW_WIDTH);
            int height = config.getInt(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_HEIGHT, MIN_WINDOW_HEIGHT);
            int x = config.getInt(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_LOCATION_X);
            int y = config.getInt(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_LOCATION_Y);

            if (width < MIN_WINDOW_WIDTH)
                width = MIN_WINDOW_WIDTH;
            if (height < MIN_WINDOW_HEIGHT)
                height = MIN_WINDOW_HEIGHT;

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
                    filmeLaden.loadFilmlist("", false);
                }
            }
        };

        automaticFilmlistUpdate = new AutomaticFilmlistUpdate(performUpdate);
        automaticFilmlistUpdate.start();
    }

    protected void initWindowListenerForTray() {
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                if (tray != null && config.getBoolean(ApplicationConfiguration.APPLICATION_UI_USE_TRAY, false)) {
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

    @Handler
    private void handleUpdateStateChanged(UpdateStateChangedEvent e) {
        SwingUtilities.invokeLater(() -> setupUpdateCheck(e.isActive()));
    }

    /**
     * This creates a repeating update check every 24 hours.
     */
    private void setupUpdateCheck(boolean newState) {
        if (newState) {
            programUpdateChecker = new ProgramUpdateCheck(daten);
            programUpdateChecker.start();
        } else {
            endProgramUpdateChecker();
        }
    }

    private void endProgramUpdateChecker() {
        if (programUpdateChecker != null) {
            programUpdateChecker.close();
            programUpdateChecker = null;
        }
    }

    public void initializeSystemTray() {
        final var useTray = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_USE_TRAY, false);
        if (tray == null && useTray) {
            tray = new MVTray().systemTray();
        } else if (tray != null && !useTray) {
            tray.beenden();
            tray = null;
        }
    }

    protected JPanel createTabFilme(@NotNull Daten daten) {
        return new GuiFilme(daten, this);
    }

    protected JPanel createTabDownloads(@NotNull Daten daten) {
        return new GuiDownloads(daten, this);
    }

    private void initTabs() {
        Container contentPane = getContentPane();
        contentPane.add(tabbedPane, BorderLayout.CENTER);

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_DOWNLOAD_TAB));
        tabDownloads = (GuiDownloads) createTabDownloads(daten);

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_FILM_TAB));
        tabFilme = (GuiFilme) createTabFilme(daten);

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.ADD_TABS_TO_UI));
        tabbedPane.addTab(GuiFilme.NAME, tabFilme);
        tabbedPane.addTab(GuiDownloads.NAME, tabDownloads);
        tabbedPane.setSelectedComponent(tabFilme);

        installTouchBarSupport();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.CONFIGURE_TABS));
        configureTabPlacement();
        configureTabIcons();
    }

    /**
     * Install touch bar support on macOS
     */
    protected void installTouchBarSupport() {
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
        final boolean icon = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_TAB_ICONS, false);

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
            case INSTALL -> SwingUtilities.invokeLater(() -> {
                jMenuFilme.addMenuListener(menuListeners.get(jMenuFilme));
                jMenuDownload.addMenuListener(menuListeners.get(jMenuDownload));
            });
            case REMOVE -> SwingUtilities.invokeLater(() -> {
                jMenuFilme.removeMenuListener(menuListeners.get(jMenuFilme));
                jMenuDownload.removeMenuListener(menuListeners.get(jMenuDownload));
            });
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
        cbBandwidthDisplay.setSelected(config.getBoolean(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, false));
        cbBandwidthDisplay.addActionListener(e -> {
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, cbBandwidthDisplay.isSelected());
            getBandwidthMonitorController().setVisibility();
        });

        cbSearchMediaDb.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
        cbSearchMediaDb.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, String.valueOf(cbSearchMediaDb.isSelected()));
            getMediaDatabaseDialog().setVis();
        });

        JMenuItem showFilmFilterDialog = new JMenuItem("Filterdialog anzeigen");
        showFilmFilterDialog.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F12, 0));
        showFilmFilterDialog.addActionListener(l -> {
            var dlg = tabFilme.fap.filterDialog;
            if (dlg != null) {
                if (!dlg.isVisible()) {
                    dlg.setVisible(true);
                }
            }

        });

        JMenuItem showBookmarkList = new JMenuItem("Merkliste anzeigen");
        showBookmarkList.addActionListener(l -> JavaFxUtils.invokeInFxThreadAndWait(() -> tabFilme.showBookmarkWindow()));

        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(showMemoryMonitorAction);
        jMenuAnsicht.add(cbBandwidthDisplay);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(showFilmFilterDialog);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(new ShowFilmInformationAction(true));
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(showBookmarkList);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(cbSearchMediaDb);
    }

    private void createHelpMenu() {
        jMenuHilfe.add(new ShowOnlineHelpAction());
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new ResetSettingsAction(this, daten));
        jMenuHilfe.add(new ResetDownloadHistoryAction(this));
        jMenuHilfe.add(new ResetAboHistoryAction(this));
        jMenuHilfe.addSeparator();
        //do not show menu entry if we have external update support
        var externalUpdateCheck = System.getProperty(Konstanten.EXTERNAL_UPDATE_PROPERTY);
        if (externalUpdateCheck == null || !externalUpdateCheck.equalsIgnoreCase("true")) {
            jMenuHilfe.add(searchProgramUpdateAction);
        }
        jMenuHilfe.add(new ShowProgramInfosAction(this));
        if (officialLauncherInUse()) {
            jMenuHilfe.addSeparator();
            jMenuHilfe.add(new SetAppMemoryAction());
        }
        installAdditionalHelpEntries();
    }

    /**
     * Test if MediathekView is launched by our official downloads.
     * @return true if official binary is used, false otherwise.
     */
    protected boolean officialLauncherInUse() {
        boolean winBinaryInUse = true;
        final var externalUpdateCheck = System.getProperty(Konstanten.EXTERNAL_UPDATE_PROPERTY);
        if (externalUpdateCheck == null || !externalUpdateCheck.equalsIgnoreCase("true")) {
            winBinaryInUse = false;
        }

        return winBinaryInUse;
    }

    protected void installAdditionalHelpEntries() {
        jMenuHilfe.addSeparator();
        var action = new ChangeGlobalFontSetting();
        var item = jMenuHilfe.add(action);
        action.setMenuItem(item);

        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new ShowAboutAction(this));
    }

    protected void initMenus() {
        installMenuTabSwitchListener();

        createFileMenu();
        tabFilme.installMenuEntries(jMenuFilme);
        tabDownloads.installMenuEntries(jMenuDownload);

        createViewMenu();
        tabFilme.installViewMenuEntry(jMenuAnsicht);

        createAboMenu();
        createHelpMenu();
    }

    private void createAboMenu() {
        jMenuAbos.add(new CreateNewAboAction(daten.getListeAbo()));
        jMenuAbos.add(new ShowAboHistoryAction(MediathekGui.ui(), daten));
        jMenuAbos.addSeparator();
        manageAboAction = new ManageAboAction();
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
            filmeLaden.loadFilmlist("", false);
        }
    }

    public DialogEinstellungen getSettingsDialog() {
        if (dialogEinstellungen == null) {
            dialogEinstellungen = new DialogEinstellungen();
        }

        return dialogEinstellungen;
    }

    private void writeOldConfiguration() {
        // Infodialog/Bandwidth
        if (bandwidthMonitor != null)
            bandwidthMonitor.writeConfig();

        // MediaDB
        if (dialogMediaDB != null)
            GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_GROESSE, getMediaDatabaseDialog());
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

        if (automaticFilmlistUpdate != null)
            automaticFilmlistUpdate.close();

        showMemoryMonitorAction.closeMemoryMonitor();

        endProgramUpdateChecker();

        ShutdownDialog dialog = new ShutdownDialog(this);
        dialog.show();

        dialog.setStatusText(ShutdownState.SHUTDOWN_NOTIFICATION_CENTER);
        closeNotificationCenter();

        manageAboAction.closeDialog();

        tabFilme.saveSettings();  // needs thread pools active!

        dialog.setStatusText(ShutdownState.SHUTDOWN_THREAD_POOL);
        shutdownTimerPool();
        waitForCommonPoolToComplete();

        dialog.setStatusText(ShutdownState.PERFORM_SEEN_HISTORY_MAINTENANCE);
        try (SeenHistoryController history = new SeenHistoryController()) {
            history.performMaintenance();
        }

        // Tabelleneinstellungen merken
        dialog.setStatusText(ShutdownState.SAVE_FILM_DATA);
        tabFilme.tabelleSpeichern();

        dialog.setStatusText(ShutdownState.SAVE_DOWNLOAD_DATA);
        tabDownloads.tabelleSpeichern();

        dialog.setStatusText(ShutdownState.SAVE_MEDIA_DB);
        if (dialogMediaDB != null)
            getMediaDatabaseDialog().tabelleSpeichern();

        dialog.setStatusText(ShutdownState.STOP_DOWNLOADS);
        stopDownloads();

        dialog.setStatusText(ShutdownState.SAVE_CONFIG);
        writeOldConfiguration();

        dialog.setStatusText(ShutdownState.SAVE_BOOKMARKS);
        daten.getListeBookmarkList().saveToFile(Daten.getBookmarkFilePath());

        dialog.setStatusText(ShutdownState.CLOSE_DB);
        if (MemoryUtils.isLowMemoryEnvironment()) {
            DatenFilm.Database.closeDatabase();
        }

        dialog.setStatusText(ShutdownState.SAVE_APP_DATA);
        daten.allesSpeichern();

        dialog.setStatusText(ShutdownState.COMPLETE);
        dialog.hide();

        tabFilme.fap.filterDialog.dispose();

        if (bandwidthMonitor != null)
            bandwidthMonitor.close();

        Log.printRuntimeStatistics();

        dispose();

        JavaFxUtils.invokeInFxThreadAndWait(() -> JFXHiddenApplication.getPrimaryStage().close());

        //write all settings if not done already...
        ApplicationConfiguration.getInstance().writeConfiguration();

        //shutdown JavaFX
        Platform.exit();

        if (shutDown) {
            shutdownComputer();
        }

        System.exit(0);

        return false;
    }

    private void shutdownTimerPool() {
        var timerPool = daten.getTimerPool();
        timerPool.shutdown();
        try {
            if (!timerPool.awaitTermination(3, TimeUnit.SECONDS))
                logger.warn("Time out occured before pool final termination");
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

    /**
     * Gracefully shutdown config and log.
     * This may be necessary in case the app is not properly quit.
     */
    static class ShutdownHookThread extends Thread {
        @Override
        public void run() {
            //write all settings if not done already...just to be sure
            ApplicationConfiguration.getInstance().writeConfiguration();

            //shut down log4j
            Log4jShutdownCallbackRegistry.Companion.execute();
        }
    }
}
