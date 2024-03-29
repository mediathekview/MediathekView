package mediathek.mainwindow;

import com.sun.jna.platform.win32.VersionHelpers;
import javafx.application.Platform;
import javafx.stage.Stage;
import mediathek.Main;
import mediathek.config.*;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.IndexedFilmList;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.filmlisten.FilmeLaden;
import mediathek.filmlisten.reader.FilmListReader;
import mediathek.gui.MVTray;
import mediathek.gui.actions.*;
import mediathek.gui.actions.export.ExportDecompressedFilmlistAction;
import mediathek.gui.actions.export.ExportReadableFilmlistAction;
import mediathek.gui.actions.import_actions.ImportOldAbosAction;
import mediathek.gui.actions.import_actions.ImportOldBlacklistAction;
import mediathek.gui.actions.import_actions.ImportOldReplacementListAction;
import mediathek.gui.dialog.DialogBeenden;
import mediathek.gui.dialog.LoadFilmListDialog;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.filmInformation.InfoDialog;
import mediathek.gui.history.ResetAboHistoryAction;
import mediathek.gui.history.ResetDownloadHistoryAction;
import mediathek.gui.messages.*;
import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.gui.tasks.BlacklistFilterWorker;
import mediathek.gui.tasks.LuceneIndexWorker;
import mediathek.gui.tasks.RefreshAboWorker;
import mediathek.javafx.tool.JFXHiddenApplication;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.res.GetIcon;
import mediathek.tool.*;
import mediathek.tool.notification.GenericNotificationCenter;
import mediathek.tool.notification.INotificationCenter;
import mediathek.tool.notification.NullNotificationCenter;
import mediathek.tool.threads.IndicatorThread;
import mediathek.update.AutomaticFilmlistUpdate;
import mediathek.update.ProgramUpdateCheck;
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
import java.io.IOException;
import java.util.HashMap;
import java.util.NoSuchElementException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static mediathek.tool.ApplicationConfiguration.CONFIG_AUTOMATIC_UPDATE_CHECK;

public class MediathekGui extends JFrame {

    protected static final Logger logger = LogManager.getLogger();
    private static final String ICON_NAME = "MediathekView.png";
    private static final String ICON_PATH = "/mediathek/res/";
    private static final int ICON_WIDTH = 58;
    private static final int ICON_HEIGHT = 58;
    private static final String KEY_F10 = "F10";
    private static final String NONE = "none";
    private static final int MIN_WINDOW_WIDTH = 800;
    private static final int MIN_WINDOW_HEIGHT = 600;
    /**
     * "Pointer" to UI
     */
    private static MediathekGui ui;
    public final LoadFilmListAction loadFilmListAction;
    /**
     * Number of active downloads
     */
    protected final AtomicInteger numDownloadsStarted = new AtomicInteger(0);
    protected final Daten daten = Daten.getInstance();
    protected final PositionSavingTabbedPane tabbedPane = new PositionSavingTabbedPane();
    protected final JMenu jMenuHilfe = new JMenu();
    protected final SettingsAction settingsAction = new SettingsAction(this);
    final JMenu fontMenu = new JMenu("Schrift");
    private final JMenu jMenuDatei = new JMenu();
    private final JMenu jMenuFilme = new JMenu();
    private final JMenuBar jMenuBar = new JMenuBar();
    private final JMenu jMenuDownload = new JMenu();
    private final JMenu jMenuAbos = new JMenu();
    private final JMenu jMenuAnsicht = new JMenu();
    private final HashMap<JMenu, MenuTabSwitchListener> menuListeners = new HashMap<>();
    private final SearchProgramUpdateAction searchProgramUpdateAction;
    private final MemoryMonitorAction showMemoryMonitorAction = new MemoryMonitorAction(this);
    private final InfoDialog filmInfo;
    private final ManageAboAction manageAboAction = new ManageAboAction();
    private final ShowBandwidthUsageAction showBandwidthUsageAction = new ShowBandwidthUsageAction(this);
    public FixedRedrawStatusBar swingStatusBar;
    public GuiFilme tabFilme;
    public GuiDownloads tabDownloads;
    public EditBlacklistAction editBlacklistAction = new EditBlacklistAction(this);
    public ToggleBlacklistAction toggleBlacklistAction = new ToggleBlacklistAction();
    public ShowFilmInformationAction showFilmInformationAction = new ShowFilmInformationAction();
    /**
     * this property keeps track how many items are currently selected in the active table view
     */
    public ListSelectedItemsProperty selectedListItemsProperty = new ListSelectedItemsProperty(0);
    /**
     * Used for status bar progress.
     */
    public JLabel progressLabel = new JLabel();
    /**
     * Used for status bar progress.
     */
    public JProgressBar progressBar = new JProgressBar();
    /**
     * the global configuration for this app.
     */
    protected Configuration config = ApplicationConfiguration.getConfiguration();
    protected JToolBar commonToolBar = new JToolBar();
    protected ManageBookmarkAction manageBookmarkAction = new ManageBookmarkAction(this);
    protected FontManager fontManager = new FontManager(this);
    protected ToggleDarkModeAction toggleDarkModeAction = new ToggleDarkModeAction();
    private MVTray tray;
    private DialogEinstellungen dialogEinstellungen;
    private ProgramUpdateCheck programUpdateChecker;
    /**
     * Progress indicator thread for OS X and windows.
     */
    private IndicatorThread progressIndicatorThread;
    private AutomaticFilmlistUpdate automaticFilmlistUpdate;
    private boolean shutdownRequested;

    public MediathekGui() {
        ui = this;

        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

        setupScrollBarWidth();
        UIManager.put("TabbedPane.showTabSeparators", true);

        setupAlternatingRowColors();

        loadFilmListAction = new LoadFilmListAction(this);
        searchProgramUpdateAction = new SearchProgramUpdateAction();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_MAINWINDOW));

        getContentPane().setLayout(new BorderLayout());

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

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.LOAD_MEMORY_MONITOR));
        createMemoryMonitor();

        setupNotificationCenter();

        createCommonToolBar();

        Main.splashScreen.ifPresent(s -> s.update(UIProgressState.FINISHED));

        workaroundJavaFxInitializationBug();

        var messageBus = MessageBus.getMessageBus();
        //send before subscribing
        messageBus.publishAsync(new TableModelChangeEvent(true));
        messageBus.subscribe(this);

        SwingUtilities.invokeLater(() -> {
            if (Taskbar.isTaskbarSupported())
                setupTaskbarMenu();
        });

        setupSystemTray();

        SwingUtilities.invokeLater(this::setApplicationWindowSize);

        loadFilmlist();

        setupUpdateCheck(config.getBoolean(CONFIG_AUTOMATIC_UPDATE_CHECK, true));

        setupShutdownHook();

        checkInvalidRegularExpressions();

        logger.trace("Loading bandwidth monitor");
        if (config.getBoolean(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, false)) {
            showBandwidthUsageAction.actionPerformed(null);
        }
        logger.trace("Finished loading bandwidth monitor");

        logger.trace("Loading info dialog");
        filmInfo = new InfoDialog(this);
        logger.trace("Finished loading info dialog");
    }

    /**
     * Return the user interface instance
     *
     * @return the class instance or null.
     */
    public static MediathekGui ui() {
        return ui;
    }

    protected void setupScrollBarWidth() {
        // win and linux users complain about scrollbars being too small...
        UIManager.put( "ScrollBar.width", 16 );
    }

    public void setupAlternatingRowColors() {
        // install alternate row color only for windows >8 and macOS, Linux
        boolean installAlternateRowColor;
        if (SystemUtils.IS_OS_WINDOWS && VersionHelpers.IsWindows8OrGreater()) {
            installAlternateRowColor = true;
        } else installAlternateRowColor = SystemUtils.IS_OS_MAC_OSX || SystemUtils.IS_OS_LINUX;

        if (installAlternateRowColor)
            UIManager.put("Table.alternateRowColor", MVColor.getAlternatingRowColor());
    }

    protected void createCommonToolBar() {
        commonToolBar.add(loadFilmListAction);
        commonToolBar.add(showFilmInformationAction);
        commonToolBar.add(toggleBlacklistAction);
        commonToolBar.addSeparator();
        commonToolBar.add(editBlacklistAction);
        commonToolBar.add(manageAboAction);
        commonToolBar.add(manageBookmarkAction);
        commonToolBar.addSeparator();
        commonToolBar.add(settingsAction);
        commonToolBar.add(Box.createHorizontalGlue());
        commonToolBar.add(toggleDarkModeAction);

        if (!SystemUtils.IS_OS_MAC_OSX) {
            commonToolBar.setFloatable(true);
            commonToolBar.setName("Allgemein");
        }


        if (!SystemUtils.IS_OS_MAC_OSX) {
            tabbedPane.putClientProperty("JTabbedPane.trailingComponent", commonToolBar);
        }
        else
            getContentPane().add(commonToolBar, BorderLayout.PAGE_START);
    }

    /**
     * Check if we encountered invalid regexps and warn user if necessary.
     * This needs to be delayed unfortunately as we can see result only after table has been filled.
     * So we simply wait 30 seconds until we check.
     */
    private void checkInvalidRegularExpressions() {
        TimerPool.getTimerPool().schedule(() -> {
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

                JOptionPane.showMessageDialog(this,
                        message,
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.ERROR_MESSAGE);
            }
        }, 15, TimeUnit.SECONDS);
    }

    /**
     * Create either a native or a javafx notification center depending on platform
     */
    private void setupNotificationCenter() {
        final var notificationCenter = daten.notificationCenter();
        final boolean showNotifications = config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS, true);

        if (notificationCenter != null) {
            try {
                notificationCenter.close();
            } catch (IOException e) {
                logger.error("Failed to close old notification center", e);
            }
        }

        if (!showNotifications) {
            daten.setNotificationCenter(new NullNotificationCenter());
        } else {
            daten.setNotificationCenter(getNotificationCenter());
        }
    }

    /**
     * Return the platform-specific notification implementation.
     *
     * @return generic or platform-specific notification implementation.
     */
    protected INotificationCenter getNotificationCenter() {
        return new GenericNotificationCenter();
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
        Runtime.getRuntime().addShutdownHook(new Log4jShutdownHookThread());
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

        if (!SystemUtils.IS_OS_MAC_OSX)
            jMenuBar.add(fontMenu);

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
        boolean visible = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.MemoryMonitorDialog.VISIBLE, false);
        if (visible)
            showMemoryMonitorAction.showMemoryMonitor();
    }

    /**
     * Read a local filmlist or load a new one in auto mode.
     */
    private void loadFilmlist() {
        //don´t write filmlist when we are reading only...
        var writeCondition = !(GuiFunktionen.getFilmListUpdateType() == FilmListUpdateType.AUTOMATIC && daten.getListeFilme().needsUpdate());
        Daten.dontWriteFilmlistOnStartup.set(writeCondition);

        swingStatusBar.add(progressLabel);
        swingStatusBar.add(progressBar);

        var worker = CompletableFuture.runAsync(() -> {
                    logger.trace("Reading local filmlist");
                    MessageBus.getMessageBus().publishAsync(new FilmListReadStartEvent());

                    try (FilmListReader reader = new FilmListReader()) {
                        final int num_days = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0);
                        reader.readFilmListe(StandardLocations.getFilmlistFilePathString(), daten.getListeFilme(), num_days);
                    }
                    MessageBus.getMessageBus().publishAsync(new FilmListReadStopEvent());
                })
                .thenRun(() -> {
                    logger.trace("Check for filmlist updates");
                    if (GuiFunktionen.getFilmListUpdateType() == FilmListUpdateType.AUTOMATIC && daten.getListeFilme().needsUpdate()) {
                        daten.getFilmeLaden().loadFilmlist("", true);
                    }
                })
                .thenRun(new RefreshAboWorker(progressLabel, progressBar))
                .thenRun(new BlacklistFilterWorker(progressLabel, progressBar));

        if (daten.getListeFilmeNachBlackList() instanceof IndexedFilmList){
            worker = worker.thenRun(new LuceneIndexWorker(progressLabel, progressBar));
        }

        worker.thenRun(() -> SwingUtilities.invokeLater(() -> Daten.getInstance().getFilmeLaden().notifyFertig(new ListenerFilmeLadenEvent("", "", 100, 100, false))))
                .thenRun(() -> Daten.dontWriteFilmlistOnStartup.set(false))
                .thenRun(() -> SwingUtilities.invokeLater(() -> {
                    swingStatusBar.remove(progressBar);
                    swingStatusBar.remove(progressLabel);
                }));
    }

    /**
     * Create the status bar item.
     */
    private void createStatusBar() {
        swingStatusBar = new FixedRedrawStatusBar(this);
        getContentPane().add(swingStatusBar, BorderLayout.SOUTH);

        createFilmlistDownloadProgress();
    }

    private void createFilmlistDownloadProgress() {
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                swingStatusBar.add(progressLabel);
                swingStatusBar.add(progressBar);
            }

            @Override
            public void progress(ListenerFilmeLadenEvent event) {
                if (event.max == 0 || event.progress == event.max) {
                    progressBar.setIndeterminate(true);
                } else {
                    progressBar.setIndeterminate(false);
                    progressBar.setMinimum(0);
                    progressBar.setMaximum(event.max);
                    progressBar.setValue(event.progress);
                }
                progressLabel.setText(event.text);
            }
        });
    }

    public InfoDialog getFilmInfoDialog() {
        return filmInfo;
    }

    @Handler
    private void handleTabVisualSettingsChangedEvent(TabVisualSettingsChangedEvent e) {
        SwingUtilities.invokeLater(() -> {
            configureTabPlacement();
            configureTabIcons();
        });
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
            }
        });
    }

    /**
     * Reload filmlist every 24h when in automatic mode.
     */
    private void setupAutomaticFilmlistReload() {
        final AutomaticFilmlistUpdate.IUpdateAction performUpdate = () -> {
            if (GuiFunktionen.getFilmListUpdateType() == FilmListUpdateType.AUTOMATIC) {
                loadFilmListAction.setEnabled(false);
                //if downloads are running, don´t update
                if (daten.getListeDownloads().unfinishedDownloads() == 0) {
                    performFilmListLoadOperation(false);
                }
                loadFilmListAction.setEnabled(true);
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
                    quitApplication();
                }
            }
        });
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
            programUpdateChecker = new ProgramUpdateCheck();
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

        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_RESTORE_SELECTED_TAB, false))
            tabbedPane.restoreSavedTabPosition();
        tabbedPane.installChangeListener();

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
        exportMenu.add(new ExportReadableFilmlistAction());
        exportMenu.add(new ExportDecompressedFilmlistAction());

        var importMenu = new JMenu("Import");
        importMenu.add(new ImportOldAbosAction());
        importMenu.add(new ImportOldBlacklistAction());
        importMenu.add(new ImportOldReplacementListAction());

        jMenuDatei.add(exportMenu);
        jMenuDatei.add(importMenu);

        //on macOS we will use native handlers instead...
        if (!SystemUtils.IS_OS_MAC_OSX) {
            jMenuDatei.addSeparator();
            jMenuDatei.add(settingsAction);
            jMenuDatei.addSeparator();
            jMenuDatei.add(new QuitAction(this));
        }
    }

    private void createViewMenu() {
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(showMemoryMonitorAction);
        jMenuAnsicht.add(showBandwidthUsageAction);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(tabFilme.toggleFilterDialogVisibilityAction);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(showFilmInformationAction);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(manageBookmarkAction);
    }

    private void createFontMenu() {
        var restoreFontMenuItem = new JMenuItem();
        restoreFontMenuItem.setText("Schrift zurücksetzen");
        restoreFontMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_0, Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx()));
        restoreFontMenuItem.addActionListener(e -> fontManager.resetFont());
        fontMenu.add(restoreFontMenuItem);

        var incrFontMenuItem = new JMenuItem();
        incrFontMenuItem.setText("Schrift vergrößern");
        incrFontMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_PLUS, Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx()));
        incrFontMenuItem.addActionListener(e -> fontManager.increaseFontSize());
        fontMenu.add(incrFontMenuItem);

        var decrFontMenuItem = new JMenuItem();
        decrFontMenuItem.setText("Schrift verkleinern");
        decrFontMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_MINUS, Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx()));
        decrFontMenuItem.addActionListener(e -> fontManager.decreaseFontSize());
        fontMenu.add(decrFontMenuItem);

        fontManager.updateFontMenuItems();
    }

    @Handler
    private void handleFilmlistWriteStartEvent(FilmListWriteStartEvent e) {
        SwingUtilities.invokeLater(() -> loadFilmListAction.setEnabled(false));
    }

    @Handler
    private void handleFilmlistWriteStopEvent(FilmListWriteStopEvent e) {
        SwingUtilities.invokeLater(() -> loadFilmListAction.setEnabled(true));
    }

    private void createHelpMenu() {
        jMenuHilfe.add(new ShowOnlineHelpAction());
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new ResetSettingsAction(this, daten));
        jMenuHilfe.add(new ResetDownloadHistoryAction(this));
        jMenuHilfe.add(new ResetAboHistoryAction(this));
        jMenuHilfe.add(new DeleteLocalFilmlistAction(this));
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new ResetFilterDialogPosition(this));
        jMenuHilfe.addSeparator();
        //do not show menu entry if we have external update support
        if (GuiFunktionen.isNotUsingExternalUpdater()) {
            jMenuHilfe.add(searchProgramUpdateAction);
        }
        jMenuHilfe.add(new ShowProgramInfosAction());

        installAdditionalHelpEntries();
    }

    protected void installAdditionalHelpEntries() {
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new ShowAboutAction());
    }

    protected void initMenus() {
        installMenuTabSwitchListener();

        createFileMenu();
        tabFilme.installMenuEntries(jMenuFilme);
        tabDownloads.installMenuEntries(jMenuDownload);

        createFontMenu();
        createViewMenu();
        tabFilme.installViewMenuEntry(jMenuAnsicht);

        createAboMenu();
        if (Config.isDebugModeEnabled())
            createDeveloperMenu();
        createHelpMenu();
    }

    private void createDeveloperMenu() {
        JMenu devMenu = new JMenu("Entwickler");

        JMenuItem miGc = new JMenuItem("GC ausführen");
        miGc.addActionListener(l -> System.gc());

        devMenu.add(miGc);
        jMenuBar.add(devMenu);
    }

    private void createAboMenu() {
        jMenuAbos.add(new CreateNewAboAction(daten.getListeAbo()));
        jMenuAbos.add(new ShowAboHistoryAction(MediathekGui.ui()));
        jMenuAbos.addSeparator();
        jMenuAbos.add(manageAboAction);
    }

    public void performFilmListLoadOperation(boolean manualMode) {
        if (manualMode || GuiFunktionen.getFilmListUpdateType() == FilmListUpdateType.MANUAL) {
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

    public boolean isShutdownRequested() {
        return shutdownRequested;
    }

    public void setShutdownRequested(boolean shutdownRequested) {
        this.shutdownRequested = shutdownRequested;
    }

    public boolean quitApplication() {
        if (daten.getListeDownloads().unfinishedDownloads() > 0) {
            // erst mal prüfen ob noch Downloads laufen
            DialogBeenden dialogBeenden = new DialogBeenden(this);
            dialogBeenden.setVisible(true);
            if (!dialogBeenden.getApplicationCanTerminate()) {
                return false;
            }
            setShutdownRequested(dialogBeenden.isShutdownRequested());
        }

        if (automaticFilmlistUpdate != null)
            automaticFilmlistUpdate.close();

        endProgramUpdateChecker();

        showMemoryMonitorAction.closeMemoryMonitor();

        showBandwidthUsageAction.getDialogOptional().ifPresent(dlg -> {
            dlg.dispose();
            //little hack, we must preserve the visible state since it was open when app quits...
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, true);
        });

        manageAboAction.closeDialog();

        ShutdownDialogController shutdownProgress = new ShutdownDialogController(this);
        shutdownProgress.show();

        var historyWorker = CompletableFuture.runAsync(() -> {
            try (SeenHistoryController history = new SeenHistoryController()) {
                history.performMaintenance();
            }
        });

        var bookmarkWorker = CompletableFuture.runAsync(() ->
                daten.getListeBookmarkList().saveToFile(StandardLocations.getBookmarkFilePath()));

        // stop the download thread
        shutdownProgress.setStatus(ShutdownState.TERMINATE_STARTER_THREAD);
        daten.getStarterClass().getStarterThread().interrupt();

        shutdownProgress.setStatus(ShutdownState.SHUTDOWN_NOTIFICATION_CENTER);
        closeNotificationCenter();

        // Tabelleneinstellungen merken
        shutdownProgress.setStatus(ShutdownState.SAVE_FILM_DATA);
        tabFilme.tabelleSpeichern();
        tabFilme.saveSettings();  // needs thread pools active!
        tabFilme.filmActionPanel.getFilterDialog().dispose();

        shutdownProgress.setStatus(ShutdownState.SAVE_DOWNLOAD_DATA);
        tabDownloads.tabelleSpeichern();

        shutdownProgress.setStatus(ShutdownState.STOP_DOWNLOADS);
        stopDownloads();

        shutdownProgress.setStatus(ShutdownState.SAVE_APP_DATA);
        daten.allesSpeichern();

        shutdownProgress.setStatus(ShutdownState.SHUTDOWN_THREAD_POOL);
        shutdownTimerPool();
        waitForCommonPoolToComplete();

        //shutdown JavaFX
        shutdownProgress.setStatus(ShutdownState.TERMINATE_JAVAFX_SUPPORT);
        shutdownJavaFx();

        try {
            shutdownProgress.setStatus(ShutdownState.SAVE_BOOKMARKS);
            bookmarkWorker.get();

            shutdownProgress.setStatus(ShutdownState.PERFORM_SEEN_HISTORY_MAINTENANCE);
            historyWorker.get();
        } catch (InterruptedException | ExecutionException e) {
            logger.error("Async task error", e);
        }

        //close main window
        dispose();

        //write all settings if not done already...
        ApplicationConfiguration.getInstance().writeConfiguration();

        RuntimeStatistics.printRuntimeStatistics();
        RuntimeStatistics.printDataUsageStatistics();
        shutdownProgress.setStatus(ShutdownState.COMPLETE);

        cleanupLuceneIndex();

        if (isShutdownRequested()) {
            shutdownComputer();
        }

        System.exit(0);

        return true;
    }

    private void cleanupLuceneIndex() {
        if (daten.getListeFilmeNachBlackList() instanceof IndexedFilmList filmList) {
            try (var writer = filmList.getWriter()) {
                writer.deleteAll();
                writer.commit();
                //filmList.getLuceneDirectory().close();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }

    /**
     * Gracefully shutdown the JavaFX environment.
     */
    private void shutdownJavaFx() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> JFXHiddenApplication.getPrimaryStage().close());
        Platform.exit();
    }

    private void shutdownTimerPool() {
        var timerPool = TimerPool.getTimerPool();
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

}
