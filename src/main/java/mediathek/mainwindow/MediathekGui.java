/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.mainwindow;

import com.formdev.flatlaf.extras.components.FlatButton;
import mediathek.SplashScreenLifecycle;
import mediathek.audiothek.repository.AudioRepository;
import mediathek.audiothek.ui.main.AudiothekPanel;
import mediathek.config.*;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.IndexedFilmList;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.filmlisten.FilmListLoadOptions;
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
import mediathek.gui.duplicates.CommonStatsEvaluationTask;
import mediathek.gui.duplicates.FilmDuplicateEvaluationTask;
import mediathek.gui.duplicates.overview.FilmDuplicateOverviewDialog;
import mediathek.gui.filmInformation.FilmInfoDialog;
import mediathek.gui.history.ResetAboHistoryAction;
import mediathek.gui.history.ResetDownloadHistoryAction;
import mediathek.gui.messages.*;
import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.gui.tabs.tab_livestreams.LivestreamPanel;
import mediathek.gui.tasks.BlacklistFilterWorker;
import mediathek.gui.tasks.LuceneIndexWorker;
import mediathek.gui.tasks.RefreshAboWorker;
import mediathek.logging.LogDialog;
import mediathek.sqlite.RecoverHistoryDbAction;
import mediathek.swing.IconOnlyButton;
import mediathek.tool.*;
import mediathek.tool.notification.GenericNotificationCenter;
import mediathek.tool.notification.INotificationCenter;
import mediathek.tool.notification.NotificationService;
import mediathek.tool.threads.IndicatorThread;
import mediathek.tool.timer.TimerPool;
import mediathek.update.AutomaticFilmlistUpdate;
import mediathek.update.ProgramUpdateCheck;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;
import raven.toast.Notifications;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Comparator;
import java.util.HashMap;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.IntConsumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

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
    private static final String ACTION_MAP_KEY_COPY_HQ_URL = "COPY_HQ_URL";
    private static final String ACTION_MAP_KEY_COPY_NORMAL_URL = "COPY_NORMAL_URL";
    private static final String TABBED_PANE_TRAILING_COMPONENT = "JTabbedPane.trailingComponent";
    /**
     * "Pointer" to UI
     */
    private static MediathekGui ui;
    public final LoadFilmListAction loadFilmListAction;
    public final EditBlacklistAction editBlacklistAction = new EditBlacklistAction(this);
    public final ToggleBlacklistAction toggleBlacklistAction = new ToggleBlacklistAction();
    public final ShowFilmInformationAction showFilmInformationAction = new ShowFilmInformationAction();
    /**
     * this property keeps track how many items are currently selected in the active table view
     */
    public final ListSelectedItemsProperty selectedListItemsProperty = new ListSelectedItemsProperty(0);
    /**
     * Used for status bar progress.
     */
    public final JLabel progressLabel = new JLabel();
    /**
     * Used for status bar progress.
     */
    public final JProgressBar progressBar = new JProgressBar();
    /**
     * Number of active downloads
     */
    protected final AtomicInteger numDownloadsStarted = new AtomicInteger(0);
    protected final Daten daten = Daten.getInstance();
    protected final PositionSavingTabbedPane tabbedPane = new PositionSavingTabbedPane();
    protected final JMenu jMenuHilfe = new JMenu();
    protected final SettingsAction settingsAction = new SettingsAction();
    /**
     * the global configuration for this app.
     */
    protected final Configuration config = ApplicationConfiguration.getConfiguration();
    protected final JToolBar commonToolBar = new JToolBar();
    protected final ManageBookmarkAction manageBookmarkAction = new ManageBookmarkAction(this);
    protected final ToggleDarkModeAction toggleDarkModeAction = new ToggleDarkModeAction();
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
    private final FilmInfoDialog filmInfo;
    private final ManageAboAction manageAboAction = new ManageAboAction();
    private final ShowBandwidthUsageAction showBandwidthUsageAction = new ShowBandwidthUsageAction(this);
    private final ShowFilmStatisticsAction showFilmStatisticsAction = new ShowFilmStatisticsAction(this);
    private final ShowDuplicateStatisticsAction showDuplicateStatisticsAction = new ShowDuplicateStatisticsAction(this);
    private final ShowLuceneTutorialAction showLuceneTutorialAction = new ShowLuceneTutorialAction(this);
    private final LivestreamPanel tabLivestreams = new LivestreamPanel();
    private final ToggleZappLivestreamsTabAction toggleZappLivestreamsTabAction = new ToggleZappLivestreamsTabAction(tabLivestreams);
    private final AudioRepository audiothekRepository = new AudioRepository();
    private final AudiothekPanel tabAudiothek = new AudiothekPanel(audiothekRepository);
    private final ToggleAudiothekTabAction toggleAudiothekTabAction = new ToggleAudiothekTabAction(tabAudiothek);
    private final LogDialog logWindow = new LogDialog(this);
    public FixedRedrawStatusBar swingStatusBar;
    public GuiFilme tabFilme;
    public GuiDownloads tabDownloads;
    protected FontManager fontManager;
    private MVTray tray;
    private DialogEinstellungen dialogEinstellungen;
    private ProgramUpdateCheck programUpdateChecker;
    /**
     * Progress indicator thread for OS X and windows.
     */
    private IndicatorThread progressIndicatorThread;
    private AutomaticFilmlistUpdate automaticFilmlistUpdate;
    private boolean shutdownRequested;
    private boolean resetSettingsOnQuit;
    private final Supplier<INotificationCenter> notificationCenterFactory;

    public MediathekGui() {
        this(GenericNotificationCenter::new);
    }

    protected MediathekGui(Supplier<INotificationCenter> notificationCenterFactory) {
        this.notificationCenterFactory = Objects.requireNonNull(notificationCenterFactory);
        ui = this;

        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

        setupScrollBarWidth();
        UIManager.put("TabbedPane.showTabSeparators", true);
        UIManager.addPropertyChangeListener(evt -> {
            if (evt.getPropertyName().equalsIgnoreCase("lookAndFeel")) {
                SwingUtilities.updateComponentTreeUI(progressLabel);
                SwingUtilities.updateComponentTreeUI(progressBar);
            }
        });

        setupAlternatingRowColors();

        loadFilmListAction = new LoadFilmListAction(this);
        searchProgramUpdateAction = new SearchProgramUpdateAction();

        SplashScreenLifecycle.update(UIProgressState.LOAD_MAINWINDOW);

        getContentPane().setLayout(new BorderLayout());

        setIconAndWindowImage();

        createMenuBar();

        remapF10Key();

        SplashScreenLifecycle.update(UIProgressState.WAIT_FOR_HISTORY_DATA);
        try {
            daten.waitForHistoryDataLoadingToComplete();
        }
        catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            logger.error("waitForHistoryDataLoadingToComplete()", e);
        }
        catch (ExecutionException e) {
            logger.error("waitForHistoryDataLoadingToComplete()", e);
        }

        SplashScreenLifecycle.update(UIProgressState.CREATE_STATUS_BAR);
        createStatusBar();

        SplashScreenLifecycle.update(UIProgressState.SETUP_FILM_LISTENERS);
        setupFilmListListener();

        SplashScreenLifecycle.update(UIProgressState.LOAD_TABS);
        initTabs();

        SplashScreenLifecycle.update(UIProgressState.INIT_MENUS);
        initMenus();

        SplashScreenLifecycle.update(UIProgressState.LOAD_MEMORY_MONITOR);
        createMemoryMonitor();

        setupNotificationCenter();

        createCommonToolBar();
        installToolBar();

        SplashScreenLifecycle.update(UIProgressState.FINISHED);

        subscribeTableModelChangeEvent();

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

        loadBandwidthMonitor();

        logger.trace("Loading info dialog");
        filmInfo = new FilmInfoDialog(this);
        logger.trace("Finished loading info dialog");

        mapFilmUrlCopyCommands();

        resetTabPlacement();

        //setup Raven Notification library
        Notifications.getInstance().setJFrame(this);

        performGeoCountryStartupCheck();
    }

    /**
     * Return the user interface instance
     *
     * @return the class instance or null.
     */
    public static MediathekGui ui() {
        return ui;
    }

    public boolean supportsAutomaticMenuTabSwitching() {
        return true;
    }

    protected void resetTabPlacement() {
        // we need to re-setup tab-placement if the tabs are not in top position as toolbar is installed after tab creation
        MessageBus.getMessageBus().publishAsync(new TabVisualSettingsChangedEvent());
    }

    private void performAustrianVlcCheck() {
        //show a link to tutorial if we are in Austria and have never used MV before...
        new AustrianVlcCheck(this).perform();
    }

    private void performGeoCountryStartupCheck() {
        new GeoCountryStartupCheck(this, this::performAustrianVlcCheck).perform();
    }

    private void loadBandwidthMonitor() {
        logger.trace("Loading bandwidth monitor");
        if (config.getBoolean(ApplicationConfiguration.APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, false)) {
            showBandwidthUsageAction.actionPerformed(null);
        }
        logger.trace("Finished loading bandwidth monitor");
    }

    private void subscribeTableModelChangeEvent() {
        var messageBus = MessageBus.getMessageBus();
        //send before subscribing
        messageBus.publishAsync(new TableModelChangeEvent(true, false));
        messageBus.subscribe(this);
    }

    private void mapFilmUrlCopyCommands() {
        final var im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, GuiFunktionen.getPlatformControlKey() |
                KeyEvent.SHIFT_DOWN_MASK | KeyEvent.ALT_DOWN_MASK), ACTION_MAP_KEY_COPY_HQ_URL);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, GuiFunktionen.getPlatformControlKey() |
                KeyEvent.SHIFT_DOWN_MASK | KeyEvent.ALT_DOWN_MASK), ACTION_MAP_KEY_COPY_NORMAL_URL);

        final var am = jMenuBar.getActionMap();
        am.put(ACTION_MAP_KEY_COPY_HQ_URL, tabFilme.copyHqUrlToClipboardAction);
        am.put(ACTION_MAP_KEY_COPY_NORMAL_URL, tabFilme.copyNormalUrlToClipboardAction);
    }

    protected void setupScrollBarWidth() {
        // win and linux users complain about scrollbars being too small...
        UIManager.put("ScrollBar.width", 16);
    }

    /**
     * Check if alternate row colors in table should be used.
     * Overridden in subclasses for the different OSes.
     *
     * @return true when alternating row colors should be used, false otherwise.
     */
    protected boolean useAlternateRowColors() {
        return true;
    }

    public void setupAlternatingRowColors() {
        if (useAlternateRowColors())
            UIManager.put("Table.alternateRowColor", MVColor.getAlternatingRowColor());
    }

    protected void createDarkModeToggleButton() {
        commonToolBar.add(Box.createHorizontalGlue());
        commonToolBar.add(toggleDarkModeAction);
    }

    protected void createDarkModeMenuAction() {
        var actionButton = new FlatButton();
        actionButton.setButtonType(FlatButton.ButtonType.toolBarButton);
        actionButton.setFocusable(false);
        actionButton.setAction(toggleDarkModeAction);
        actionButton.setSquareSize(true);
        jMenuBar.add(Box.createGlue());
        jMenuBar.add(actionButton);
    }

    protected void setToolBarProperties() {
        commonToolBar.setFloatable(true);
        commonToolBar.setName("Allgemein");
    }

    protected void installToolBar() {
        tabbedPane.putClientProperty(TABBED_PANE_TRAILING_COMPONENT, commonToolBar);
        tabbedPane.putClientProperty("JTabbedPane.tabRotation", "auto");
    }

    protected void createToggleBlacklistButton() {
        boolean useIconWithText = ApplicationConfiguration.getConfiguration()
                .getBoolean(ApplicationConfiguration.TOOLBAR_BLACKLIST_ICON_WITH_TEXT, false);
        if (useIconWithText) {
            commonToolBar.add(new JButton(toggleBlacklistAction));
        }
        else {
            commonToolBar.add(new IconOnlyButton(toggleBlacklistAction));
        }
    }

    protected void createCommonToolBar() {
        commonToolBar.add(new IconOnlyButton(loadFilmListAction));
        commonToolBar.add(new IconOnlyButton(showFilmInformationAction));
        createToggleBlacklistButton();
        commonToolBar.addSeparator();
        commonToolBar.add(new IconOnlyButton(editBlacklistAction));
        commonToolBar.add(new IconOnlyButton(manageAboAction));
        commonToolBar.addSeparator();
        commonToolBar.add(new IconOnlyButton(settingsAction));
        createDarkModeToggleButton();

        setToolBarProperties();
    }

    /**
     * Check if we encountered invalid regexps and warn user if necessary.
     * This needs to be delayed unfortunately as we can see result only after table has been filled.
     * So we simply wait 15 seconds until we check.
     */
    private void checkInvalidRegularExpressions() {
        TimerPool.schedule(() -> {
            final var invalidExpressions = Filter.drainRegExpErrors();
            if (invalidExpressions.isEmpty()) {
                return;
            }

            final var regexStr = invalidExpressions.stream()
                    .sorted(Comparator.naturalOrder())
                    .map(HtmlUtils::escapeHtml)
                    .collect(Collectors.joining("<br/>"));

            final var message = String.format(
                    "<html>Während des Starts wurden ungültige reguläre Ausdrücke (RegExp) in Ihrer Blacklist und/oder Abos entdeckt.<br/>" +
                            "<b>Sie müssen diese korrigieren, ansonsten funktioniert das Programm nicht fehlerfrei!</b><br/><br/>" +
                            "Nachfolgende Ausdrücke sind fehlerbehaftet: <br/>%s</html>", regexStr);

            MVMessageDialog.showMessageDialog(this,
                    message,
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.ERROR_MESSAGE);
        }, 15, TimeUnit.SECONDS);
    }

    /**
     * Create either a native or a java notification center depending on platform
     */
    private void setupNotificationCenter() {
        final boolean showNotifications = config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS, true);
        NotificationService.configure(notificationCenterFactory, showNotifications);
    }

    @Handler
    protected void handleNotificationCenterChangeEvent(NotificationCenterChangeEvent e) {
        setupNotificationCenter();
    }

    protected void closeNotificationCenter() {
        NotificationService.INSTANCE.close();
    }

    /**
     * This shutdown hook will try to save both log messages and write config changes to disk before app terminates.
     */
    private void setupShutdownHook() {
        Runtime.getRuntime().addShutdownHook(new Log4jShutdownHookThread());
    }

    protected void setupSystemTray() {
        SwingUtilities.invokeLater(() -> {
            initializeSystemTray();

            addWindowListener(new WindowAdapter() {
                @Override
                public void windowClosing(WindowEvent evt) {
                    if (tray != null && config.getBoolean(ApplicationConfiguration.APPLICATION_UI_USE_TRAY, false)) {
                        setVisible(false);
                    }
                    else {
                        quitApplication();
                    }
                }
            });
        });
    }

    private void setupTaskbarMenu() {
        var taskbar = Taskbar.getTaskbar();
        if (taskbar.isSupported(Taskbar.Feature.MENU)) {
            PopupMenu popupMenu = taskbar.getMenu();
            if (popupMenu == null)
                popupMenu = new PopupMenu();

            popupMenu.addSeparator();
            popupMenu.add(new NoIconAwtMenuItem(loadFilmListAction));

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

    protected void createMenuBar() {
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

        addFontMenu();

        jMenuAnsicht.setMnemonic('a');
        jMenuAnsicht.setText("Ansicht");
        jMenuBar.add(jMenuAnsicht);

        jMenuHilfe.setMnemonic('h');
        jMenuHilfe.setText("Hilfe");
        jMenuBar.add(jMenuHilfe);

        setJMenuBar(jMenuBar);
    }

    protected void addFontMenu() {
        jMenuBar.add(fontMenu);
    }

    private void createMemoryMonitor() {
        boolean visible = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.MemoryMonitorDialog.VISIBLE, false);
        if (visible) {
            showMemoryMonitorAction.showMemoryMonitor();
        }
    }

    /**
     * Read a local filmlist or load a new one in auto mode.
     */
    private void loadFilmlist() {
        swingStatusBar.add(progressLabel);
        swingStatusBar.add(progressBar);

        var evaluateDuplicates = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FILM_EVALUATE_DUPLICATES, true);

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
                        daten.getFilmeLaden().loadFilmlist("", true, new FilmListLoadOptions(true));
                    }
                });

        if (evaluateDuplicates) {
            worker = worker.thenRun(new FilmDuplicateEvaluationTask());
        }

        worker = worker.thenRun(new CommonStatsEvaluationTask())
                .thenRun(new RefreshAboWorker(progressLabel, progressBar))
                .thenRun(new BlacklistFilterWorker(progressLabel, progressBar));

        if (daten.getListeFilmeNachBlackList() instanceof IndexedFilmList) {
            worker = worker.thenRun(new LuceneIndexWorker(progressLabel, progressBar));
        }

        worker.thenRun(() -> SwingUtilities.invokeLater(() -> Daten.getInstance().getFilmeLaden().notifyFertig(new ListenerFilmeLadenEvent("", "", 100, 100, false))))
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
                }
                else {
                    progressBar.setIndeterminate(false);
                    progressBar.setMinimum(0);
                    progressBar.setMaximum(event.max);
                    progressBar.setValue(event.progress);
                }
                progressLabel.setText(event.text);
            }
        });
    }

    public FilmInfoDialog getFilmInfoDialog() {
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
        }
        catch (NoSuchElementException _) {
            //in case of any error, just make the window maximized
            setExtendedState(JFrame.MAXIMIZED_BOTH);
        }
        finally {
            config.unlock(LockMode.READ);
        }
    }

    private void setApplicationWindowSize() {
        if (Config.isStartMaximized() ||
                ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_MAXIMIZED, true)) {
            setExtendedState(JFrame.MAXIMIZED_BOTH);
        }
        else
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
        }
        else {
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
        }
        else if (tray != null && !useTray) {
            tray.beenden();
            tray = null;
        }
    }

    protected JPanel createTabFilme(@NonNull Daten daten) {
        return new GuiFilme(daten, this);
    }

    public Action getShowLuceneTutorialAction() {
        return showLuceneTutorialAction;
    }

    protected JPanel createTabDownloads(@NonNull Daten daten) {
        return new GuiDownloads(daten, this);
    }

    private void initTabs() {
        Container contentPane = getContentPane();
        contentPane.add(tabbedPane, BorderLayout.CENTER);

        SplashScreenLifecycle.update(UIProgressState.LOAD_DOWNLOAD_TAB);
        tabDownloads = (GuiDownloads) createTabDownloads(daten);

        SplashScreenLifecycle.update(UIProgressState.LOAD_FILM_TAB);
        tabFilme = (GuiFilme) createTabFilme(daten);

        SplashScreenLifecycle.update(UIProgressState.ADD_TABS_TO_UI);
        tabbedPane.addTab(GuiFilme.NAME, tabFilme);
        tabbedPane.addTab(GuiDownloads.NAME, tabDownloads);
        installLivestreamsTab();
        installAudiothekTab();

        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_RESTORE_SELECTED_TAB, false))
            tabbedPane.restoreSavedTabPosition();
        tabbedPane.installChangeListener();

        SplashScreenLifecycle.update(UIProgressState.CONFIGURE_TABS);
        configureTabPlacement();
        configureTabIcons();
    }

    protected void installLivestreamsTab() {
        var show = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_UI_SHOW_ZAPP_LIVESTREAMS, true);
        tabLivestreams.putClientProperty("JTabbedPane.tabClosable", true);
        tabLivestreams.putClientProperty("JTabbedPane.tabCloseCallback", (IntConsumer) _ -> toggleZappLivestreamsTabAction.actionPerformed(null));
        if (show) {
            tabbedPane.addTab("zapp Livestreams", tabLivestreams);
        }
    }

    protected void installAudiothekTab() {
        var show = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_UI_SHOW_AUDIOTHEK, true);
        tabAudiothek.putClientProperty("JTabbedPane.tabClosable", true);
        tabAudiothek.putClientProperty("JTabbedPane.tabCloseCallback", (IntConsumer) _ -> toggleAudiothekTabAction.actionPerformed(null));
        if (show) {
            tabbedPane.addTab("Audiothek", tabAudiothek);
        }
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
    protected void configureTabPlacement() {
        final boolean topPosition = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_TAB_POSITION_TOP, true);
        if (topPosition) {
            tabbedPane.setTabPlacement(JTabbedPane.TOP);
            getContentPane().remove(commonToolBar);
            tabbedPane.putClientProperty(TABBED_PANE_TRAILING_COMPONENT, commonToolBar);
        }
        else {
            tabbedPane.setTabPlacement(JTabbedPane.LEFT);
            tabbedPane.putClientProperty(TABBED_PANE_TRAILING_COMPONENT, null);
            getContentPane().add(commonToolBar, BorderLayout.PAGE_START);
        }
    }

    private void configureTabIcons() {
        final boolean icon = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_TAB_ICONS, false);

        //no icons...
        if (!icon) {
            setTabIcon(tabFilme, null);
            setTabIcon(tabDownloads, null);
        }
        else {
            //setup icons for each tab here
            setTabIcon(tabFilme, GetIcon.getProgramIcon("tab-film.png", 32, 32));
            setTabIcon(tabDownloads, GetIcon.getProgramIcon("tab-download.png", 32, 32));
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
    protected IndicatorThread createProgressIndicatorThread() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Unsupported Platform");
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
            }
            catch (Exception _) {
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

    @Handler
    private void handleShowSettingsDialogEvent(ShowSettingsDialogEvent evt) {
        SwingUtilities.invokeLater(() -> {
            getSettingsDialog().setVisible(true);
            if (!SystemUtils.IS_OS_LINUX)
                getSettingsDialog().toFront();
        });
    }

    /**
     * Install the listeners which will cause automatic tab switching based on associated Menu item.
     */
    protected void installMenuTabSwitchListener() {
        if (!supportsAutomaticMenuTabSwitching()) {
            return;
        }

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
        if (!supportsAutomaticMenuTabSwitching()) {
            return;
        }

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

        addSettingsMenuItem();
        addQuitMenuItem();
    }

    protected void addSettingsMenuItem() {
        jMenuDatei.addSeparator();
        jMenuDatei.add(settingsAction);
    }

    protected void addQuitMenuItem() {
        jMenuDatei.addSeparator();
        jMenuDatei.add(new QuitAction(this));
    }

    private void createViewMenu() {
        tabFilme.installViewMenuEntry(jMenuAnsicht);
        jMenuAnsicht.add(toggleZappLivestreamsTabAction);
        jMenuAnsicht.add(toggleAudiothekTabAction);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(showMemoryMonitorAction);
        jMenuAnsicht.add(showBandwidthUsageAction);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(showFilmStatisticsAction);
        jMenuAnsicht.add(showDuplicateStatisticsAction);
        var mi = new JMenuItem("Übersicht aller Duplikate anzeigen...");
        mi.addActionListener(_ -> {
            FilmDuplicateOverviewDialog dlg = new FilmDuplicateOverviewDialog(this);
            dlg.setVisible(true);
        });
        jMenuAnsicht.add(mi);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(tabFilme.toggleFilterDialogVisibilityAction);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(showFilmInformationAction);
        jMenuAnsicht.addSeparator();
        jMenuAnsicht.add(manageBookmarkAction);
    }

    protected void createFontMenu() {
        fontManager = new FontManager(fontMenu);
        fontManager.restoreConfigData();
    }

    @Handler
    private void handleFilmlistWriteStartEvent(FilmListWriteStartEvent e) {
        SwingUtilities.invokeLater(() -> loadFilmListAction.setEnabled(false));
    }

    @Handler
    private void handleFilmlistWriteStopEvent(FilmListWriteStopEvent e) {
        SwingUtilities.invokeLater(() -> loadFilmListAction.setEnabled(true));
    }

    private void createHelperToolsEntries() {
        var menu = new JMenu("Hilfsmittel");
        menu.add(new OptimizeHistoryDbAction(this));
        menu.add(new RecoverHistoryDbAction(this));
        jMenuHilfe.add(menu);
    }

    private void createHelpMenu() {
        jMenuHilfe.add(new ShowOnlineHelpAction());
        jMenuHilfe.add(showLuceneTutorialAction);
        jMenuHilfe.add(new ShowOnlineFaqAction(this));
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new ShowLogWindowAction());
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new ResetSettingsAction(this, daten));
        jMenuHilfe.add(new ResetDownloadHistoryAction(this));
        jMenuHilfe.add(new ResetAboHistoryAction(this));
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new DeleteLocalFilmlistAction(this));
        jMenuHilfe.add(new DeleteBookmarksAction(this));
        jMenuHilfe.addSeparator();
        jMenuHilfe.add(new ResetFilterDialogPosition(this));
        jMenuHilfe.addSeparator();
        createHelperToolsEntries();
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

        createAboMenu();
        if (Config.isDebugModeEnabled())
            createDeveloperMenu();
        createHelpMenu();
    }

    private void createDeveloperMenu() {
        JMenu devMenu = new JMenu("Entwickler");

        JMenuItem miGc = new JMenuItem("GC ausführen");
        miGc.addActionListener(_ -> System.gc());

        devMenu.add(miGc);

        var idx = jMenuBar.getComponentIndex(jMenuAnsicht);
        jMenuBar.add(devMenu, ++idx);
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
        }
        else {
            // Filme werden automatisch geladen
            daten.getFilmeLaden().loadFilmlist("", false);
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

    public void requestSettingsResetOnQuit() {
        resetSettingsOnQuit = true;
    }

    public boolean quitApplication() {
        if (!confirmApplicationQuit()) {
            return false;
        }

        performApplicationShutdown();
        return true;
    }

    private boolean confirmApplicationQuit() {
        if (daten.getListeDownloads().unfinishedDownloads() > 0) {
            // erst mal prüfen ob noch Downloads laufen
            DialogBeenden dialogBeenden = new DialogBeenden(this);
            dialogBeenden.setVisible(true);
            if (!dialogBeenden.getApplicationCanTerminate()) {
                return false;
            }
            setShutdownRequested(dialogBeenden.isShutdownRequested());
        }

        if (tabAudiothek.activeDownloadCount() > 0) {
            var activeAudiothekDownloads = tabAudiothek.activeDownloadCount();
            var result = JOptionPane.showConfirmDialog(
                    this,
                    activeAudiothekDownloads == 1
                            ? "Es ist noch ein Audiothek-Download aktiv.\nTrotzdem beenden?"
                            : "Es sind noch " + activeAudiothekDownloads + " Audiothek-Downloads aktiv.\nTrotzdem beenden?",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.YES_NO_OPTION,
                    JOptionPane.WARNING_MESSAGE
            );
            if (result != JOptionPane.YES_OPTION) {
                return false;
            }
            tabAudiothek.pauseDownloadsForShutdown();
        }

        return true;
    }

    private void performApplicationShutdown() {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        try {
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

            logger.trace("Perform history maintenance.");
            try (SeenHistoryController history = new SeenHistoryController()) {
                history.performMaintenance();
            }

            logger.trace("Save bookmark list.");
            daten.getListeBookmarkList().saveToFile();

            // stop the download thread
            logger.trace("Stop Starter Thread.");
            daten.getStarterClass().shutdown();

            logger.trace("Close Notification center.");
            closeNotificationCenter();

            // Tabelleneinstellungen merken
            logger.trace("Save Tab Filme data.");
            tabFilme.disposePanel();

            logger.trace("Save Tab Download data.");
            tabDownloads.tabelleSpeichern();

            logger.trace("Disposing Tab Audiothek");
            tabAudiothek.disposePanel();

            logger.trace("Stop all downloads.");
            stopDownloads();

            logger.trace("Save app data.");
            daten.allesSpeichern();

            logger.trace("Shutdown pools.");
            shutdownTimerPool();
            waitForCommonPoolToComplete();

            //close main window
            logger.trace("Close main window.");
            dispose();

            //write all settings if not done already...
            logger.trace("Write app config.");
            ApplicationConfiguration.getInstance().writeConfiguration();

            if (resetSettingsOnQuit) {
                logger.trace("Move settings directory aside for reset.");
                SettingsResetService.moveSettingsDirectoryAside();
            }

            RuntimeStatistics.INSTANCE.printRuntimeStatistics();
            if (Config.isEnhancedLoggingEnabled()) {
                RuntimeStatistics.INSTANCE.printDataUsageStatistics();
            }
        } finally {
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }

        if (isShutdownRequested()) {
            logger.info("Requesting computer shutdown.");
            shutdownComputer();
        }

        System.exit(0);
    }

    private void shutdownTimerPool() {
        logger.trace("Entering shutdownTimerPool()");

        try {
            var taskList = TimerPool.shutdown(500, TimeUnit.MILLISECONDS);
            if (Config.isDebugModeEnabled() && !taskList.isEmpty()) {
                logger.trace("timerPool taskList was not empty: {}", taskList.toString());
            }
        }
        catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            logger.error("timerPool shutdown exception", e);
        }

        logger.trace("Leaving shutdownTimerPool()");
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
        logger.trace("Entering waitForCommonPoolToComplete()");

        var pool = ForkJoinPool.commonPool();
        boolean isQuiescent = pool.isQuiescent();

        while (pool.hasQueuedSubmissions() && !isQuiescent) {
            logger.trace("POOL SUBMISSIONS: {}", pool.getQueuedSubmissionCount());
            isQuiescent = pool.awaitQuiescence(500, TimeUnit.MILLISECONDS);
        }

        logger.trace("Leaving waitForCommonPoolToComplete()");
    }

    /**
     * Shutdown the computer depending on Operating System.
     */
    protected void shutdownComputer() {
        //default is none
    }

    static class NoIconAwtMenuItem extends MenuItem {
        public NoIconAwtMenuItem(@NonNull Action action) {
            super((String) action.getValue(Action.NAME));
            addActionListener(action);
        }
    }

    class ShowLogWindowAction extends AbstractAction {
        public ShowLogWindowAction() {
            super("Live Programm-Log anzeigen");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!logWindow.isVisible())
                logWindow.setVisible(true);
            logWindow.toFront();
        }
    }

    public class ToggleZappLivestreamsTabAction extends AbstractAction {
        private static final String TAB_TITLE = "Zapp Livestreams Tab ein-/ausblenden";
        private final LivestreamPanel livestreamPanel;

        public ToggleZappLivestreamsTabAction(LivestreamPanel livestreamPanel) {
            this.livestreamPanel = livestreamPanel;
            putValue(Action.NAME, TAB_TITLE);
        }

        private void toggleTab() {
            var tabIndex = tabbedPane.indexOfComponent(livestreamPanel);
            if (tabIndex == -1) {
                //install tab
                tabbedPane.add("Zapp Livestreams", livestreamPanel);
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_UI_SHOW_ZAPP_LIVESTREAMS, true);
            }
            else {
                tabbedPane.remove(tabIndex);
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_UI_SHOW_ZAPP_LIVESTREAMS, false);
            }
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            toggleTab();
        }
    }

    public class ToggleAudiothekTabAction extends AbstractAction {
        private static final String TAB_TITLE = "Audiothek Tab ein-/ausblenden";
        private final AudiothekPanel audiothekPanel;

        public ToggleAudiothekTabAction(AudiothekPanel audiothekPanel) {
            this.audiothekPanel = audiothekPanel;
            putValue(Action.NAME, TAB_TITLE);
        }

        private void toggleTab() {
            var tabIndex = tabbedPane.indexOfComponent(audiothekPanel);
            if (tabIndex == -1) {
                audiothekPanel.putClientProperty("JTabbedPane.tabClosable", true);
                audiothekPanel.putClientProperty("JTabbedPane.tabCloseCallback", (IntConsumer) _ -> actionPerformed(null));
                tabbedPane.add("Audiothek", audiothekPanel);
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_UI_SHOW_AUDIOTHEK, true);
            }
            else {
                tabbedPane.remove(tabIndex);
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_UI_SHOW_AUDIOTHEK, false);
            }
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            toggleTab();
        }
    }

}
