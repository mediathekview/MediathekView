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
import mediathek.config.application.ApplicationConfiguration;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.actions.*;
import mediathek.gui.bookmark.BookmarkDialog;
import mediathek.gui.dialog.DialogBeenden;
import mediathek.gui.dialog.LoadFilmListDialog;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.filmInformation.FilmInfoDialog;
import mediathek.gui.messages.*;
import mediathek.gui.progress.DownloadProgressIndicator;
import mediathek.gui.progress.NoDownloadProgressIndicator;
import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.gui.tabs.tab_livestreams.LivestreamPanel;
import mediathek.gui.tabs.tab_online_search.OnlineSearchPanel;
import mediathek.logging.LogDialog;
import mediathek.shutdown.ComputerShutdown;
import mediathek.swing.SwingDispatch;
import mediathek.tool.*;
import mediathek.tool.notification.GenericNotificationCenter;
import mediathek.tool.notification.INotificationCenter;
import mediathek.tool.notification.NotificationService;
import mediathek.tool.timer.TimerPool;
import mediathek.update.AutomaticFilmlistUpdate;
import mediathek.update.ProgramUpdateHost;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.InvocationTargetException;
import java.util.Comparator;
import java.util.HashMap;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class MediathekGui extends JFrame implements FilmBookmarkHost, DownloadControlHost, ProgramUpdateHost, TrayHost, SettingsDialogHost, SettingsResetHost, FilmListLoadHost {

    protected static final Logger logger = LogManager.getLogger();
    private static final String ICON_NAME = "MediathekView.png";
    private static final String ICON_PATH = "/mediathek/res/";
    private static final int ICON_WIDTH = 58;
    private static final int ICON_HEIGHT = 58;
    private static final String DISABLED_ACTION_KEY = "none";
    private static final int MIN_WINDOW_WIDTH = 800;
    private static final int MIN_WINDOW_HEIGHT = 600;
    private static final String ACTION_MAP_KEY_COPY_HQ_URL = "COPY_HQ_URL";
    private static final String ACTION_MAP_KEY_COPY_NORMAL_URL = "COPY_NORMAL_URL";
    private static final String TABBED_PANE_TRAILING_COMPONENT = "JTabbedPane.trailingComponent";
    private static final int COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS = 5;
    private static final ComputerShutdown NO_COMPUTER_SHUTDOWN = () -> {};
    private static final Function<MediathekGui, DownloadProgressIndicator> NO_DOWNLOAD_PROGRESS_INDICATOR_FACTORY = _ ->
            NoDownloadProgressIndicator.INSTANCE;
    private final AtomicBoolean applicationQuitInProgress = new AtomicBoolean();
    private final AtomicBoolean disposed = new AtomicBoolean();
    public final LoadFilmListAction loadFilmListAction;
    public final EditBlacklistAction editBlacklistAction = new EditBlacklistAction(this);
    public final ToggleBlacklistAction toggleBlacklistAction = new ToggleBlacklistAction();
    public final ShowFilmInformationAction showFilmInformationAction;
    /**
     * this property keeps track how many items are currently selected in the active table view
     */
    public final ListSelectedItemsProperty selectedListItemsProperty = new ListSelectedItemsProperty(0);
    private final PropertyChangeListener lookAndFeelListener = this::handleLookAndFeelChange;
    protected final Daten daten = Daten.getInstance();
    protected final PositionSavingTabbedPane tabbedPane = new PositionSavingTabbedPane();
    protected final JMenu jMenuHilfe = new JMenu();
    protected final SettingsAction settingsAction = new SettingsAction();
    protected final JToolBar commonToolBar = new JToolBar();
    protected final ManageBookmarkAction manageBookmarkAction = new ManageBookmarkAction(this);
    protected final ToggleDarkModeAction toggleDarkModeAction = new ToggleDarkModeAction(this);
    private final JMenu fontMenu = new JMenu("Schrift");
    private final JMenu jMenuDatei = new JMenu();
    private final JMenu jMenuFilme = new JMenu();
    private final JMenuBar jMenuBar = new JMenuBar();
    private final JMenu jMenuDownload = new JMenu();
    private final JMenu jMenuAbos = new JMenu();
    private final JMenu jMenuAnsicht = new JMenu();
    private final HashMap<JMenu, MenuTabSwitchListener> menuListeners = new HashMap<>();
    private final MainWindowTabRegistry tabRegistry = new MainWindowTabRegistry(tabbedPane);
    private final SearchProgramUpdateAction searchProgramUpdateAction;
    private final MemoryMonitorAction showMemoryMonitorAction = new MemoryMonitorAction(this);
    private final ManageAboAction manageAboAction = new ManageAboAction(this);
    private final ShowBandwidthUsageAction showBandwidthUsageAction = new ShowBandwidthUsageAction(this);
    private final MainWindowDialogCoordinator dialogCoordinator =
            new MainWindowDialogCoordinator(this, showMemoryMonitorAction, showBandwidthUsageAction, manageAboAction);
    private final ShowLuceneTutorialAction showLuceneTutorialAction = new ShowLuceneTutorialAction(this);
    private final LivestreamPanel tabLivestreams = new LivestreamPanel(this);
    private final ToggleZappLivestreamsTabAction toggleZappLivestreamsTabAction = new ToggleZappLivestreamsTabAction(tabbedPane, tabLivestreams);
    private final OnlineSearchPanel tabOnlineSearch = new OnlineSearchPanel(this);
    private final ToggleOnlineSearchTabAction toggleOnlineSearchTabAction = new ToggleOnlineSearchTabAction(tabbedPane, tabOnlineSearch);
    private final AudioRepository audiothekRepository = new AudioRepository();
    private final AudiothekPanel tabAudiothek = new AudiothekPanel(audiothekRepository, this);
    private final ToggleAudiothekTabAction toggleAudiothekTabAction = new ToggleAudiothekTabAction(tabbedPane, tabAudiothek);
    private final LogDialog logDialog = new LogDialog(this);
    private final Supplier<INotificationCenter> notificationCenterFactory;
    private final ComputerShutdown computerShutdown;
    private final DownloadProgressIndicator downloadProgressIndicator;
    private final MainWindowController mainWindowController;
    private final MainWindowPlatformIntegration platformIntegration;
    private final MainWindowProgramUpdateCoordinator programUpdateCoordinator =
            new MainWindowProgramUpdateCoordinator(this);
    private final MainWindowStatusBarController statusBarController =
            new MainWindowStatusBarController(this, this::runOnEventDispatchThreadAndWait);
    private final FilmlistProgressPresenter filmlistDownloadProgressListener =
            new FilmlistProgressPresenter(SwingDispatch.INSTANCE, statusBarController::showProgress);
    private final ListenerFilmeLaden filmListListener;
    private GuiFilme tabFilme;
    private GuiDownloads tabDownloads;
    private AutomaticFilmlistUpdate automaticFilmlistUpdate;
    private StartupFilmlistLoader startupFilmlistLoader;
    private boolean resetSettingsOnQuit;
    private boolean menuTabSwitchListenersInstalled;
    private final MainWindowLifecycle mainWindowLifecycle;

    public MediathekGui() {
        this(GenericNotificationCenter::new);
    }

    protected MediathekGui(Supplier<INotificationCenter> notificationCenterFactory) {
        this(notificationCenterFactory, NO_COMPUTER_SHUTDOWN);
    }

    protected MediathekGui(Supplier<INotificationCenter> notificationCenterFactory, ComputerShutdown computerShutdown) {
        this(notificationCenterFactory, computerShutdown, NO_DOWNLOAD_PROGRESS_INDICATOR_FACTORY);
    }

    protected MediathekGui(
            Supplier<INotificationCenter> notificationCenterFactory,
            ComputerShutdown computerShutdown,
            Function<MediathekGui, DownloadProgressIndicator> downloadProgressIndicatorFactory
    ) {
        this.notificationCenterFactory = Objects.requireNonNull(notificationCenterFactory);
        this.computerShutdown = Objects.requireNonNull(computerShutdown);
        this.downloadProgressIndicator = Objects.requireNonNull(
                Objects.requireNonNull(downloadProgressIndicatorFactory).apply(this)
        );
        loadFilmListAction = new LoadFilmListAction(this);
        showFilmInformationAction = new ShowFilmInformationAction(this::getFilmInfoDialog);
        filmListListener = new MainWindowFilmListListener(
                SwingDispatch.INSTANCE,
                () -> loadFilmListAction,
                () -> daten.allesSpeichern(),
                this::setupAutomaticFilmlistReload
        );
        mainWindowLifecycle = new MainWindowLifecycle(
                this,
                daten,
                this,
                lookAndFeelListener,
                filmlistDownloadProgressListener,
                filmListListener,
                this,
                this::getCurrentZeitraumFilterValue
        );
        searchProgramUpdateAction = new SearchProgramUpdateAction(this);
        platformIntegration = new MainWindowPlatformIntegration(
                this,
                loadFilmListAction,
                this::setupSystemTray
        );
        mainWindowController = createMainWindowController();
    }

    private MainWindowController createMainWindowController() {
        return new MainWindowController(
                this::initializeMainWindow,
                this::startMainWindowRuntime
        );
    }

    public void start() {
        mainWindowController.start();
    }

    private void initializeMainWindow() {
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

        setupScrollBarWidth();
        UIManager.put("TabbedPane.showTabSeparators", true);
        mainWindowLifecycle.registerLookAndFeelListener();

        setupAlternatingRowColors();

        SplashScreenLifecycle.update(UIProgressState.LOAD_MAINWINDOW);

        getContentPane().setLayout(new BorderLayout());

        setIconAndWindowImage();

        createMenuBar();

        configureMenuKeyboardShortcuts();

        SplashScreenLifecycle.update(UIProgressState.WAIT_FOR_HISTORY_DATA);
        waitForHistoryDataLoadingToComplete();

        SplashScreenLifecycle.update(UIProgressState.CREATE_STATUS_BAR);
        createStatusBar();

        SplashScreenLifecycle.update(UIProgressState.SETUP_FILM_LISTENERS);
        mainWindowLifecycle.registerFilmListListeners();

        SplashScreenLifecycle.update(UIProgressState.LOAD_TABS);
        initTabs();

        SplashScreenLifecycle.update(UIProgressState.INIT_MENUS);
        initMenus();

        setupNotificationCenter();

        createCommonToolBar();
        installToolBar();
        mapFilmUrlCopyCommands();

        SplashScreenLifecycle.update(UIProgressState.FINISHED);
    }

    private void startMainWindowRuntime() {
        mainWindowLifecycle.start();
        platformIntegration.setupTaskbarMenuLater();
        platformIntegration.setupSystemTray();
        setApplicationWindowSizeLater();
        loadFilmlist();
        setupAutomaticUpdateCheck();
        setupShutdownHook();
        checkInvalidRegularExpressions();
        setupFilmInfoDialog();
        resetTabPlacement();
        platformIntegration.setupRavenNotifications();
        performGeoCountryStartupCheck();
    }

    private void setApplicationWindowSizeLater() {
        SwingUtilities.invokeLater(this::setApplicationWindowSize);
    }

    private void setupAutomaticUpdateCheck() {
        programUpdateCoordinator.startFromConfiguration();
    }

    @Override
    public void dispose() {
        if (disposed.compareAndSet(false, true)) {
            mainWindowLifecycle.close();
            closeStartupFilmlistLoader();
            closeFilmlistDownloadProgress();
            closeAutomaticFilmlistUpdate();
            closeProgramUpdateCoordinator();
            closeSystemTray();
            closeNotificationCenter();
            downloadProgressIndicator.close();
        }
        super.dispose();
    }

    private void handleLookAndFeelChange(PropertyChangeEvent evt) {
        if (evt.getPropertyName().equalsIgnoreCase("lookAndFeel")) {
            statusBarController.updateComponentTreeUi();
        }
    }

    @Override
    public JFrame ownerFrame() {
        return this;
    }

    @Override
    public void showMainWindow() {
        setVisible(true);
    }

    @Override
    public void toggleMainWindowVisibility() {
        setVisible(!isVisible());
        if (isVisible()) {
            toFront();
            requestFocusInWindow();
        }
    }

    @Override
    public void refreshSystemTray() {
        initializeSystemTray();
    }

    @Override
    public void repaintMainWindow() {
        repaint();
    }

    public int getFilmTableRowCount() {
        return tabFilme.getTableRowCount();
    }

    public String getCurrentZeitraumFilterValue() {
        return tabFilme.getCurrentZeitraumFilterValue();
    }

    @Override
    public BookmarkDialog getBookmarkDialog() {
        return tabFilme.getBookmarkDialog();
    }

    @Override
    public void showManageBookmarkWindow() {
        tabFilme.showManageBookmarkWindow();
    }

    @Override
    public void resetFilterDialogPosition() {
        tabFilme.resetFilterDialogPosition();
    }

    @Override
    public void repaintFilmTab() {
        tabFilme.repaint();
    }

    @Override
    public void stopAllWaitingDownloads() {
        tabDownloads.stopAllWaitingDownloads();
    }

    private void setupFilmInfoDialog() {
        dialogCoordinator.setupFilmInfoDialog();
    }

    private void waitForHistoryDataLoadingToComplete() {
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

    private void mapFilmUrlCopyCommands() {
        final var im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_H, GuiFunktionen.getPlatformControlKey() |
                KeyEvent.SHIFT_DOWN_MASK | KeyEvent.ALT_DOWN_MASK), ACTION_MAP_KEY_COPY_HQ_URL);
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_N, GuiFunktionen.getPlatformControlKey() |
                KeyEvent.SHIFT_DOWN_MASK | KeyEvent.ALT_DOWN_MASK), ACTION_MAP_KEY_COPY_NORMAL_URL);

        final var am = jMenuBar.getActionMap();
        am.put(ACTION_MAP_KEY_COPY_HQ_URL, tabFilme.copyHqUrlToClipboardAction());
        am.put(ACTION_MAP_KEY_COPY_NORMAL_URL, tabFilme.copyNormalUrlToClipboardAction());
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

    protected void createCommonToolBar() {
        createToolbarBuilder().createCommonToolBar();
    }

    private MainWindowToolbarBuilder createToolbarBuilder() {
        return new MainWindowToolbarBuilder(
                commonToolBar,
                loadFilmListAction,
                showFilmInformationAction,
                toggleBlacklistAction,
                editBlacklistAction,
                manageAboAction,
                settingsAction,
                this::createDarkModeToggleButton,
                this::setToolBarProperties
        );
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
        final boolean showNotifications = ApplicationConfiguration.getInstance().getShowNotifications();
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
        platformIntegration.setupSystemTrayLater();
    }

    private void setIconAndWindowImage() {
        setWindowTitle();
        setIconImage(GetIcon.getIcon(ICON_NAME, ICON_PATH, ICON_WIDTH, ICON_HEIGHT).getImage());
    }

    private void configureMenuKeyboardShortcuts() {
        if (shouldDisableF10MenuShortcut()) {
            var im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F10, 0), DISABLED_ACTION_KEY);
        }
    }

    protected boolean shouldDisableF10MenuShortcut() {
        return true;
    }

    protected void createMenuBar() {
        setJMenuBar(createMenuBuilder().createMenuBar());
    }

    private MainWindowMenuBuilder createMenuBuilder() {
        return new MainWindowMenuBuilder(
                this,
                daten,
                jMenuBar,
                jMenuDatei,
                jMenuFilme,
                jMenuDownload,
                jMenuAbos,
                fontMenu,
                jMenuAnsicht,
                jMenuHilfe,
                createMenuPolicy(),
                tabRegistry,
                () -> tabFilme,
                () -> tabDownloads,
                logDialog,
                loadFilmListAction,
                settingsAction,
                showMemoryMonitorAction,
                manageAboAction,
                showBandwidthUsageAction,
                showLuceneTutorialAction,
                showFilmInformationAction,
                manageBookmarkAction,
                searchProgramUpdateAction
        );
    }

    protected MainWindowMenuPolicy createMenuPolicy() {
        return DefaultMainWindowMenuPolicy.INSTANCE;
    }

    /**
     * Read a local filmlist or load a new one in auto mode.
     */
    private void loadFilmlist() {
        statusBarController.installStartupProgress();
        startupFilmlistLoader = new StartupFilmlistLoader(
                daten,
                statusBarController.getStartupProgressLabel(),
                statusBarController.getStartupProgressBar(),
                this::finishStartupFilmlistLoad
        );
        startupFilmlistLoader.start();
    }

    private void finishStartupFilmlistLoad(boolean remoteUpdateStarted, boolean failed) {
        try {
            if (!remoteUpdateStarted) {
                Daten.getInstance().getFilmeLaden().notifyFertig(new ListenerFilmeLadenEvent("", "", 100, 100, failed));
            }
        } finally {
            statusBarController.uninstallStartupProgress();
        }
    }

    /**
     * Create the status bar item.
     */
    private void createStatusBar() {
        statusBarController.createStatusBar();
        mainWindowLifecycle.registerFilmlistProgressListener();
    }

    @Handler
    private void handleFilmListReadStopEvent(FilmListReadStopEvent event) {
        SwingUtilities.invokeLater(this::closeFilmlistDownloadProgress);
    }

    private void closeFilmlistDownloadProgress() {
        filmlistDownloadProgressListener.close();
    }

    public StatusBarProgressHandle showStatusBarProgress() {
        return statusBarController.showProgress();
    }

    @Override
    public void setFilmIndexingActionsEnabled(boolean enabled) {
        toggleBlacklistAction.setEnabled(enabled);
        editBlacklistAction.setEnabled(enabled);
        loadFilmListAction.setEnabled(enabled);
    }

    private void runOnEventDispatchThreadAndWait(String description, Runnable action) {
        if (SwingUtilities.isEventDispatchThread()) {
            action.run();
            return;
        }

        try {
            SwingUtilities.invokeAndWait(action);
        }
        catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            logger.error("{} interrupted", description, e);
        }
        catch (InvocationTargetException e) {
            throw new IllegalStateException(description + " failed", e.getCause());
        }
    }

    public FilmInfoDialog getFilmInfoDialog() {
        return dialogCoordinator.getFilmInfoDialog();
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
            var bounds = ApplicationConfiguration.getInstance().getMainWindowBounds(MIN_WINDOW_WIDTH, MIN_WINDOW_HEIGHT);
            setBounds(bounds.x(), bounds.y(), bounds.width(), bounds.height());
        }
        catch (NoSuchElementException _) {
            //in case of any error, just make the window maximized
            setExtendedState(JFrame.MAXIMIZED_BOTH);
        }
    }

    private void setApplicationWindowSize() {
        if (CommandLineOptions.isStartMaximized() ||
                ApplicationConfiguration.getInstance().getMainWindowMaximized()) {
            setExtendedState(JFrame.MAXIMIZED_BOTH);
        }
        else
            restoreSizeFromConfig();

        SwingUtilities.invokeLater(() -> addComponentListener(new WindowLocationConfigSaverListener()));
    }

    private void closeStartupFilmlistLoader() {
        if (startupFilmlistLoader != null) {
            startupFilmlistLoader.close();
            startupFilmlistLoader = null;
        }
    }

    /**
     * Reload filmlist every 24h when in automatic mode.
     */
    private void setupAutomaticFilmlistReload() {
        closeAutomaticFilmlistUpdate();

        final Runnable performUpdate = () -> {
            if (FilmListUpdateType.AUTOMATIC.isConfigured()) {
                //if downloads are running, don´t update
                if (daten.getListeDownloads().unfinishedDownloads() == 0) {
                    loadFilmListAction.setEnabled(false);
                    performFilmListLoadOperation(false);
                }
            }
        };

        automaticFilmlistUpdate = new AutomaticFilmlistUpdate(performUpdate);
        automaticFilmlistUpdate.start();
    }

    @Handler
    private void handleUpdateStateChanged(UpdateStateChangedEvent e) {
        SwingUtilities.invokeLater(() -> programUpdateCoordinator.update(e.isActive()));
    }

    private void closeProgramUpdateCoordinator() {
        programUpdateCoordinator.close();
    }

    public void initializeSystemTray() {
        platformIntegration.initializeSystemTray();
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
        registerMainWindowTabs();
        tabRegistry.installVisibleTabs();

        if (ApplicationConfiguration.getInstance().getRestoreSelectedTab())
            tabbedPane.restoreSavedTabPosition();
        tabbedPane.installChangeListener();

        SplashScreenLifecycle.update(UIProgressState.CONFIGURE_TABS);
        configureTabPlacement();
        configureTabIcons();
    }

    private void registerMainWindowTabs() {
        tabRegistry.register(new MainWindowTab(
                GuiFilme.NAME,
                tabFilme,
                () -> true,
                () -> GetIcon.getProgramIcon("tab-film.png", 32, 32),
                null,
                () -> tabFilme.disposePanel()
        ));
        tabRegistry.register(new MainWindowTab(
                GuiDownloads.NAME,
                tabDownloads,
                () -> true,
                () -> GetIcon.getProgramIcon("tab-download.png", 32, 32),
                null,
                () -> tabDownloads.tabelleSpeichern()
        ));
        tabRegistry.register(new MainWindowTab(
                "Onlinesuche",
                tabOnlineSearch,
                () -> ApplicationConfiguration.getInstance().getOnlineSearchTabVisible(),
                null,
                toggleOnlineSearchTabAction
        ));
        tabRegistry.register(new MainWindowTab(
                "zapp Livestreams",
                tabLivestreams,
                () -> ApplicationConfiguration.getInstance().getZappLivestreamsTabVisible(),
                null,
                toggleZappLivestreamsTabAction
        ));
        tabRegistry.register(new MainWindowTab(
                "Audiothek",
                tabAudiothek,
                () -> ApplicationConfiguration.getInstance().getAudiothekTabVisible(),
                null,
                toggleAudiothekTabAction,
                tabAudiothek::disposePanel
        ));
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
        final boolean topPosition = ApplicationConfiguration.getInstance().getTabPositionTop();
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
        tabRegistry.configureIcons(ApplicationConfiguration.getInstance().getMainWindowTabIcons());
    }

    /**
     * Message bus handler which gets called when a download is started.
     *
     * @param msg Information about the download
     */
    @Handler
    protected void handleDownloadStart(DownloadStartEvent msg) {
        downloadProgressIndicator.downloadStarted();
    }

    /**
     * Message bus handler which gets called when a download is stopped.
     *
     * @param msg Information about the download
     */
    @Handler
    protected void handleDownloadFinishedEvent(DownloadFinishedEvent msg) {
        downloadProgressIndicator.downloadFinished();
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
        menuListeners.put(jMenuFilme, new MenuTabSwitchListener(this, tabFilme));
        menuListeners.put(jMenuDownload, new MenuTabSwitchListener(this, tabDownloads));

        //now assign if really necessary
        if (ApplicationConfiguration.getInstance().getInstallTabSwitchListener()) {
            installConfiguredMenuTabSwitchListeners();
        }
    }

    private void installConfiguredMenuTabSwitchListeners() {
        if (menuTabSwitchListenersInstalled) {
            return;
        }

        jMenuFilme.addMenuListener(menuListeners.get(jMenuFilme));
        jMenuDownload.addMenuListener(menuListeners.get(jMenuDownload));
        menuTabSwitchListenersInstalled = true;
    }

    private void removeConfiguredMenuTabSwitchListeners() {
        if (!menuTabSwitchListenersInstalled) {
            return;
        }

        jMenuFilme.removeMenuListener(menuListeners.get(jMenuFilme));
        jMenuDownload.removeMenuListener(menuListeners.get(jMenuDownload));
        menuTabSwitchListenersInstalled = false;
    }

    /**
     * Handle the install/or remove event sent from settings dialog
     */
    @Handler
    protected void handleInstallTabSwitchListenerEvent(InstallTabSwitchListenerEvent msg) {
        if (!supportsAutomaticMenuTabSwitching()) {
            return;
        }

        switch (msg.getEvent()) {
            case INSTALL -> SwingUtilities.invokeLater(this::installConfiguredMenuTabSwitchListeners);
            case REMOVE -> SwingUtilities.invokeLater(this::removeConfiguredMenuTabSwitchListeners);
        }
    }

    @Handler
    private void handleFilmlistWriteStartEvent(FilmListWriteStartEvent e) {
        SwingUtilities.invokeLater(() -> loadFilmListAction.setEnabled(false));
    }

    @Handler
    private void handleFilmlistWriteStopEvent(FilmListWriteStopEvent e) {
        SwingUtilities.invokeLater(() -> loadFilmListAction.setEnabled(true));
    }

    protected void initMenus() {
        installMenuTabSwitchListener();
        createMenuBuilder().initializeMenus();
    }

    public void performFilmListLoadOperation(boolean manualMode) {
        if (manualMode || FilmListUpdateType.MANUAL.isConfigured()) {
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
        return dialogCoordinator.getSettingsDialog();
    }

    public void requestSettingsResetOnQuit() {
        resetSettingsOnQuit = true;
    }

    public void restoreStartupDialogs() {
        dialogCoordinator.restoreStartupDialogs();
    }

    @Override
    public boolean quitApplication() {
        return quitApplication(false);
    }

    public boolean quitApplication(boolean shutdownComputer) {
        if (!applicationQuitInProgress.compareAndSet(false, true)) {
            return true;
        }

        var confirmation = confirmApplicationQuitOnEdt(shutdownComputer);
        if (!confirmation.canQuit()) {
            applicationQuitInProgress.set(false);
            return false;
        }

        startApplicationShutdown(confirmation.shutdownComputer());
        return true;
    }

    private QuitConfirmation confirmApplicationQuitOnEdt(boolean shutdownComputer) {
        if (SwingUtilities.isEventDispatchThread()) {
            return confirmApplicationQuit(shutdownComputer);
        }

        var confirmation = new AtomicReference<QuitConfirmation>();
        runOnEventDispatchThreadAndWait(
                "Confirm application quit",
                () -> confirmation.set(confirmApplicationQuit(shutdownComputer))
        );
        return Objects.requireNonNullElseGet(confirmation.get(), QuitConfirmation::declined);
    }

    private QuitConfirmation confirmApplicationQuit(boolean shutdownComputer) {
        if (daten.getListeDownloads().unfinishedDownloads() > 0) {
            // erst mal prüfen ob noch Downloads laufen
            DialogBeenden dialogBeenden = new DialogBeenden(this, this);
            dialogBeenden.setVisible(true);
            if (!dialogBeenden.getApplicationCanTerminate()) {
                return QuitConfirmation.declined();
            }
            shutdownComputer = dialogBeenden.isShutdownRequested();
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
                return QuitConfirmation.declined();
            }
            tabAudiothek.pauseDownloadsForShutdown();
        }

        return new QuitConfirmation(true, shutdownComputer);
    }

    private record QuitConfirmation(boolean canQuit, boolean shutdownComputer) {
        private static QuitConfirmation declined() {
            return new QuitConfirmation(false, false);
        }
    }

    private void startApplicationShutdown(boolean shutdownComputer) {
        Thread.ofPlatform()
                .name("MediathekView-shutdown")
                .daemon(false)
                .start(() -> performApplicationShutdown(shutdownComputer));
    }

    private void performApplicationShutdown(boolean shutdownComputer) {
        createShutdownCoordinator().shutdown(shutdownComputer);
        System.exit(0);
    }

    private MainWindowShutdownCoordinator createShutdownCoordinator() {
        return new MainWindowShutdownCoordinator(
                this,
                daten,
                dialogCoordinator,
                tabRegistry,
                computerShutdown,
                resetSettingsOnQuit,
                this::closeAutomaticFilmlistUpdate,
                this::closeProgramUpdateCoordinator,
                this::closeSystemTray,
                this::closeNotificationCenter,
                this::shutdownTimerPool,
                this::waitForCommonPoolToComplete,
                this::runOnEventDispatchThreadAndWait
        );
    }

    private void closeSystemTray() {
        platformIntegration.closeSystemTray();
    }

    private void closeAutomaticFilmlistUpdate() {
        if (automaticFilmlistUpdate != null) {
            automaticFilmlistUpdate.close();
            automaticFilmlistUpdate = null;
        }
    }

    private void shutdownTimerPool() {
        logger.trace("Entering shutdownTimerPool()");

        try {
            var taskList = TimerPool.shutdown(500, TimeUnit.MILLISECONDS);
            if (CommandLineOptions.isDebugModeEnabled() && !taskList.isEmpty()) {
                logger.trace("timerPool taskList was not empty: {}", taskList.toString());
            }
        }
        catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            logger.error("timerPool shutdown exception", e);
        }

        logger.trace("Leaving shutdownTimerPool()");
    }

    private void waitForCommonPoolToComplete() {
        logger.trace("Entering waitForCommonPoolToComplete()");

        var pool = ForkJoinPool.commonPool();
        if (!pool.awaitQuiescence(COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
            logger.warn(
                    "Common pool did not become quiescent within {} seconds. Continuing shutdown.",
                    COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS
            );
        }

        logger.trace("Leaving waitForCommonPoolToComplete()");
    }

}
