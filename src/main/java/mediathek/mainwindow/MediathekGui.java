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
import mediathek.gui.actions.*;
import mediathek.gui.bookmark.BookmarkDialog;
import mediathek.gui.dialog.DialogBeenden;
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
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class MediathekGui extends JFrame implements FilmBookmarkHost, DownloadControlHost, ProgramUpdateHost, TrayHost, SettingsDialogHost, SettingsResetHost, FilmListLoadHost {

    private static final Logger logger = LogManager.getLogger();
    private static final String ICON_NAME = "MediathekView.png";
    private static final String ICON_PATH = "/mediathek/res/";
    private static final int ICON_WIDTH = 58;
    private static final int ICON_HEIGHT = 58;
    private static final String DISABLED_ACTION_KEY = "none";
    private static final int MIN_WINDOW_WIDTH = 800;
    private static final int MIN_WINDOW_HEIGHT = 600;
    private static final String ACTION_MAP_KEY_COPY_HQ_URL = "COPY_HQ_URL";
    private static final String ACTION_MAP_KEY_COPY_NORMAL_URL = "COPY_NORMAL_URL";
    private static final int COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS = 5;
    private static final ComputerShutdown NO_COMPUTER_SHUTDOWN = () -> {};
    private static final Function<MediathekGui, DownloadProgressIndicator> NO_DOWNLOAD_PROGRESS_INDICATOR_FACTORY = _ ->
            NoDownloadProgressIndicator.INSTANCE;
    private static final MainWindowToolbarInstaller DEFAULT_TOOLBAR_INSTALLER = new MainWindowToolbarInstaller() {
        @Override
        public void configure(JToolBar commonToolBar) {
            commonToolBar.setFloatable(true);
            commonToolBar.setName("Allgemein");
        }

        @Override
        public void install(Container contentPane, JTabbedPane tabbedPane, JToolBar commonToolBar) {
            tabbedPane.putClientProperty(MainWindowTabPlacementController.TRAILING_COMPONENT_KEY, commonToolBar);
            tabbedPane.putClientProperty(MainWindowTabPlacementController.TAB_ROTATION_KEY, "auto");
        }
    };
    private final AtomicBoolean applicationQuitInProgress = new AtomicBoolean();
    private final AtomicBoolean disposed = new AtomicBoolean();
    private final LoadFilmListAction loadFilmListAction;
    private final EditBlacklistAction editBlacklistAction = new EditBlacklistAction(this);
    private final ToggleBlacklistAction toggleBlacklistAction = new ToggleBlacklistAction();
    private final ShowFilmInformationAction showFilmInformationAction;
    /**
     * this property keeps track how many items are currently selected in the active table view
     */
    private final ListSelectedItemsProperty selectedListItemsProperty = new ListSelectedItemsProperty(0);
    private final Daten daten = Daten.getInstance();
    private final PositionSavingTabbedPane tabbedPane = new PositionSavingTabbedPane();
    private final JMenu jMenuHilfe = new JMenu();
    private final SettingsAction settingsAction = new SettingsAction();
    private final JToolBar commonToolBar = new JToolBar();
    private final ManageBookmarkAction manageBookmarkAction = new ManageBookmarkAction(this);
    private final ToggleDarkModeAction toggleDarkModeAction = new ToggleDarkModeAction(this);
    private final JMenu fontMenu = new JMenu("Schrift");
    private final JMenu jMenuDatei = new JMenu();
    private final JMenu jMenuFilme = new JMenu();
    private final JMenuBar jMenuBar = new JMenuBar();
    private final JMenu jMenuDownload = new JMenu();
    private final JMenu jMenuAbos = new JMenu();
    private final JMenu jMenuAnsicht = new JMenu();
    private final MainWindowTabRegistry tabRegistry = new MainWindowTabRegistry(tabbedPane);
    private GuiFilme tabFilme;
    private GuiDownloads tabDownloads;
    private final MainWindowMenuTabSwitchController menuTabSwitchController;
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
    private final MainWindowDarkModeActionPlacement darkModeActionPlacement;
    private final MainWindowToolbarInstaller toolbarInstaller;
    private final MainWindowTabPlacementController tabPlacementController;
    private final MainWindowMenuPolicy menuPolicy;
    private final MainWindowScrollBarConfigurator scrollBarConfigurator;
    private final MainWindowSystemTrayController systemTrayController;
    private final boolean disableF10MenuShortcut;
    private final Consumer<MediathekGui> afterMenusInitialized;
    private final MainWindowController mainWindowController;
    private final MainWindowPlatformIntegration platformIntegration;
    private final MainWindowProgramUpdateCoordinator programUpdateCoordinator =
            new MainWindowProgramUpdateCoordinator(this);
    private final MainWindowStatusBarController statusBarController =
            new MainWindowStatusBarController(
                    this,
                    selectedListItemsProperty,
                    this::getFilmTableRowCount,
                    this::runOnEventDispatchThreadAndWait
            );
    private final MainWindowFilmlistLoadCoordinator filmlistLoadCoordinator =
            new MainWindowFilmlistLoadCoordinator(this, daten, statusBarController);
    private final FilmlistProgressPresenter filmlistDownloadProgressListener =
            new FilmlistProgressPresenter(SwingDispatch.INSTANCE, statusBarController::showProgress);
    private final MainWindowFilmlistReloadCoordinator filmlistReloadCoordinator;
    private boolean resetSettingsOnQuit;
    private final MainWindowLifecycle mainWindowLifecycle;

    public MediathekGui() {
        this(
                GenericNotificationCenter::new,
                NO_COMPUTER_SHUTDOWN,
                NO_DOWNLOAD_PROGRESS_INDICATOR_FACTORY,
                MainWindowDarkModeActionPlacement.TOOL_BAR,
                DEFAULT_TOOLBAR_INSTALLER,
                new MainWindowTabPlacementController(true),
                DefaultMainWindowMenuPolicy.INSTANCE,
                true,
                DefaultMainWindowScrollBarConfigurator.INSTANCE,
                DefaultMainWindowSystemTrayController.INSTANCE,
                true,
                _ -> {}
        );
    }

    protected MediathekGui(
            Supplier<INotificationCenter> notificationCenterFactory,
            ComputerShutdown computerShutdown,
            MainWindowDarkModeActionPlacement darkModeActionPlacement,
            MainWindowSystemTrayController systemTrayController
    ) {
        this(
                notificationCenterFactory,
                computerShutdown,
                NO_DOWNLOAD_PROGRESS_INDICATOR_FACTORY,
                darkModeActionPlacement,
                DEFAULT_TOOLBAR_INSTALLER,
                new MainWindowTabPlacementController(true),
                DefaultMainWindowMenuPolicy.INSTANCE,
                true,
                DefaultMainWindowScrollBarConfigurator.INSTANCE,
                systemTrayController,
                true,
                _ -> {}
        );
    }

    protected MediathekGui(
            Supplier<INotificationCenter> notificationCenterFactory,
            ComputerShutdown computerShutdown,
            Function<MediathekGui, DownloadProgressIndicator> downloadProgressIndicatorFactory,
            MainWindowToolbarInstaller toolbarInstaller,
            MainWindowTabPlacementController tabPlacementController,
            MainWindowMenuPolicy menuPolicy,
            boolean automaticMenuTabSwitchingSupported,
            MainWindowScrollBarConfigurator scrollBarConfigurator,
            MainWindowSystemTrayController systemTrayController,
            boolean disableF10MenuShortcut,
            Consumer<MediathekGui> afterMenusInitialized
    ) {
        this(
                notificationCenterFactory,
                computerShutdown,
                downloadProgressIndicatorFactory,
                MainWindowDarkModeActionPlacement.TOOL_BAR,
                toolbarInstaller,
                tabPlacementController,
                menuPolicy,
                automaticMenuTabSwitchingSupported,
                scrollBarConfigurator,
                systemTrayController,
                disableF10MenuShortcut,
                afterMenusInitialized
        );
    }

    protected MediathekGui(
            Supplier<INotificationCenter> notificationCenterFactory,
            ComputerShutdown computerShutdown,
            Function<MediathekGui, DownloadProgressIndicator> downloadProgressIndicatorFactory,
            MainWindowDarkModeActionPlacement darkModeActionPlacement
    ) {
        this(
                notificationCenterFactory,
                computerShutdown,
                downloadProgressIndicatorFactory,
                darkModeActionPlacement,
                DEFAULT_TOOLBAR_INSTALLER,
                new MainWindowTabPlacementController(true),
                DefaultMainWindowMenuPolicy.INSTANCE,
                true,
                DefaultMainWindowScrollBarConfigurator.INSTANCE,
                DefaultMainWindowSystemTrayController.INSTANCE,
                true,
                _ -> {}
        );
    }

    private MediathekGui(
            Supplier<INotificationCenter> notificationCenterFactory,
            ComputerShutdown computerShutdown,
            Function<MediathekGui, DownloadProgressIndicator> downloadProgressIndicatorFactory,
            MainWindowDarkModeActionPlacement darkModeActionPlacement,
            MainWindowToolbarInstaller toolbarInstaller,
            MainWindowTabPlacementController tabPlacementController,
            MainWindowMenuPolicy menuPolicy,
            boolean automaticMenuTabSwitchingSupported,
            MainWindowScrollBarConfigurator scrollBarConfigurator,
            MainWindowSystemTrayController systemTrayController,
            boolean disableF10MenuShortcut,
            Consumer<MediathekGui> afterMenusInitialized
    ) {
        this.notificationCenterFactory = Objects.requireNonNull(notificationCenterFactory);
        this.computerShutdown = Objects.requireNonNull(computerShutdown);
        this.darkModeActionPlacement = Objects.requireNonNull(darkModeActionPlacement);
        this.toolbarInstaller = Objects.requireNonNull(toolbarInstaller);
        this.tabPlacementController = Objects.requireNonNull(tabPlacementController);
        this.menuPolicy = Objects.requireNonNull(menuPolicy);
        this.scrollBarConfigurator = Objects.requireNonNull(scrollBarConfigurator);
        this.systemTrayController = Objects.requireNonNull(systemTrayController);
        this.disableF10MenuShortcut = disableF10MenuShortcut;
        this.afterMenusInitialized = Objects.requireNonNull(afterMenusInitialized);
        menuTabSwitchController = new MainWindowMenuTabSwitchController(
                tabbedPane,
                jMenuFilme,
                jMenuDownload,
                () -> tabFilme,
                () -> tabDownloads,
                automaticMenuTabSwitchingSupported
        );
        this.downloadProgressIndicator = Objects.requireNonNull(
                Objects.requireNonNull(downloadProgressIndicatorFactory).apply(this)
        );
        loadFilmListAction = new LoadFilmListAction(this);
        showFilmInformationAction = new ShowFilmInformationAction(this::getFilmInfoDialog);
        PropertyChangeListener lookAndFeelListener = this::handleLookAndFeelChange;
        filmlistReloadCoordinator = new MainWindowFilmlistReloadCoordinator(
                daten,
                loadFilmListAction,
                () -> filmlistLoadCoordinator.performFilmListLoadOperation(false)
        );
        ListenerFilmeLaden filmListListener = new MainWindowFilmListListener(
                SwingDispatch.INSTANCE,
                () -> loadFilmListAction,
                daten::allesSpeichern,
                filmlistReloadCoordinator::setupAutomaticFilmlistReload
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
                this::setupSystemTray,
                systemTrayController
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
        filmlistLoadCoordinator.loadStartupFilmlist();
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
            filmlistLoadCoordinator.close();
            closeFilmlistDownloadProgress();
            filmlistReloadCoordinator.close();
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
    public @NonNull JFrame ownerFrame() {
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
        platformIntegration.initializeSystemTray();
    }

    @Override
    public void repaintMainWindow() {
        repaint();
    }

    private int getFilmTableRowCount() {
        return tabFilme.getTableRowCount();
    }

    private String getCurrentZeitraumFilterValue() {
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

    public void setSelectedListItemsCount(long count) {
        selectedListItemsProperty.setSelectedItems(count);
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

    public final boolean supportsAutomaticMenuTabSwitching() {
        return menuTabSwitchController.supportsAutomaticSwitching();
    }

    private void resetTabPlacement() {
        tabPlacementController.resetTabPlacement();
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

    private void setupScrollBarWidth() {
        scrollBarConfigurator.configure();
    }

    /**
     * Check if alternate row colors in table should be used.
     * Overridden in subclasses for the different OSes.
     *
     * @return true when alternating row colors should be used, false otherwise.
     */
    private boolean useAlternateRowColors() {
        return true;
    }

    public void setupAlternatingRowColors() {
        if (useAlternateRowColors())
            UIManager.put("Table.alternateRowColor", MVColor.getAlternatingRowColor());
    }

    private void createDarkModeToolBarAction() {
        if (darkModeActionPlacement != MainWindowDarkModeActionPlacement.TOOL_BAR) {
            return;
        }

        commonToolBar.add(Box.createHorizontalGlue());
        commonToolBar.add(toggleDarkModeAction);
    }

    private void createDarkModeMenuAction() {
        if (darkModeActionPlacement != MainWindowDarkModeActionPlacement.MENU_BAR) {
            return;
        }

        var actionButton = new FlatButton();
        actionButton.setButtonType(FlatButton.ButtonType.toolBarButton);
        actionButton.setFocusable(false);
        actionButton.setAction(toggleDarkModeAction);
        actionButton.setSquareSize(true);
        jMenuBar.add(Box.createGlue());
        jMenuBar.add(actionButton);
    }

    private void setToolBarProperties() {
        toolbarInstaller.configure(commonToolBar);
    }

    private void installToolBar() {
        toolbarInstaller.install(getContentPane(), tabbedPane, commonToolBar);
    }

    private void createCommonToolBar() {
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
                this::createDarkModeToolBarAction,
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
    private void handleNotificationCenterChangeEvent(NotificationCenterChangeEvent e) {
        setupNotificationCenter();
    }

    private void closeNotificationCenter() {
        NotificationService.INSTANCE.close();
    }

    /**
     * This shutdown hook will try to save both log messages and write config changes to disk before app terminates.
     */
    private void setupShutdownHook() {
        Runtime.getRuntime().addShutdownHook(new Log4jShutdownHookThread());
    }

    private void setupSystemTray() {
        platformIntegration.setupSystemTrayLater();
    }

    private void setIconAndWindowImage() {
        setWindowTitle();
        setIconImage(GetIcon.getIcon(ICON_NAME, ICON_PATH, ICON_WIDTH, ICON_HEIGHT).getImage());
    }

    private void configureMenuKeyboardShortcuts() {
        if (disableF10MenuShortcut) {
            var im = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F10, 0), DISABLED_ACTION_KEY);
        }
    }

    private void createMenuBar() {
        setJMenuBar(createMenuBuilder().createMenuBar());
        createDarkModeMenuAction();
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
                menuPolicy,
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

    public @NonNull StatusBarProgressHandle showStatusBarProgress() {
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

    @Handler
    private void handleUpdateStateChanged(UpdateStateChangedEvent e) {
        SwingUtilities.invokeLater(() -> programUpdateCoordinator.update(e.isActive()));
    }

    private void closeProgramUpdateCoordinator() {
        programUpdateCoordinator.close();
    }

    private JPanel createTabFilme(@NonNull Daten daten) {
        return new GuiFilme(
                daten,
                this,
                toggleBlacklistAction,
                editBlacklistAction,
                showFilmInformationAction,
                showLuceneTutorialAction
        );
    }

    private JPanel createTabDownloads(@NonNull Daten daten) {
        return new GuiDownloads(daten, this, showFilmInformationAction);
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
    private void configureTabPlacement() {
        tabPlacementController.configureTabPlacement(getContentPane(), tabbedPane, commonToolBar);
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
    private void handleDownloadStart(DownloadStartEvent msg) {
        downloadProgressIndicator.downloadStarted();
    }

    /**
     * Message bus handler which gets called when a download is stopped.
     *
     * @param msg Information about the download
     */
    @Handler
    private void handleDownloadFinishedEvent(DownloadFinishedEvent msg) {
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
    private void installMenuTabSwitchListener() {
        menuTabSwitchController.initialize();
    }

    /**
     * Handle the install/or remove event sent from settings dialog
     */
    @Handler
    private void handleInstallTabSwitchListenerEvent(InstallTabSwitchListenerEvent msg) {
        menuTabSwitchController.handleInstallTabSwitchListenerEvent(msg);
    }

    @Handler
    private void handleFilmlistWriteStartEvent(FilmListWriteStartEvent e) {
        SwingUtilities.invokeLater(() -> loadFilmListAction.setEnabled(false));
    }

    @Handler
    private void handleFilmlistWriteStopEvent(FilmListWriteStopEvent e) {
        SwingUtilities.invokeLater(() -> loadFilmListAction.setEnabled(true));
    }

    private void initMenus() {
        installMenuTabSwitchListener();
        createMenuBuilder().initializeMenus();
        afterMenusInitialized.accept(this);
    }

    public void performFilmListLoadOperation(boolean manualMode) {
        filmlistLoadCoordinator.performFilmListLoadOperation(manualMode);
    }

    private DialogEinstellungen getSettingsDialog() {
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
                filmlistReloadCoordinator::close,
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
