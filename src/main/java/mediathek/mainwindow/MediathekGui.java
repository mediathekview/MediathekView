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
import mediathek.gui.MVTray;
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
import mediathek.swing.IconOnlyButton;
import mediathek.swing.SwingDispatch;
import mediathek.tool.*;
import mediathek.tool.notification.GenericNotificationCenter;
import mediathek.tool.notification.INotificationCenter;
import mediathek.tool.notification.NotificationService;
import mediathek.tool.timer.TimerPool;
import mediathek.update.AutomaticFilmlistUpdate;
import mediathek.update.ProgramUpdateCheck;
import mediathek.update.ProgramUpdateHost;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;
import raven.toast.Notifications;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
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

public class MediathekGui extends JFrame implements FilmBookmarkHost, DownloadControlHost, ProgramUpdateHost, TrayHost, SettingsDialogHost, FilmListLoadHost {

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
    /**
     * "Pointer" to UI
     */
    private static MediathekGui ui;
    private final AtomicBoolean applicationQuitInProgress = new AtomicBoolean();
    private final AtomicBoolean disposed = new AtomicBoolean();
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
    private final JLabel progressLabel = new JLabel();
    /**
     * Used for status bar progress.
     */
    private final JProgressBar progressBar = new JProgressBar();
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
    private final ShowLuceneTutorialAction showLuceneTutorialAction = new ShowLuceneTutorialAction(this);
    private final LivestreamPanel tabLivestreams = new LivestreamPanel();
    private final ToggleZappLivestreamsTabAction toggleZappLivestreamsTabAction = new ToggleZappLivestreamsTabAction(tabbedPane, tabLivestreams);
    private final OnlineSearchPanel tabOnlineSearch = new OnlineSearchPanel(this);
    private final ToggleOnlineSearchTabAction toggleOnlineSearchTabAction = new ToggleOnlineSearchTabAction(tabbedPane, tabOnlineSearch);
    private final AudioRepository audiothekRepository = new AudioRepository();
    private final AudiothekPanel tabAudiothek = new AudiothekPanel(audiothekRepository);
    private final ToggleAudiothekTabAction toggleAudiothekTabAction = new ToggleAudiothekTabAction(tabbedPane, tabAudiothek);
    private final LogDialog logDialog = new LogDialog(this);
    private final Supplier<INotificationCenter> notificationCenterFactory;
    private final ComputerShutdown computerShutdown;
    private final DownloadProgressIndicator downloadProgressIndicator;
    private final MainWindowController mainWindowController;
    private final FilmlistProgressPresenter filmlistDownloadProgressListener =
            new FilmlistProgressPresenter(SwingDispatch.INSTANCE, this::showStatusBarProgress);
    private final ListenerFilmeLaden filmListListener;
    private FixedRedrawStatusBar swingStatusBar;
    private GuiFilme tabFilme;
    private GuiDownloads tabDownloads;
    private FilmInfoDialog filmInfo;
    private MVTray tray;
    private DialogEinstellungen dialogEinstellungen;
    private ProgramUpdateCheck programUpdateChecker;
    private AutomaticFilmlistUpdate automaticFilmlistUpdate;
    private StartupFilmlistLoader startupFilmlistLoader;
    private boolean resetSettingsOnQuit;
    private boolean menuTabSwitchListenersInstalled;
    private boolean messageBusSubscribed;

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
        filmListListener = new MainWindowFilmListListener(
                SwingDispatch.INSTANCE,
                () -> loadFilmListAction,
                () -> daten.allesSpeichern(),
                this::setupAutomaticFilmlistReload
        );
        searchProgramUpdateAction = new SearchProgramUpdateAction(this);
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
        ui = this;

        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

        setupScrollBarWidth();
        UIManager.put("TabbedPane.showTabSeparators", true);
        UIManager.addPropertyChangeListener(lookAndFeelListener);

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
        mapFilmUrlCopyCommands();

        SplashScreenLifecycle.update(UIProgressState.FINISHED);
    }

    private void startMainWindowRuntime() {
        daten.getDownloadStartCoordinator().setDialogOwner(this);
        subscribeTableModelChangeEvent();
        setupTaskbarMenuLater();
        setupSystemTray();
        setApplicationWindowSizeLater();
        loadFilmlist();
        setupAutomaticUpdateCheck();
        setupShutdownHook();
        checkInvalidRegularExpressions();
        loadBandwidthMonitor();
        setupFilmInfoDialog();
        resetTabPlacement();
        setupRavenNotifications();
        performGeoCountryStartupCheck();
    }

    private void setupTaskbarMenuLater() {
        SwingUtilities.invokeLater(() -> {
            if (Taskbar.isTaskbarSupported())
                setupTaskbarMenu();
        });
    }

    private void setApplicationWindowSizeLater() {
        SwingUtilities.invokeLater(this::setApplicationWindowSize);
    }

    private void setupAutomaticUpdateCheck() {
        setupUpdateCheck(ApplicationConfiguration.getInstance().getAutomaticUpdateCheck());
    }

    private void setupRavenNotifications() {
        Notifications.getInstance().setJFrame(this);
    }

    @Override
    public void dispose() {
        if (disposed.compareAndSet(false, true)) {
            unsubscribeFromMessageBus();
            daten.getDownloadStartCoordinator().setDialogOwner(null);
            removeFilmListListeners();
            closeStartupFilmlistLoader();
            closeFilmlistDownloadProgress();
            closeAutomaticFilmlistUpdate();
            endProgramUpdateChecker();
            closeSystemTray();
            closeNotificationCenter();
            downloadProgressIndicator.close();
            UIManager.removePropertyChangeListener(lookAndFeelListener);
            if (ui == this) {
                ui = null;
            }
        }
        super.dispose();
    }

    private void handleLookAndFeelChange(PropertyChangeEvent evt) {
        if (evt.getPropertyName().equalsIgnoreCase("lookAndFeel")) {
            SwingUtilities.updateComponentTreeUI(progressLabel);
            SwingUtilities.updateComponentTreeUI(progressBar);
        }
    }

    /**
     * Return the user interface instance
     *
     * @return the class instance or null.
     */
    public static MediathekGui ui() {
        return ui;
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
        logger.trace("Loading info dialog");
        filmInfo = new FilmInfoDialog(this);
        logger.trace("Finished loading info dialog");
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

    private void loadBandwidthMonitor() {
        logger.trace("Loading bandwidth monitor");
        if (ApplicationConfiguration.getInstance().getBandwidthMonitorVisible()) {
            showBandwidthUsageAction.actionPerformed(null);
        }
        logger.trace("Finished loading bandwidth monitor");
    }

    private void subscribeTableModelChangeEvent() {
        var messageBus = MessageBus.getMessageBus();
        //send before subscribing
        messageBus.publishAsync(new TableModelChangeEvent(true, false));
        messageBus.subscribe(this);
        messageBusSubscribed = true;
    }

    private void unsubscribeFromMessageBus() {
        if (!messageBusSubscribed) {
            return;
        }

        MessageBus.getMessageBus().unsubscribe(this);
        messageBusSubscribed = false;
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

    protected void createToggleBlacklistButton() {
        boolean useIconWithText = ApplicationConfiguration.getInstance().getToolbarBlacklistIconWithText();
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
        SwingUtilities.invokeLater(() -> {
            initializeSystemTray();

            addWindowListener(new WindowAdapter() {
                @Override
                public void windowClosing(WindowEvent evt) {
                    if (tray != null && ApplicationConfiguration.getInstance().getUseTray()) {
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

    private void createMemoryMonitor() {
        if (ApplicationConfiguration.getInstance().getMemoryMonitorDialogVisible()) {
            showMemoryMonitorAction.showMemoryMonitor();
        }
    }

    /**
     * Read a local filmlist or load a new one in auto mode.
     */
    private void loadFilmlist() {
        installStatusBarProgressOnEdt(progressLabel, progressBar);
        startupFilmlistLoader = new StartupFilmlistLoader(daten, progressLabel, progressBar, this::finishStartupFilmlistLoad);
        startupFilmlistLoader.start();
    }

    private void finishStartupFilmlistLoad(boolean remoteUpdateStarted, boolean failed) {
        try {
            if (!remoteUpdateStarted) {
                Daten.getInstance().getFilmeLaden().notifyFertig(new ListenerFilmeLadenEvent("", "", 100, 100, failed));
            }
        } finally {
            uninstallStatusBarProgressOnEdt(progressLabel, progressBar);
        }
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
        daten.getFilmeLaden().addAdListener(filmlistDownloadProgressListener);
    }

    @Handler
    private void handleFilmListReadStopEvent(FilmListReadStopEvent event) {
        SwingUtilities.invokeLater(this::closeFilmlistDownloadProgress);
    }

    private void closeFilmlistDownloadProgress() {
        filmlistDownloadProgressListener.close();
    }

    private StatusBarProgressHandle showStatusBarProgress(JLabel label, JProgressBar progressBar) {
        installStatusBarProgressOnEdt(label, progressBar);
        return new StatusBarProgressRegistration(label, progressBar);
    }

    public StatusBarProgressHandle showStatusBarProgress() {
        return showStatusBarProgress(new JLabel(), new JProgressBar());
    }

    private void installStatusBarProgressOnEdt(JLabel label, JProgressBar progressBar) {
        runOnEventDispatchThreadAndWait("Install status bar progress", () -> installStatusBarProgress(label, progressBar));
    }

    private void installStatusBarProgress(JLabel label, JProgressBar progressBar) {
        if (label.getParent() != swingStatusBar) {
            swingStatusBar.add(label);
        }
        if (progressBar.getParent() != swingStatusBar) {
            swingStatusBar.add(progressBar);
        }
        refreshStatusBar();
    }

    private void uninstallStatusBarProgressOnEdt(JLabel label, JProgressBar progressBar) {
        runOnEventDispatchThreadAndWait("Uninstall status bar progress", () -> uninstallStatusBarProgress(label, progressBar));
    }

    private void uninstallStatusBarProgress(JLabel label, JProgressBar progressBar) {
        if (progressBar.getParent() == swingStatusBar) {
            swingStatusBar.remove(progressBar);
        }
        if (label.getParent() == swingStatusBar) {
            swingStatusBar.remove(label);
        }
        refreshStatusBar();
    }

    private void refreshStatusBar() {
        swingStatusBar.revalidate();
        swingStatusBar.repaint();
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

    private final class StatusBarProgressRegistration implements StatusBarProgressHandle {
        private final JLabel label;
        private final JProgressBar progressBar;
        private final AtomicBoolean closed = new AtomicBoolean();

        private StatusBarProgressRegistration(JLabel label, JProgressBar progressBar) {
            this.label = label;
            this.progressBar = progressBar;
        }

        @Override
        public @NonNull JLabel label() {
            return label;
        }

        @Override
        public @NonNull JProgressBar progressBar() {
            return progressBar;
        }

        @Override
        public void close() {
            if (!closed.compareAndSet(false, true)) {
                return;
            }
            uninstallStatusBarProgressOnEdt(label, progressBar);
        }
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

    private void setupFilmListListener() {
        daten.getFilmeLaden().setUiHost(this);
        daten.getFilmeLaden().addAdListener(filmListListener);
    }

    private void removeFilmListListeners() {
        daten.getFilmeLaden().setUiHost(null);
        daten.getFilmeLaden().removeAdListener(filmlistDownloadProgressListener);
        daten.getFilmeLaden().removeAdListener(filmListListener);
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
        SwingUtilities.invokeLater(() -> setupUpdateCheck(e.isActive()));
    }

    /**
     * This creates a repeating update check every 24 hours.
     */
    private void setupUpdateCheck(boolean newState) {
        if (newState) {
            endProgramUpdateChecker();
            programUpdateChecker = new ProgramUpdateCheck(this);
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
        final var useTray = ApplicationConfiguration.getInstance().getUseTray();
        if (tray == null && useTray) {
            tray = new MVTray(this).systemTray();
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
        if (dialogEinstellungen == null) {
            dialogEinstellungen = new DialogEinstellungen(this);
        }

        return dialogEinstellungen;
    }

    public void requestSettingsResetOnQuit() {
        resetSettingsOnQuit = true;
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
                showMemoryMonitorAction,
                showBandwidthUsageAction,
                manageAboAction,
                tabRegistry,
                computerShutdown,
                resetSettingsOnQuit,
                this::closeAutomaticFilmlistUpdate,
                this::endProgramUpdateChecker,
                this::closeSystemTray,
                this::closeNotificationCenter,
                this::shutdownTimerPool,
                this::waitForCommonPoolToComplete,
                this::runOnEventDispatchThreadAndWait
        );
    }

    private void closeSystemTray() {
        if (tray != null) {
            tray.beenden();
            tray = null;
        }
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
