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

package mediathek.mainwindow

import com.formdev.flatlaf.extras.components.FlatButton
import mediathek.SplashScreenLifecycle
import mediathek.audiothek.repository.AudioRepository
import mediathek.audiothek.ui.main.AudiothekPanel
import mediathek.config.CommandLineOptions
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.MVColor
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.gui.actions.*
import mediathek.gui.bookmark.BookmarkDialog
import mediathek.gui.dialogEinstellungen.DialogEinstellungen
import mediathek.gui.filmInformation.FilmInfoDialog
import mediathek.gui.messages.*
import mediathek.gui.progress.DownloadProgressIndicator
import mediathek.gui.progress.NoDownloadProgressIndicator
import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.gui.tabs.tab_film.GuiFilme
import mediathek.gui.tabs.tab_livestreams.LivestreamPanel
import mediathek.gui.tabs.tab_online_search.OnlineSearchHost
import mediathek.gui.tabs.tab_online_search.OnlineSearchPanel
import mediathek.logging.LogDialog
import mediathek.shutdown.ComputerShutdown
import mediathek.swing.SwingDispatch
import mediathek.tool.*
import mediathek.tool.notification.INotificationCenter
import mediathek.tool.notification.NotificationService
import mediathek.tool.timer.TimerPool
import mediathek.update.ProgramUpdateHost
import net.engio.mbassy.listener.Handler
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.awt.BorderLayout
import java.awt.Container
import java.awt.event.KeyEvent
import java.beans.PropertyChangeEvent
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.function.Consumer
import java.util.function.Function
import java.util.function.IntConsumer
import java.util.function.Supplier
import javax.swing.*

open class MediathekGui private constructor(
    notificationCenterFactory: Supplier<INotificationCenter>,
    computerShutdown: ComputerShutdown,
    downloadProgressIndicatorFactory: Function<JFrame, DownloadProgressIndicator>,
    darkModeActionPlacement: MainWindowDarkModeActionPlacement,
    toolbarInstaller: MainWindowToolbarInstaller,
    tabPlacementController: MainWindowTabPlacementController,
    menuPolicy: MainWindowMenuPolicy,
    automaticMenuTabSwitchingSupported: Boolean,
    scrollBarConfigurator: MainWindowScrollBarConfigurator,
    systemTrayController: MainWindowSystemTrayController,
    private val disableF10MenuShortcut: Boolean,
    private val afterMenusInitialized: Consumer<MainWindowQuitHost>,
) : JFrame(),
    FilmBookmarkHost,
    DownloadControlHost,
    ProgramUpdateHost,
    TrayHost,
    SettingsDialogHost,
    SettingsResetHost,
    FilmListLoadHost {
    private val disposed = AtomicBoolean()
    private val editBlacklistAction = EditBlacklistAction(this)
    private val toggleBlacklistAction = ToggleBlacklistAction()
    private val selectedListItemsProperty = ListSelectedItemsProperty(0)
    private val daten = Daten.getInstance()
    private val tabbedPane = PositionSavingTabbedPane()
    private val jMenuHilfe = JMenu()
    private val settingsAction = SettingsAction()
    private val commonToolBar = JToolBar()
    private val manageBookmarkAction = ManageBookmarkAction(this)
    private val toggleDarkModeAction = ToggleDarkModeAction(this)
    private val fontMenu = JMenu("Schrift")
    private val jMenuDatei = JMenu()
    private val jMenuFilme = JMenu()
    private val jMenuBar = JMenuBar()
    private val jMenuDownload = JMenu()
    private val jMenuAbos = JMenu()
    private val jMenuAnsicht = JMenu()
    private val tabRegistry = MainWindowTabRegistry(tabbedPane)
    private var mainTabs: MainWindowTabs? = null
    private val menuTabSwitchController: MainWindowMenuTabSwitchController = MainWindowMenuTabSwitchController(
        tabbedPane,
        jMenuFilme,
        jMenuDownload,
        Supplier { tabs().films },
        Supplier { tabs().downloads },
        automaticMenuTabSwitchingSupported,
    )
    private val loadFilmListAction: LoadFilmListAction
    private val showFilmInformationAction: ShowFilmInformationAction
    private val searchProgramUpdateAction: SearchProgramUpdateAction
    private val showMemoryMonitorAction = MemoryMonitorAction(this)
    private val manageAboAction = ManageAboAction(this)
    private val showBandwidthUsageAction = ShowBandwidthUsageAction(this)
    private val dialogCoordinator =
        MainWindowDialogCoordinator(this, this, showMemoryMonitorAction, showBandwidthUsageAction, manageAboAction)
    private val showLuceneTutorialAction = ShowLuceneTutorialAction(this)
    private val onlineSearchTab: MainWindowTab by lazy(LazyThreadSafetyMode.NONE) {
        MainWindowTab(
            "Onlinesuche",
            { OnlineSearchPanel(createOnlineSearchHost()) },
            visible = { ApplicationConfiguration.getInstance().onlineSearchTabVisible },
            toggleActionFactory = { toggleOnlineSearchTabAction },
            onComponentCreated = { configureClosableOptionalTab(it, toggleOnlineSearchTabAction) },
            initialComponentFactory = { createDeferredTabPlaceholder("Onlinesuche") },
        )
    }
    private val toggleOnlineSearchTabAction: ToggleOnlineSearchTabAction by lazy(LazyThreadSafetyMode.NONE) {
        ToggleOnlineSearchTabAction(tabbedPane, onlineSearchTab)
    }
    private val zappLivestreamsTab: MainWindowTab by lazy(LazyThreadSafetyMode.NONE) {
        MainWindowTab(
            "zapp Livestreams",
            { LivestreamPanel(this) },
            visible = { ApplicationConfiguration.getInstance().zappLivestreamsTabVisible },
            toggleActionFactory = { toggleZappLivestreamsTabAction },
            onComponentCreated = { configureClosableOptionalTab(it, toggleZappLivestreamsTabAction) },
        )
    }
    private val toggleZappLivestreamsTabAction: ToggleZappLivestreamsTabAction by lazy(LazyThreadSafetyMode.NONE) {
        ToggleZappLivestreamsTabAction(tabbedPane, zappLivestreamsTab)
    }
    private val audiothekTab: MainWindowTab by lazy(LazyThreadSafetyMode.NONE) {
        MainWindowTab(
            "Audiothek",
            { AudiothekPanel(AudioRepository(), this) },
            visible = { ApplicationConfiguration.getInstance().audiothekTabVisible },
            toggleActionFactory = { toggleAudiothekTabAction },
            onComponentCreated = { configureClosableOptionalTab(it, toggleAudiothekTabAction) },
            onComponentSelected = { (it as AudiothekPanel).loadIfNecessary() },
            initialComponentFactory = { createDeferredTabPlaceholder("Audiothek") },
            dispose = { (it as AudiothekPanel).disposePanel() },
        )
    }
    private val toggleAudiothekTabAction: ToggleAudiothekTabAction by lazy(LazyThreadSafetyMode.NONE) {
        ToggleAudiothekTabAction(tabbedPane, audiothekTab)
    }
    private val logDialog = LogDialog(this)
    private val notificationCenterFactory = notificationCenterFactory
    private val darkModeActionPlacement = darkModeActionPlacement
    private val toolbarInstaller = toolbarInstaller
    private val tabPlacementController = tabPlacementController
    private val menuPolicy = menuPolicy
    private val menuBuilder by lazy(LazyThreadSafetyMode.NONE) { createMenuBuilder() }
    private val scrollBarConfigurator = scrollBarConfigurator
    private val downloadProgressIndicator: DownloadProgressIndicator = requireNotNull(downloadProgressIndicatorFactory.apply(this))
    private val startupOrchestrator: MainWindowStartupOrchestrator
    private val platformIntegration: MainWindowPlatformIntegration
    private val programUpdateCoordinator = MainWindowProgramUpdateCoordinator(this)
    private val shutdownRuntime = MainWindowShutdownRuntime()
    private val statusBarController =
        MainWindowStatusBarController(
            contentPane,
            selectedListItemsProperty,
            ::getFilmTableRowCount,
            ::runOnEventDispatchThreadAndWait,
        )
    private val filmlistLoadCoordinator = MainWindowFilmlistLoadCoordinator(this, daten, statusBarController)
    private val filmlistDownloadProgressListener =
        FilmlistProgressPresenter(SwingDispatch, statusBarController::showProgress)
    private val filmlistReloadCoordinator: MainWindowFilmlistReloadCoordinator
    private val quitController = MainWindowQuitController(
        this,
        daten,
        this,
        dialogCoordinator,
        tabRegistry,
        computerShutdown,
        ::activeAudiothekDownloadCount,
        ::pauseAudiothekDownloadsForShutdown,
        { filmlistReloadCoordinator.close() },
        { closeProgramUpdateCoordinator() },
        { closeSystemTray() },
        { closeNotificationCenter() },
        shutdownRuntime::shutdownTimerPool,
        shutdownRuntime::waitForCommonPoolToComplete,
        ::runOnEventDispatchThreadAndWait,
    )
    private val mainWindowLifecycle: MainWindowLifecycle

    protected constructor(
        notificationCenterFactory: Supplier<INotificationCenter>,
        computerShutdown: ComputerShutdown,
        darkModeActionPlacement: MainWindowDarkModeActionPlacement,
        systemTrayController: MainWindowSystemTrayController,
    ) : this(
        notificationCenterFactory,
        computerShutdown,
        NO_DOWNLOAD_PROGRESS_INDICATOR_FACTORY,
        darkModeActionPlacement,
        DEFAULT_TOOLBAR_INSTALLER,
        MainWindowTabPlacementController(true),
        DefaultMainWindowMenuPolicy,
        true,
        DefaultMainWindowScrollBarConfigurator,
        systemTrayController,
        true,
        Consumer {},
    )

    protected constructor(
        notificationCenterFactory: Supplier<INotificationCenter>,
        computerShutdown: ComputerShutdown,
        downloadProgressIndicatorFactory: Function<JFrame, DownloadProgressIndicator>,
        toolbarInstaller: MainWindowToolbarInstaller,
        tabPlacementController: MainWindowTabPlacementController,
        menuPolicy: MainWindowMenuPolicy,
        automaticMenuTabSwitchingSupported: Boolean,
        scrollBarConfigurator: MainWindowScrollBarConfigurator,
        systemTrayController: MainWindowSystemTrayController,
        disableF10MenuShortcut: Boolean,
        afterMenusInitialized: Consumer<MainWindowQuitHost>,
    ) : this(
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
        afterMenusInitialized,
    )

    protected constructor(
        notificationCenterFactory: Supplier<INotificationCenter>,
        computerShutdown: ComputerShutdown,
        downloadProgressIndicatorFactory: Function<JFrame, DownloadProgressIndicator>,
        darkModeActionPlacement: MainWindowDarkModeActionPlacement,
    ) : this(
        notificationCenterFactory,
        computerShutdown,
        downloadProgressIndicatorFactory,
        darkModeActionPlacement,
        DEFAULT_TOOLBAR_INSTALLER,
        MainWindowTabPlacementController(true),
        DefaultMainWindowMenuPolicy,
        true,
        DefaultMainWindowScrollBarConfigurator,
        DefaultMainWindowSystemTrayController,
        true,
        Consumer {},
    )

    init {
        loadFilmListAction = LoadFilmListAction { filmlistLoadCoordinator.performFilmListLoadOperation(false) }
        showFilmInformationAction = ShowFilmInformationAction(::getFilmInfoDialog)
        filmlistReloadCoordinator = MainWindowFilmlistReloadCoordinator(
            daten,
            loadFilmListAction,
        ) { filmlistLoadCoordinator.performFilmListLoadOperation(false) }
        val filmListListener: ListenerFilmeLaden = MainWindowFilmListListener(
            SwingDispatch,
            { loadFilmListAction },
            { daten.allesSpeichern() },
            { filmlistReloadCoordinator.setupAutomaticFilmlistReload() }
        )
        mainWindowLifecycle = MainWindowLifecycle(
            this,
            daten,
            this,
            ::handleLookAndFeelChange,
            filmlistDownloadProgressListener,
            filmListListener,
            this
        ) { getCurrentZeitraumFilterValue() }
        searchProgramUpdateAction = SearchProgramUpdateAction(this)
        platformIntegration = MainWindowPlatformIntegration(
            this,
            this,
            loadFilmListAction,
            { setupSystemTray() },
            systemTrayController
        )
        startupOrchestrator = createStartupOrchestrator()
    }

    private fun createStartupOrchestrator(): MainWindowStartupOrchestrator =
        MainWindowStartupOrchestrator(
            { initializeMainWindow() },
            { startMainWindowRuntime() }
        )

    fun start() {
        startupOrchestrator.start()
    }

    private fun initializeMainWindow() {
        defaultCloseOperation = DO_NOTHING_ON_CLOSE

        setupScrollBarWidth()
        UIManager.put("TabbedPane.showTabSeparators", true)
        mainWindowLifecycle.registerLookAndFeelListener()

        setupAlternatingRowColors()

        SplashScreenLifecycle.update(UIProgressState.LOAD_MAINWINDOW)

        contentPane.layout = BorderLayout()

        setIconAndWindowImage()

        createMenuBar()

        configureMenuKeyboardShortcuts()

        SplashScreenLifecycle.update(UIProgressState.CREATE_STATUS_BAR)
        createStatusBar()

        SplashScreenLifecycle.update(UIProgressState.SETUP_FILM_LISTENERS)
        mainWindowLifecycle.registerFilmListListeners()

        SplashScreenLifecycle.update(UIProgressState.LOAD_TABS)
        initTabs()

        SplashScreenLifecycle.update(UIProgressState.INIT_MENUS)
        initMenus()

        setupNotificationCenter()

        createCommonToolBar()
        installToolBar()
        mapFilmUrlCopyCommands()

        SplashScreenLifecycle.update(UIProgressState.FINISHED)
    }

    private fun startMainWindowRuntime() {
        mainWindowLifecycle.start()
        platformIntegration.setupTaskbarMenuLater()
        platformIntegration.setupSystemTray()
        setApplicationWindowSizeLater()
        filmlistLoadCoordinator.loadStartupFilmlist()
        setupAutomaticUpdateCheck()
        setupShutdownHook()
        checkInvalidRegularExpressions()
        setupFilmInfoDialog()
        resetTabPlacement()
        platformIntegration.setupRavenNotifications()
        performGeoCountryStartupCheck()
    }


    private fun setApplicationWindowSizeLater() {
        runOnEventDispatchThread(::setApplicationWindowSize)
    }

    private fun setupAutomaticUpdateCheck() {
        programUpdateCoordinator.startFromConfiguration()
    }

    override fun dispose() {
        if (disposed.compareAndSet(false, true)) {
            mainWindowLifecycle.close()
            filmlistLoadCoordinator.close()
            closeFilmlistDownloadProgress()
            filmlistReloadCoordinator.close()
            closeProgramUpdateCoordinator()
            closeSystemTray()
            closeNotificationCenter()
            downloadProgressIndicator.close()
        }
        super.dispose()
    }

    private fun handleLookAndFeelChange(event: PropertyChangeEvent) {
        if (event.propertyName.equals("lookAndFeel", ignoreCase = true)) {
            runOnEventDispatchThread { statusBarController.updateComponentTreeUi() }
        }
    }

    override fun ownerFrame(): JFrame = this

    override fun showMainWindow() {
        runOnEventDispatchThread { isVisible = true }
    }

    override fun toggleMainWindowVisibility() {
        runOnEventDispatchThread {
            isVisible = !isVisible
            if (isVisible) {
                toFront()
                requestFocusInWindow()
            }
        }
    }

    override fun refreshSystemTray() {
        runOnEventDispatchThread { platformIntegration.initializeSystemTray() }
    }

    override fun repaintMainWindow() {
        runOnEventDispatchThread { repaint() }
    }

    private fun getFilmTableRowCount(): Int = tabs().films.tableRowCount

    private fun getCurrentZeitraumFilterValue(): String = tabs().films.currentZeitraumFilterValue

    override val bookmarkDialog: BookmarkDialog?
        get() = tabs().films.getBookmarkDialog()

    override fun showManageBookmarkWindow() {
        runOnEventDispatchThread { tabs().films.showManageBookmarkWindow() }
    }

    override fun resetFilterDialogPosition() {
        runOnEventDispatchThread { tabs().films.resetFilterDialogPosition() }
    }

    override fun repaintFilmTab() {
        runOnEventDispatchThread { tabs().films.repaint() }
    }

    override fun stopAllWaitingDownloads() {
        runOnEventDispatchThread { tabs().downloads.stopAllWaitingDownloads() }
    }

    private fun setSelectedListItemsCount(count: Long) {
        runOnEventDispatchThread { selectedListItemsProperty.setSelectedItems(count) }
    }

    private fun setupFilmInfoDialog() {
        dialogCoordinator.setupFilmInfoDialog()
    }

    override fun supportsAutomaticMenuTabSwitching(): Boolean =
        menuTabSwitchController.supportsAutomaticSwitching()

    private fun resetTabPlacement() {
        tabPlacementController.resetTabPlacement()
    }

    private fun performAustrianVlcCheck() {
        AustrianVlcCheck(this).perform()
    }

    private fun performGeoCountryStartupCheck() {
        GeoCountryStartupCheck(this, { performAustrianVlcCheck() }).perform()
    }

    private fun mapFilmUrlCopyCommands() {
        val inputMap = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
        inputMap.put(
            KeyStroke.getKeyStroke(
                KeyEvent.VK_H,
                GuiFunktionen.getPlatformControlKey() or KeyEvent.SHIFT_DOWN_MASK or KeyEvent.ALT_DOWN_MASK,
            ),
            ACTION_MAP_KEY_COPY_HQ_URL,
        )
        inputMap.put(
            KeyStroke.getKeyStroke(
                KeyEvent.VK_N,
                GuiFunktionen.getPlatformControlKey() or KeyEvent.SHIFT_DOWN_MASK or KeyEvent.ALT_DOWN_MASK,
            ),
            ACTION_MAP_KEY_COPY_NORMAL_URL,
        )

        val actionMap = jMenuBar.actionMap
        actionMap.put(ACTION_MAP_KEY_COPY_HQ_URL, tabs().films.copyHqUrlToClipboardAction())
        actionMap.put(ACTION_MAP_KEY_COPY_NORMAL_URL, tabs().films.copyNormalUrlToClipboardAction())
    }

    private fun setupScrollBarWidth() {
        scrollBarConfigurator.configure()
    }

    private fun useAlternateRowColors(): Boolean = true

    override fun setupAlternatingRowColors() {
        if (useAlternateRowColors()) {
            UIManager.put("Table.alternateRowColor", MVColor.getAlternatingRowColor())
        }
    }

    private fun createDarkModeToolBarAction() {
        if (darkModeActionPlacement != MainWindowDarkModeActionPlacement.TOOL_BAR) {
            return
        }

        commonToolBar.add(Box.createHorizontalGlue())
        commonToolBar.add(toggleDarkModeAction)
    }

    private fun createDarkModeMenuAction() {
        if (darkModeActionPlacement != MainWindowDarkModeActionPlacement.MENU_BAR) {
            return
        }

        val actionButton = FlatButton()
        actionButton.buttonType = FlatButton.ButtonType.toolBarButton
        actionButton.isFocusable = false
        actionButton.action = toggleDarkModeAction
        actionButton.isSquareSize = true
        jMenuBar.add(Box.createGlue())
        jMenuBar.add(actionButton)
    }

    private fun setToolBarProperties() {
        toolbarInstaller.configure(commonToolBar)
    }

    private fun installToolBar() {
        toolbarInstaller.install(contentPane, tabbedPane, commonToolBar)
    }

    private fun createCommonToolBar() {
        createToolbarBuilder().createCommonToolBar()
    }

    private fun createToolbarBuilder(): MainWindowToolbarBuilder =
        MainWindowToolbarBuilder(
            commonToolBar,
            loadFilmListAction,
            showFilmInformationAction,
            toggleBlacklistAction,
            editBlacklistAction,
            manageAboAction,
            settingsAction,
            { createDarkModeToolBarAction() },
            { setToolBarProperties() },
        )

    private fun checkInvalidRegularExpressions() {
        TimerPool.schedule(
            {
                val invalidExpressions = Filter.drainRegExpErrors()
                if (invalidExpressions.isNotEmpty()) {
                    val regexString = invalidExpressions
                        .asSequence()
                        .sorted()
                        .joinToString("<br/>") { HtmlUtils.escapeHtml(it) }

                    val message = String.format(
                        "<html>Während des Starts wurden ungültige reguläre Ausdrücke (RegExp) in Ihrer Blacklist und/oder Abos entdeckt.<br/>" +
                            "<b>Sie müssen diese korrigieren, ansonsten funktioniert das Programm nicht fehlerfrei!</b><br/><br/>" +
                            "Nachfolgende Ausdrücke sind fehlerbehaftet: <br/>%s</html>",
                        regexString,
                    )

                    MVMessageDialog.showMessageDialog(
                        this,
                        message,
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.ERROR_MESSAGE,
                    )
                }
            },
            15,
            TimeUnit.SECONDS,
        )
    }

    private fun setupNotificationCenter() {
        val showNotifications = ApplicationConfiguration.getInstance().showNotifications
        NotificationService.configure(notificationCenterFactory, showNotifications)
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleNotificationCenterChangeEvent(event: NotificationCenterChangeEvent) {
        setupNotificationCenter()
    }

    private fun closeNotificationCenter() {
        NotificationService.close()
    }

    private fun setupShutdownHook() {
        Runtime.getRuntime().addShutdownHook(Log4jShutdownHookThread())
    }

    private fun setupSystemTray() {
        platformIntegration.setupSystemTrayLater()
    }

    private fun setIconAndWindowImage() {
        setWindowTitle()
        iconImage = GetIcon.getIcon(ICON_NAME, ICON_PATH, ICON_WIDTH, ICON_HEIGHT).image
    }

    private fun configureMenuKeyboardShortcuts() {
        if (disableF10MenuShortcut) {
            val inputMap = jMenuBar.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
            inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_F10, 0), DISABLED_ACTION_KEY)
        }
    }

    private fun createMenuBar() {
        setJMenuBar(menuBuilder.createMenuBar())
        createDarkModeMenuAction()
    }

    private fun createMenuBuilder(): MainWindowMenuBuilder =
        MainWindowMenuBuilder(
            this,
            this,
            this,
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
            { tabs().films },
            { tabs().downloads },
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
        )

    private fun createStatusBar() {
        statusBarController.createStatusBar()
        mainWindowLifecycle.registerFilmlistProgressListener()
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleFilmListReadStopEvent(event: FilmListReadStopEvent) {
        runOnEventDispatchThread(::closeFilmlistDownloadProgress)
    }

    private fun closeFilmlistDownloadProgress() {
        filmlistDownloadProgressListener.close()
    }

    override fun showStatusBarProgress(): StatusBarProgressHandle =
        statusBarController.showProgress()

    override fun setFilmIndexingActionsEnabled(enabled: Boolean) {
        runOnEventDispatchThread {
            toggleBlacklistAction.isEnabled = enabled
            editBlacklistAction.isEnabled = enabled
            loadFilmListAction.isEnabled = enabled
        }
    }

    private fun runOnEventDispatchThread(action: () -> Unit) {
        SwingDispatch.dispatch(Runnable(action))
    }

    private fun runOnEventDispatchThreadAndWait(description: String, action: Runnable) {
        try {
            SwingDispatch.runAndWait(description, action)
        } catch (exception: IllegalStateException) {
            if (exception.cause is InterruptedException) {
                logger.error("{} interrupted", description, exception)
            } else {
                throw exception
            }
        }
    }

    private fun getFilmInfoDialog(): FilmInfoDialog =
        dialogCoordinator.getFilmInfoDialog()

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleTabVisualSettingsChangedEvent(event: TabVisualSettingsChangedEvent) {
        runOnEventDispatchThread {
            configureTabPlacement()
            configureTabIcons()
        }
    }

    private fun setWindowTitle() {
        title = Konstanten.PROGRAMMNAME + ' ' + Konstanten.MVVERSION
    }

    private fun restoreSizeFromConfig() {
        try {
            val bounds = ApplicationConfiguration.getInstance().getMainWindowBounds(MIN_WINDOW_WIDTH, MIN_WINDOW_HEIGHT)
            setBounds(bounds.x, bounds.y, bounds.width, bounds.height)
        } catch (_: NoSuchElementException) {
            extendedState = MAXIMIZED_BOTH
        }
    }

    private fun setApplicationWindowSize() {
        if (CommandLineOptions.isStartMaximized() || ApplicationConfiguration.getInstance().mainWindowMaximized) {
            extendedState = MAXIMIZED_BOTH
        } else {
            restoreSizeFromConfig()
        }

        runOnEventDispatchThread { addComponentListener(WindowLocationConfigSaverListener()) }
    }

    @Handler
    private fun handleUpdateStateChanged(event: UpdateStateChangedEvent) {
        runOnEventDispatchThread { programUpdateCoordinator.update(event.isActive) }
    }

    private fun closeProgramUpdateCoordinator() {
        programUpdateCoordinator.close()
    }

    private fun createOnlineSearchHost(): OnlineSearchHost =
        MainWindowOnlineSearchHost(
            ownerFrame(),
            { film: DatenFilm? -> dialogCoordinator.updateFilmInfoCurrentFilm(film) },
            { getFilmInfoDialog().showInfo() }
        )

    private fun createTabFilme(daten: Daten): GuiFilme =
        GuiFilme(
            daten,
            this,
            toggleBlacklistAction,
            editBlacklistAction,
            showFilmInformationAction,
            showLuceneTutorialAction,
            { setSelectedListItemsCount(it) },
            { film: DatenFilm? -> dialogCoordinator.updateFilmInfoCurrentFilm(film) }
        )

    private fun createTabDownloads(daten: Daten): GuiDownloads =
        GuiDownloads(
            daten,
            this,
            showFilmInformationAction,
            { setSelectedListItemsCount(it) },
            { film: DatenFilm? -> dialogCoordinator.updateFilmInfoCurrentFilm(film) },
            { shutdownComputer -> quitApplication(shutdownComputer) }
        )

    private fun tabs(): MainWindowTabs =
        checkNotNull(mainTabs) { "Main window tabs have not been initialized." }

    private data class MainWindowTabs(
        val films: GuiFilme,
        val downloads: GuiDownloads,
    )

    private fun initTabs() {
        val mainContentPane: Container = contentPane
        mainContentPane.add(tabbedPane, BorderLayout.CENTER)

        SplashScreenLifecycle.update(UIProgressState.LOAD_DOWNLOAD_TAB)
        val downloadsTab = createTabDownloads(daten)

        SplashScreenLifecycle.update(UIProgressState.LOAD_FILM_TAB)
        val filmsTab = createTabFilme(daten)
        mainTabs = MainWindowTabs(filmsTab, downloadsTab)

        SplashScreenLifecycle.update(UIProgressState.ADD_TABS_TO_UI)
        registerMainWindowTabs()
        tabRegistry.installVisibleTabs()

        if (ApplicationConfiguration.getInstance().restoreSelectedTab) {
            tabbedPane.restoreSavedTabPosition()
        }
        tabRegistry.materializeSelectedTab()
        tabRegistry.installSelectedTabMaterializer()
        tabbedPane.installChangeListener()

        SplashScreenLifecycle.update(UIProgressState.CONFIGURE_TABS)
        configureTabPlacement()
        configureTabIcons()
    }

    private fun registerMainWindowTabs() {
        tabRegistry.register(
            MainWindowTab(
                GuiFilme.NAME,
                { tabs().films },
                visible = { true },
                icon = { GetIcon.getProgramIcon("tab-film.png", 32, 32) },
                dispose = { tabs().films.disposePanel() },
            )
        )
        tabRegistry.register(
            MainWindowTab(
                GuiDownloads.NAME,
                { tabs().downloads },
                visible = { true },
                icon = { GetIcon.getProgramIcon("tab-download.png", 32, 32) },
                dispose = { tabs().downloads.tabelleSpeichern() },
            )
        )
        tabRegistry.register(onlineSearchTab)
        tabRegistry.register(zappLivestreamsTab)
        tabRegistry.register(audiothekTab)
    }

    private fun configureClosableOptionalTab(component: JComponent, toggleAction: Action) {
        component.putClientProperty("JTabbedPane.tabClosable", true)
        component.putClientProperty("JTabbedPane.tabCloseCallback", IntConsumer { toggleAction.actionPerformed(null) })
    }

    private fun createDeferredTabPlaceholder(tabTitle: String): JComponent =
        JPanel(BorderLayout()).apply {
            add(JLabel("$tabTitle wird beim ersten Öffnen geladen.", SwingConstants.CENTER), BorderLayout.CENTER)
        }

    override fun enableUpdateMenuItem(enable: Boolean) {
        runOnEventDispatchThread { searchProgramUpdateAction.isEnabled = enable }
    }

    private fun configureTabPlacement() {
        tabPlacementController.configureTabPlacement(contentPane, tabbedPane, commonToolBar)
    }

    private fun configureTabIcons() {
        tabRegistry.configureIcons(ApplicationConfiguration.getInstance().mainWindowTabIcons)
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleDownloadStart(event: DownloadStartEvent) {
        runOnEventDispatchThread { downloadProgressIndicator.downloadStarted() }
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleDownloadFinishedEvent(event: DownloadFinishedEvent) {
        runOnEventDispatchThread { downloadProgressIndicator.downloadFinished() }
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleShowSettingsDialogEvent(event: ShowSettingsDialogEvent) {
        runOnEventDispatchThread {
            settingsDialog.isVisible = true
            if (!SystemUtils.IS_OS_LINUX) {
                settingsDialog.toFront()
            }
        }
    }

    private fun installMenuTabSwitchListener() {
        menuTabSwitchController.initialize()
    }

    @Handler
    private fun handleInstallTabSwitchListenerEvent(event: InstallTabSwitchListenerEvent) {
        menuTabSwitchController.handleInstallTabSwitchListenerEvent(event)
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleFilmlistWriteStartEvent(event: FilmListWriteStartEvent) {
        runOnEventDispatchThread { loadFilmListAction.isEnabled = false }
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleFilmlistWriteStopEvent(event: FilmListWriteStopEvent) {
        runOnEventDispatchThread { loadFilmListAction.isEnabled = true }
    }

    private fun initMenus() {
        installMenuTabSwitchListener()
        menuBuilder.initializeMenus()
        afterMenusInitialized.accept(this)
    }

    private val settingsDialog: DialogEinstellungen
        get() = dialogCoordinator.getSettingsDialog()

    override fun requestSettingsResetOnQuit() {
        quitController.requestSettingsResetOnQuit()
    }

    fun restoreStartupDialogs() {
        dialogCoordinator.restoreStartupDialogs()
    }

    override fun quitApplication(): Boolean = quitController.quitApplication()

    private fun quitApplication(shutdownComputer: Boolean): Boolean = quitController.quitApplication(shutdownComputer)

    private fun activeAudiothekDownloadCount(): Int =
        (audiothekTab.existingComponent() as? AudiothekPanel)?.activeDownloadCount() ?: 0

    private fun pauseAudiothekDownloadsForShutdown() {
        (audiothekTab.existingComponent() as? AudiothekPanel)?.pauseDownloadsForShutdown()
    }

    private fun closeSystemTray() {
        platformIntegration.closeSystemTray()
    }

    private companion object {
        private val logger: Logger = LogManager.getLogger()
        private const val ICON_NAME = "MediathekView.png"
        private const val ICON_PATH = "/mediathek/res/"
        private const val ICON_WIDTH = 58
        private const val ICON_HEIGHT = 58
        private const val DISABLED_ACTION_KEY = "none"
        private const val MIN_WINDOW_WIDTH = 800
        private const val MIN_WINDOW_HEIGHT = 600
        private const val ACTION_MAP_KEY_COPY_HQ_URL = "COPY_HQ_URL"
        private const val ACTION_MAP_KEY_COPY_NORMAL_URL = "COPY_NORMAL_URL"
        private val NO_DOWNLOAD_PROGRESS_INDICATOR_FACTORY =
            Function<JFrame, DownloadProgressIndicator> { NoDownloadProgressIndicator }
        private val DEFAULT_TOOLBAR_INSTALLER = object : MainWindowToolbarInstaller {
            override fun configure(commonToolBar: JToolBar) {
                commonToolBar.isFloatable = true
                commonToolBar.name = "Allgemein"
            }

            override fun install(contentPane: Container, tabbedPane: JTabbedPane, commonToolBar: JToolBar) {
                tabbedPane.putClientProperty(MainWindowTabPlacementController.TRAILING_COMPONENT_KEY, commonToolBar)
                tabbedPane.putClientProperty(MainWindowTabPlacementController.TAB_ROTATION_KEY, "auto")
            }
        }
    }
}
