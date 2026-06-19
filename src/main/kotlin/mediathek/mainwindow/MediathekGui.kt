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
import mediathek.gui.dialog.DialogBeenden
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
import java.lang.reflect.InvocationTargetException
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import java.util.function.Consumer
import java.util.function.Function
import java.util.function.Supplier
import javax.swing.*
import kotlin.system.exitProcess

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
    private val applicationQuitInProgress = AtomicBoolean()
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
    private lateinit var tabFilme: GuiFilme
    private lateinit var tabDownloads: GuiDownloads
    private val menuTabSwitchController: MainWindowMenuTabSwitchController = MainWindowMenuTabSwitchController(
        tabbedPane,
        jMenuFilme,
        jMenuDownload,
        Supplier { tabFilme },
        Supplier { tabDownloads },
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
    private val tabLivestreams = LivestreamPanel(this)
    private val toggleZappLivestreamsTabAction = ToggleZappLivestreamsTabAction(tabbedPane, tabLivestreams)
    private val tabOnlineSearch = OnlineSearchPanel(createOnlineSearchHost())
    private val toggleOnlineSearchTabAction = ToggleOnlineSearchTabAction(tabbedPane, tabOnlineSearch)
    private val audiothekRepository = AudioRepository()
    private val tabAudiothek = AudiothekPanel(audiothekRepository, this)
    private val toggleAudiothekTabAction = ToggleAudiothekTabAction(tabbedPane, tabAudiothek)
    private val logDialog = LogDialog(this)
    private val notificationCenterFactory = notificationCenterFactory
    private val computerShutdown = computerShutdown
    private val darkModeActionPlacement = darkModeActionPlacement
    private val toolbarInstaller = toolbarInstaller
    private val tabPlacementController = tabPlacementController
    private val menuPolicy = menuPolicy
    private val menuBuilder by lazy(LazyThreadSafetyMode.NONE) { createMenuBuilder() }
    private val scrollBarConfigurator = scrollBarConfigurator
    private val downloadProgressIndicator: DownloadProgressIndicator = requireNotNull(downloadProgressIndicatorFactory.apply(this))
    private val mainWindowController: MainWindowController
    private val platformIntegration: MainWindowPlatformIntegration
    private val programUpdateCoordinator = MainWindowProgramUpdateCoordinator(this)
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
    private var resetSettingsOnQuit = false
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
        mainWindowController = createMainWindowController()
    }

    private fun createMainWindowController(): MainWindowController =
        MainWindowController(
            { initializeMainWindow() },
            { startMainWindowRuntime() }
        )

    fun start() {
        mainWindowController.start()
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
        SwingUtilities.invokeLater(::setApplicationWindowSize)
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
            statusBarController.updateComponentTreeUi()
        }
    }

    override fun ownerFrame(): JFrame = this

    override fun showMainWindow() {
        isVisible = true
    }

    override fun toggleMainWindowVisibility() {
        isVisible = !isVisible
        if (isVisible) {
            toFront()
            requestFocusInWindow()
        }
    }

    override fun refreshSystemTray() {
        platformIntegration.initializeSystemTray()
    }

    override fun repaintMainWindow() {
        repaint()
    }

    private fun getFilmTableRowCount(): Int = tabFilme.tableRowCount

    private fun getCurrentZeitraumFilterValue(): String = tabFilme.currentZeitraumFilterValue

    override val bookmarkDialog: BookmarkDialog?
        get() = tabFilme.getBookmarkDialog()

    override fun showManageBookmarkWindow() {
        tabFilme.showManageBookmarkWindow()
    }

    override fun resetFilterDialogPosition() {
        tabFilme.resetFilterDialogPosition()
    }

    override fun repaintFilmTab() {
        tabFilme.repaint()
    }

    override fun stopAllWaitingDownloads() {
        tabDownloads.stopAllWaitingDownloads()
    }

    private fun setSelectedListItemsCount(count: Long) {
        selectedListItemsProperty.setSelectedItems(count)
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
        actionMap.put(ACTION_MAP_KEY_COPY_HQ_URL, tabFilme.copyHqUrlToClipboardAction())
        actionMap.put(ACTION_MAP_KEY_COPY_NORMAL_URL, tabFilme.copyNormalUrlToClipboardAction())
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
            { tabFilme },
            { tabDownloads },
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
        SwingUtilities.invokeLater(::closeFilmlistDownloadProgress)
    }

    private fun closeFilmlistDownloadProgress() {
        filmlistDownloadProgressListener.close()
    }

    override fun showStatusBarProgress(): StatusBarProgressHandle =
        statusBarController.showProgress()

    override fun setFilmIndexingActionsEnabled(enabled: Boolean) {
        toggleBlacklistAction.isEnabled = enabled
        editBlacklistAction.isEnabled = enabled
        loadFilmListAction.isEnabled = enabled
    }

    private fun runOnEventDispatchThreadAndWait(description: String, action: Runnable) {
        if (SwingUtilities.isEventDispatchThread()) {
            action.run()
            return
        }

        try {
            SwingUtilities.invokeAndWait(action)
        } catch (exception: InterruptedException) {
            Thread.currentThread().interrupt()
            logger.error("{} interrupted", description, exception)
        } catch (exception: InvocationTargetException) {
            throw IllegalStateException("$description failed", exception.cause)
        }
    }

    private fun getFilmInfoDialog(): FilmInfoDialog =
        dialogCoordinator.getFilmInfoDialog()

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleTabVisualSettingsChangedEvent(event: TabVisualSettingsChangedEvent) {
        SwingUtilities.invokeLater {
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

        SwingUtilities.invokeLater { addComponentListener(WindowLocationConfigSaverListener()) }
    }

    @Handler
    private fun handleUpdateStateChanged(event: UpdateStateChangedEvent) {
        SwingUtilities.invokeLater { programUpdateCoordinator.update(event.isActive) }
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

    private fun initTabs() {
        val mainContentPane: Container = contentPane
        mainContentPane.add(tabbedPane, BorderLayout.CENTER)

        SplashScreenLifecycle.update(UIProgressState.LOAD_DOWNLOAD_TAB)
        tabDownloads = createTabDownloads(daten)

        SplashScreenLifecycle.update(UIProgressState.LOAD_FILM_TAB)
        tabFilme = createTabFilme(daten)

        SplashScreenLifecycle.update(UIProgressState.ADD_TABS_TO_UI)
        registerMainWindowTabs()
        tabRegistry.installVisibleTabs()

        if (ApplicationConfiguration.getInstance().restoreSelectedTab) {
            tabbedPane.restoreSavedTabPosition()
        }
        tabbedPane.installChangeListener()

        SplashScreenLifecycle.update(UIProgressState.CONFIGURE_TABS)
        configureTabPlacement()
        configureTabIcons()
    }

    private fun registerMainWindowTabs() {
        tabRegistry.register(
            MainWindowTab(
                GuiFilme.NAME,
                tabFilme,
                { true },
                { GetIcon.getProgramIcon("tab-film.png", 32, 32) },
                null,
                { tabFilme.disposePanel() },
            )
        )
        tabRegistry.register(
            MainWindowTab(
                GuiDownloads.NAME,
                tabDownloads,
                { true },
                { GetIcon.getProgramIcon("tab-download.png", 32, 32) },
                null,
                { tabDownloads.tabelleSpeichern() },
            )
        )
        tabRegistry.register(
            MainWindowTab(
                "Onlinesuche",
                tabOnlineSearch,
                { ApplicationConfiguration.getInstance().onlineSearchTabVisible },
                null,
                toggleOnlineSearchTabAction,
            ),
        )
        tabRegistry.register(
            MainWindowTab(
                "zapp Livestreams",
                tabLivestreams,
                { ApplicationConfiguration.getInstance().zappLivestreamsTabVisible },
                null,
                toggleZappLivestreamsTabAction,
            ),
        )
        tabRegistry.register(
            MainWindowTab(
                "Audiothek",
                tabAudiothek,
                { ApplicationConfiguration.getInstance().audiothekTabVisible },
                null,
                toggleAudiothekTabAction,
                { tabAudiothek.disposePanel() }
            ),
        )
    }

    override fun enableUpdateMenuItem(enable: Boolean) {
        searchProgramUpdateAction.isEnabled = enable
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
        downloadProgressIndicator.downloadStarted()
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleDownloadFinishedEvent(event: DownloadFinishedEvent) {
        downloadProgressIndicator.downloadFinished()
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleShowSettingsDialogEvent(event: ShowSettingsDialogEvent) {
        SwingUtilities.invokeLater {
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
        SwingUtilities.invokeLater { loadFilmListAction.isEnabled = false }
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleFilmlistWriteStopEvent(event: FilmListWriteStopEvent) {
        SwingUtilities.invokeLater { loadFilmListAction.isEnabled = true }
    }

    private fun initMenus() {
        installMenuTabSwitchListener()
        menuBuilder.initializeMenus()
        afterMenusInitialized.accept(this)
    }

    private val settingsDialog: DialogEinstellungen
        get() = dialogCoordinator.getSettingsDialog()

    override fun requestSettingsResetOnQuit() {
        resetSettingsOnQuit = true
    }

    fun restoreStartupDialogs() {
        dialogCoordinator.restoreStartupDialogs()
    }

    override fun quitApplication(): Boolean = quitApplication(false)

    private fun quitApplication(shutdownComputer: Boolean): Boolean {
        if (!applicationQuitInProgress.compareAndSet(false, true)) {
            return true
        }

        val confirmation = confirmApplicationQuitOnEdt(shutdownComputer)
        if (!confirmation.canQuit) {
            applicationQuitInProgress.set(false)
            return false
        }

        startApplicationShutdown(confirmation.shutdownComputer)
        return true
    }

    private fun confirmApplicationQuitOnEdt(shutdownComputer: Boolean): QuitConfirmation {
        if (SwingUtilities.isEventDispatchThread()) {
            return confirmApplicationQuit(shutdownComputer)
        }

        val confirmation = AtomicReference<QuitConfirmation>()
        runOnEventDispatchThreadAndWait(
            "Confirm application quit",
        ) { confirmation.set(confirmApplicationQuit(shutdownComputer)) }
        return confirmation.get() ?: QuitConfirmation.declined()
    }

    private fun confirmApplicationQuit(requestShutdownComputer: Boolean): QuitConfirmation {
        var shutdownComputer = requestShutdownComputer
        if (daten.listeDownloads.unfinishedDownloads() > 0) {
            val dialogBeenden = DialogBeenden(this, this)
            dialogBeenden.isVisible = true
            if (!dialogBeenden.applicationCanTerminate) {
                return QuitConfirmation.declined()
            }
            shutdownComputer = dialogBeenden.isShutdownRequested
        }

        if (tabAudiothek.activeDownloadCount() > 0) {
            val activeAudiothekDownloads = tabAudiothek.activeDownloadCount()
            val result = JOptionPane.showConfirmDialog(
                this,
                if (activeAudiothekDownloads == 1) {
                    "Es ist noch ein Audiothek-Download aktiv.\nTrotzdem beenden?"
                } else {
                    "Es sind noch $activeAudiothekDownloads Audiothek-Downloads aktiv.\nTrotzdem beenden?"
                },
                Konstanten.PROGRAMMNAME,
                JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE,
            )
            if (result != JOptionPane.YES_OPTION) {
                return QuitConfirmation.declined()
            }
            tabAudiothek.pauseDownloadsForShutdown()
        }

        return QuitConfirmation(true, shutdownComputer)
    }

    private data class QuitConfirmation(
        val canQuit: Boolean,
        val shutdownComputer: Boolean,
    ) {
        companion object {
            fun declined(): QuitConfirmation = QuitConfirmation(canQuit = false, shutdownComputer = false)
        }
    }

    private fun startApplicationShutdown(shutdownComputer: Boolean) {
        Thread.ofPlatform()
            .name("MediathekView-shutdown")
            .daemon(false)
            .start { performApplicationShutdown(shutdownComputer) }
    }

    private fun performApplicationShutdown(shutdownComputer: Boolean) {
        createShutdownCoordinator().shutdown(shutdownComputer)
        exitProcess(0)
    }

    private fun createShutdownCoordinator(): MainWindowShutdownCoordinator =
        MainWindowShutdownCoordinator(
            this,
            daten,
            dialogCoordinator,
            tabRegistry,
            computerShutdown,
            resetSettingsOnQuit,
            { filmlistReloadCoordinator.close() },
            { closeProgramUpdateCoordinator() },
            { closeSystemTray() },
            { closeNotificationCenter() },
            { shutdownTimerPool() },
            { waitForCommonPoolToComplete() },
            ::runOnEventDispatchThreadAndWait
        )

    private fun closeSystemTray() {
        platformIntegration.closeSystemTray()
    }

    private fun shutdownTimerPool() {
        logger.trace("Entering shutdownTimerPool()")

        try {
            val taskList = TimerPool.shutdown(500, TimeUnit.MILLISECONDS)
            if (CommandLineOptions.isDebugModeEnabled() && taskList.isNotEmpty()) {
                logger.trace("timerPool taskList was not empty: {}", taskList.toString())
            }
        } catch (exception: InterruptedException) {
            Thread.currentThread().interrupt()
            logger.error("timerPool shutdown exception", exception)
        }

        logger.trace("Leaving shutdownTimerPool()")
    }

    private fun waitForCommonPoolToComplete() {
        logger.trace("Entering waitForCommonPoolToComplete()")

        val pool = ForkJoinPool.commonPool()
        if (!pool.awaitQuiescence(COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS.toLong(), TimeUnit.SECONDS)) {
            logger.warn(
                "Common pool did not become quiescent within {} seconds. Continuing shutdown.",
                COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS,
            )
        }

        logger.trace("Leaving waitForCommonPoolToComplete()")
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
        private const val COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS = 5
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
