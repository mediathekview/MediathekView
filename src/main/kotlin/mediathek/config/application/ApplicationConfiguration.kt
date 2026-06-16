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

package mediathek.config.application

import mediathek.config.Konstanten
import mediathek.config.StandardLocations
import mediathek.daten.Country
import mediathek.tool.timer.TimerPool
import org.apache.commons.configuration2.XMLConfiguration
import org.apache.commons.configuration2.event.ConfigurationEvent
import org.apache.commons.configuration2.event.EventListener
import org.apache.commons.configuration2.ex.ConfigurationException
import org.apache.commons.configuration2.io.FileHandler
import org.apache.commons.configuration2.sync.LockMode
import org.apache.commons.configuration2.sync.ReadWriteSynchronizer
import org.apache.logging.log4j.LogManager
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.RejectedExecutionException
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock
import kotlin.system.exitProcess
import kotlin.time.Duration.Companion.seconds

/**
 * The global application configuration class. This will contain all the config data in the future.
 */
class ApplicationConfiguration private constructor() {
    private val writerLock = ReentrantLock()
    private val config = createXmlConfiguration()
    private val handler = createFileHandler()
    private val windowStateConfiguration = ApplicationWindowStateConfiguration(config)
    private val downloadConfiguration = ApplicationDownloadConfiguration(config)
    private val filmListConfiguration = ApplicationFilmListConfiguration(config)
    private val networkConfiguration = ApplicationNetworkConfiguration(config)
    private val blacklistConfiguration = ApplicationBlacklistConfiguration(config)
    private val externalProgramsConfiguration = ApplicationExternalProgramsConfiguration(config)
    private val searchConfiguration = ApplicationSearchConfiguration(config)
    private val tableConfiguration = ApplicationTableConfiguration(config)
    private val audiothekConfiguration = ApplicationAudiothekConfiguration(config)
    private val onlineSearchConfiguration = ApplicationOnlineSearchConfiguration(config)
    private val aboAndFilenameConfiguration = ApplicationAboAndFilenameConfiguration(config)
    private val mainWindowConfiguration = ApplicationMainWindowConfiguration(config)
    private val generalConfiguration = ApplicationGeneralConfiguration(config)
    private val timerTaskListener = TimerTaskListener()

    /**
     * Stores the previous writer task in order to cancel it if necessary. We don't want to write
     * several times in a row.
     */
    private var future: ScheduledFuture<*>? = null
    private var timedEventWritingEnabled = false

    init {
        loadOrCreateConfiguration()
        initializeTimedEventWriting()
        writeVersionMetadata()
    }

    fun createFilterConfiguration(): FilterConfiguration =
        object : FilterConfiguration(config) {}

    var geographicLocation: Country
        get() = generalConfiguration.geographicLocation
        set(value) {
            generalConfiguration.geographicLocation = value
        }

    val userAgent: String
        get() = generalConfiguration.userAgent

    fun setUserAgent(newValue: String?) {
        generalConfiguration.setUserAgent(newValue)
    }

    var useModernSearch: Boolean
        get() = searchConfiguration.useModernSearch
        set(value) {
            searchConfiguration.useModernSearch = value
        }

    var darkMode: Boolean
        get() = generalConfiguration.darkMode
        set(value) {
            generalConfiguration.darkMode = value
        }

    var useSystemDarkMode: Boolean
        get() = generalConfiguration.useSystemDarkMode
        set(value) {
            generalConfiguration.useSystemDarkMode = value
        }

    var automaticUpdateCheck: Boolean
        get() = generalConfiguration.automaticUpdateCheck
        set(value) {
            generalConfiguration.automaticUpdateCheck = value
        }

    var isNewFilmLengthActivationQuestionCompleted: Boolean
        get() = generalConfiguration.isNewFilmLengthActivationQuestionCompleted
        set(value) {
            generalConfiguration.isNewFilmLengthActivationQuestionCompleted = value
        }

    var isNewSenderActivationQuestionCompleted: Boolean
        get() = generalConfiguration.isNewSenderActivationQuestionCompleted
        set(value) {
            generalConfiguration.isNewSenderActivationQuestionCompleted = value
        }

    var programInformationDisplayedNumber: Int
        get() = generalConfiguration.programInformationDisplayedNumber
        set(value) {
            generalConfiguration.programInformationDisplayedNumber = value
        }

    var seenHistoryMaintenanceLastRun: LocalDate?
        get() = generalConfiguration.seenHistoryMaintenanceLastRun
        set(value) {
            if (value != null) {
                generalConfiguration.seenHistoryMaintenanceLastRun = value
            }
        }

    var showNotifications: Boolean
        get() = generalConfiguration.showNotifications
        set(value) {
            generalConfiguration.showNotifications = value
        }

    var showOrfConfigHelp: Boolean
        get() = generalConfiguration.showOrfConfigHelp
        set(value) {
            generalConfiguration.showOrfConfigHelp = value
        }

    var senderListVerticalWrap: Boolean
        get() = mainWindowConfiguration.senderListVerticalWrap
        set(value) {
            mainWindowConfiguration.senderListVerticalWrap = value
        }

    var jDownloaderUrl: String
        get() = externalProgramsConfiguration.jDownloaderUrl
        set(value) {
            externalProgramsConfiguration.jDownloaderUrl = value
        }

    var directoryOpenProgram: String
        get() = externalProgramsConfiguration.directoryOpenProgram
        set(value) {
            externalProgramsConfiguration.directoryOpenProgram = value
        }

    var videoPlayerProgram: String
        get() = externalProgramsConfiguration.videoPlayerProgram
        set(value) {
            externalProgramsConfiguration.videoPlayerProgram = value
        }

    var webBrowserProgram: String
        get() = externalProgramsConfiguration.webBrowserProgram
        set(value) {
            externalProgramsConfiguration.webBrowserProgram = value
        }

    var linuxShutdownCommand: String
        get() = externalProgramsConfiguration.linuxShutdownCommand
        set(value) {
            externalProgramsConfiguration.linuxShutdownCommand = value
        }

    var standardVlcPath: String
        get() = externalProgramsConfiguration.standardVlcPath
        set(value) {
            externalProgramsConfiguration.standardVlcPath = value
        }

    var standardFFmpegPath: String
        get() = externalProgramsConfiguration.standardFFmpegPath
        set(value) {
            externalProgramsConfiguration.standardFFmpegPath = value
        }

    var pyLoadUrl: String
        get() = externalProgramsConfiguration.pyLoadUrl
        set(value) {
            externalProgramsConfiguration.pyLoadUrl = value
        }

    var pyLoadUser: String
        get() = externalProgramsConfiguration.pyLoadUser
        set(value) {
            externalProgramsConfiguration.pyLoadUser = value
        }

    var pyLoadPassword: String
        get() = externalProgramsConfiguration.pyLoadPassword
        set(value) {
            externalProgramsConfiguration.pyLoadPassword = value
        }

    var isBlacklistEnabled: Boolean
        get() = blacklistConfiguration.blacklistEnabled
        set(value) {
            blacklistConfiguration.blacklistEnabled = value
        }

    val isBlacklistDuplicateFilteringEnabled: Boolean
        get() = blacklistConfiguration.blacklistDuplicateFilteringEnabled

    var blacklistDoNotShowGeoblockedFilms: Boolean
        get() = blacklistConfiguration.blacklistDoNotShowGeoblockedFilms
        set(value) {
            blacklistConfiguration.blacklistDoNotShowGeoblockedFilms = value
        }

    var blacklistDoNotShowFutureFilms: Boolean
        get() = blacklistConfiguration.blacklistDoNotShowFutureFilms
        set(value) {
            blacklistConfiguration.blacklistDoNotShowFutureFilms = value
        }

    var blacklistApplyToAbo: Boolean
        get() = blacklistConfiguration.blacklistApplyToAbo
        set(value) {
            blacklistConfiguration.blacklistApplyToAbo = value
        }

    var blacklistWhitelistMode: Boolean
        get() = blacklistConfiguration.blacklistWhitelistMode
        set(value) {
            blacklistConfiguration.blacklistWhitelistMode = value
        }

    var blacklistMinimumFilmLengthMinutes: Int
        get() = blacklistConfiguration.blacklistMinimumFilmLengthMinutes
        set(value) {
            blacklistConfiguration.blacklistMinimumFilmLengthMinutes = value
        }

    var maxSimultaneousDownloads: Int
        get() = downloadConfiguration.maxSimultaneousDownloads
        set(value) {
            downloadConfiguration.maxSimultaneousDownloads = value
        }

    var fetchMissingDownloadFileSize: Boolean
        get() = downloadConfiguration.fetchMissingDownloadFileSize
        set(value) {
            downloadConfiguration.fetchMissingDownloadFileSize = value
        }

    var downloadContinuationTime: Int
        get() = downloadConfiguration.downloadContinuationTime
        set(value) {
            downloadConfiguration.downloadContinuationTime = value
        }

    var useCdnAwareDirectDownload: Boolean
        get() = downloadConfiguration.useCdnAwareDirectDownload
        set(value) {
            downloadConfiguration.useCdnAwareDirectDownload = value
        }

    var showLastUsedDownloadPath: Boolean
        get() = downloadConfiguration.showLastUsedDownloadPath
        set(value) {
            downloadConfiguration.showLastUsedDownloadPath = value
        }

    var savedDownloadTargetPaths: String
        get() = downloadConfiguration.savedDownloadTargetPaths
        set(value) {
            downloadConfiguration.savedDownloadTargetPaths = value
        }

    var playSoundAfterDownload: Boolean
        get() = downloadConfiguration.playSoundAfterDownload
        set(value) {
            downloadConfiguration.playSoundAfterDownload = value
        }

    var showDownloadErrorMessage: Boolean
        get() = downloadConfiguration.showDownloadErrorMessage
        set(value) {
            downloadConfiguration.showDownloadErrorMessage = value
        }

    var startDownloadsImmediately: Boolean
        get() = downloadConfiguration.startDownloadsImmediately
        set(value) {
            downloadConfiguration.startDownloadsImmediately = value
        }

    var showDownloadDescription: Boolean
        get() = downloadConfiguration.showDownloadDescription
        set(value) {
            downloadConfiguration.showDownloadDescription = value
        }

    var defaultAboMinimumDurationMinutes: Int
        get() = aboAndFilenameConfiguration.defaultAboMinimumDurationMinutes
        set(value) {
            aboAndFilenameConfiguration.defaultAboMinimumDurationMinutes = value
        }

    var searchAbosImmediately: Boolean
        get() = aboAndFilenameConfiguration.searchAbosImmediately
        set(value) {
            aboAndFilenameConfiguration.searchAbosImmediately = value
        }

    var useFilenameReplaceTable: Boolean
        get() = aboAndFilenameConfiguration.useFilenameReplaceTable
        set(value) {
            aboAndFilenameConfiguration.useFilenameReplaceTable = value
        }

    var onlyAsciiFilenames: Boolean
        get() = aboAndFilenameConfiguration.onlyAsciiFilenames
        set(value) {
            aboAndFilenameConfiguration.onlyAsciiFilenames = value
        }

    fun getDownloadDisplayFilter(defaultValue: String): String =
        downloadConfiguration.getDownloadDisplayFilter(defaultValue)

    fun setDownloadDisplayFilter(newValue: String) {
        downloadConfiguration.setDownloadDisplayFilter(newValue)
    }

    fun getDownloadViewFilter(defaultValue: String): String =
        downloadConfiguration.getDownloadViewFilter(defaultValue)

    fun setDownloadViewFilter(newValue: String) {
        downloadConfiguration.setDownloadViewFilter(newValue)
    }

    fun getGlazedTableSortKeys(configPrefix: String): String =
        tableConfiguration.getGlazedTableSortKeys(configPrefix).orEmpty()

    fun setGlazedTableSortKeys(configPrefix: String, json: String) {
        tableConfiguration.setGlazedTableSortKeys(configPrefix, json)
    }

    fun getTableColumnSettings(configPrefix: String): String =
        tableConfiguration.getTableColumnSettings(configPrefix).orEmpty()

    fun setTableColumnSettings(configPrefix: String, json: String) {
        tableConfiguration.setTableColumnSettings(configPrefix, json)
    }

    var searchUseFilmDescriptions: Boolean
        get() = searchConfiguration.searchUseFilmDescriptions
        set(value) {
            searchConfiguration.searchUseFilmDescriptions = value
        }

    fun getSearchHistoryItems(luceneSearch: Boolean): Any? =
        searchConfiguration.getSearchHistoryItems(luceneSearch)

    fun setSearchHistoryItems(luceneSearch: Boolean, json: String) {
        searchConfiguration.setSearchHistoryItems(luceneSearch, json)
    }

    var downloadRateLimitActive: Boolean
        get() = downloadConfiguration.downloadRateLimitActive
        set(value) {
            downloadConfiguration.downloadRateLimitActive = value
        }

    var downloadRateLimit: Long
        get() = downloadConfiguration.downloadRateLimit
        set(value) {
            downloadConfiguration.downloadRateLimit = value
        }

    var evaluateFilmDuplicates: Boolean
        get() = filmListConfiguration.evaluateFilmDuplicates
        set(value) {
            filmListConfiguration.evaluateFilmDuplicates = value
        }

    var filmDescriptionVisible: Boolean
        get() = filmListConfiguration.filmDescriptionVisible
        set(value) {
            filmListConfiguration.filmDescriptionVisible = value
        }

    var luceneDirectoryMode: String
        get() = filmListConfiguration.luceneDirectoryMode
        set(value) {
            filmListConfiguration.luceneDirectoryMode = value
        }

    var filmListLoadNumDays: Int
        get() = filmListConfiguration.filmListLoadNumDays
        set(value) {
            filmListConfiguration.filmListLoadNumDays = value
        }

    var approvedFilmlistLoadSenders: List<String>
        get() = filmListConfiguration.approvedFilmlistLoadSenders
        set(value) {
            filmListConfiguration.approvedFilmlistLoadSenders = value
        }

    fun setApprovedFilmlistLoadSenders(senders: Collection<String>) {
        filmListConfiguration.setApprovedFilmlistLoadSenders(senders)
    }

    var filmTableLineBreak: Boolean
        get() = tableConfiguration.filmTableLineBreak
        set(value) {
            tableConfiguration.filmTableLineBreak = value
        }

    var downloadTableLineBreak: Boolean
        get() = tableConfiguration.downloadTableLineBreak
        set(value) {
            tableConfiguration.downloadTableLineBreak = value
        }

    var filmTableShowSenderIcons: Boolean
        get() = tableConfiguration.filmTableShowSenderIcons
        set(value) {
            tableConfiguration.filmTableShowSenderIcons = value
        }

    var filmTableUseSmallSenderIcons: Boolean
        get() = tableConfiguration.filmTableUseSmallSenderIcons
        set(value) {
            tableConfiguration.filmTableUseSmallSenderIcons = value
        }

    var downloadTableShowSenderIcons: Boolean
        get() = tableConfiguration.downloadTableShowSenderIcons
        set(value) {
            tableConfiguration.downloadTableShowSenderIcons = value
        }

    var downloadTableUseSmallSenderIcons: Boolean
        get() = tableConfiguration.downloadTableUseSmallSenderIcons
        set(value) {
            tableConfiguration.downloadTableUseSmallSenderIcons = value
        }

    var aboTableShowSenderIcons: Boolean
        get() = tableConfiguration.aboTableShowSenderIcons
        set(value) {
            tableConfiguration.aboTableShowSenderIcons = value
        }

    var aboTableUseSmallSenderIcons: Boolean
        get() = tableConfiguration.aboTableUseSmallSenderIcons
        set(value) {
            tableConfiguration.aboTableUseSmallSenderIcons = value
        }

    var filmTableColumnConfiguration: String
        get() = tableConfiguration.filmTableColumnConfiguration
        set(value) {
            tableConfiguration.filmTableColumnConfiguration = value
        }

    var downloadTableColumnConfiguration: String
        get() = tableConfiguration.downloadTableColumnConfiguration
        set(value) {
            tableConfiguration.downloadTableColumnConfiguration = value
        }

    var aboTableColumnConfiguration: String
        get() = tableConfiguration.aboTableColumnConfiguration
        set(value) {
            tableConfiguration.aboTableColumnConfiguration = value
        }

    var filmListUpdateType: Int
        get() = filmListConfiguration.filmListUpdateType
        set(value) {
            filmListConfiguration.filmListUpdateType = value
        }

    var filmListManualImportUrl: String
        get() = filmListConfiguration.filmListManualImportUrl
        set(value) {
            filmListConfiguration.filmListManualImportUrl = value
        }

    var filmListLoadTrailer: Boolean
        get() = filmListConfiguration.filmListLoadTrailer
        set(value) {
            filmListConfiguration.filmListLoadTrailer = value
        }

    var filmListLoadAudioDescription: Boolean
        get() = filmListConfiguration.filmListLoadAudioDescription
        set(value) {
            filmListConfiguration.filmListLoadAudioDescription = value
        }

    var filmListLoadSignLanguage: Boolean
        get() = filmListConfiguration.filmListLoadSignLanguage
        set(value) {
            filmListConfiguration.filmListLoadSignLanguage = value
        }

    var filmListLoadLivestreams: Boolean
        get() = filmListConfiguration.filmListLoadLivestreams
        set(value) {
            filmListConfiguration.filmListLoadLivestreams = value
        }

    var extendOldFilmList: Boolean
        get() = filmListConfiguration.extendOldFilmList
        set(value) {
            filmListConfiguration.extendOldFilmList = value
        }

    var httpProxyHost: String
        get() = networkConfiguration.httpProxyHost
        set(value) {
            networkConfiguration.httpProxyHost = value
        }

    var httpProxyPort: String
        get() = networkConfiguration.httpProxyPort
        set(value) {
            networkConfiguration.httpProxyPort = value
        }

    var httpProxyUser: String
        get() = networkConfiguration.httpProxyUser
        set(value) {
            networkConfiguration.httpProxyUser = value
        }

    var httpProxyPassword: String
        get() = networkConfiguration.httpProxyPassword
        set(value) {
            networkConfiguration.httpProxyPassword = value
        }

    fun setHttpProxy(host: String, port: String, user: String, password: String) {
        networkConfiguration.setHttpProxy(host, port, user, password)
    }

    fun getNetworkingDnsMode(defaultValue: String): String =
        networkConfiguration.getNetworkingDnsMode(defaultValue)

    fun setNetworkingDnsMode(newValue: String) {
        networkConfiguration.setNetworkingDnsMode(newValue)
    }

    val httpTrafficTraceLevel: String
        get() = networkConfiguration.httpTrafficTraceLevel

    var toolbarBlacklistIconWithText: Boolean
        get() = mainWindowConfiguration.toolbarBlacklistIconWithText
        set(value) {
            mainWindowConfiguration.toolbarBlacklistIconWithText = value
        }

    var filmTimeUseLongFormat: Boolean
        get() = mainWindowConfiguration.filmTimeUseLongFormat
        set(value) {
            mainWindowConfiguration.filmTimeUseLongFormat = value
        }

    var installTabSwitchListener: Boolean
        get() = mainWindowConfiguration.installTabSwitchListener
        set(value) {
            mainWindowConfiguration.installTabSwitchListener = value
        }

    var restoreSelectedTab: Boolean
        get() = mainWindowConfiguration.restoreSelectedTab
        set(value) {
            mainWindowConfiguration.restoreSelectedTab = value
        }

    var selectedMainWindowTabIndex: Int
        get() = mainWindowConfiguration.selectedMainWindowTabIndex
        set(value) {
            mainWindowConfiguration.selectedMainWindowTabIndex = value
        }

    var tabPositionTop: Boolean
        get() = mainWindowConfiguration.tabPositionTop
        set(value) {
            mainWindowConfiguration.tabPositionTop = value
        }

    var mainWindowTabIcons: Boolean
        get() = mainWindowConfiguration.mainWindowTabIcons
        set(value) {
            mainWindowConfiguration.mainWindowTabIcons = value
        }

    var localSenderIcons: Boolean
        get() = mainWindowConfiguration.localSenderIcons
        set(value) {
            mainWindowConfiguration.localSenderIcons = value
        }

    var listIconPositionRight: Boolean
        get() = mainWindowConfiguration.listIconPositionRight
        set(value) {
            mainWindowConfiguration.listIconPositionRight = value
        }

    var programSetShowAllSettings: Boolean
        get() = mainWindowConfiguration.programSetShowAllSettings
        set(value) {
            mainWindowConfiguration.programSetShowAllSettings = value
        }

    var standardProgramSetVersion: String
        get() = mainWindowConfiguration.standardProgramSetVersion
        set(value) {
            mainWindowConfiguration.standardProgramSetVersion = value
        }

    var bandwidthMonitorVisible: Boolean
        get() = windowStateConfiguration.bandwidthMonitorVisible
        set(value) {
            windowStateConfiguration.bandwidthMonitorVisible = value
        }

    val bandwidthMonitorDialogState: BandwidthMonitorDialogState
        get() = windowStateConfiguration.bandwidthMonitorDialogState

    fun setBandwidthMonitorDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setBandwidthMonitorDialogBounds(x, y, width, height)
    }

    val memoryMonitorDialogState: MemoryMonitorDialogState
        get() = windowStateConfiguration.memoryMonitorDialogState

    var memoryMonitorDialogVisible: Boolean
        get() = windowStateConfiguration.memoryMonitorDialogVisible
        set(value) {
            windowStateConfiguration.memoryMonitorDialogVisible = value
        }

    fun setMemoryMonitorDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setMemoryMonitorDialogBounds(x, y, width, height)
    }

    val filmInfoDialogState: FilmInfoDialogState
        get() = windowStateConfiguration.filmInfoDialogState

    var filmInfoDialogVisible: Boolean
        get() = windowStateConfiguration.filmInfoDialogVisible
        set(value) {
            windowStateConfiguration.filmInfoDialogVisible = value
        }

    fun setFilmInfoDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setFilmInfoDialogBounds(x, y, width, height)
    }

    val loadFilmListDialogState: LoadFilmListDialogState
        get() = windowStateConfiguration.loadFilmListDialogState

    fun setLoadFilmListDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setLoadFilmListDialogBounds(x, y, width, height)
    }

    val settingsDialogState: SettingsDialogState
        get() = windowStateConfiguration.settingsDialogState

    fun setSettingsDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setSettingsDialogBounds(x, y, width, height)
    }

    var filterDialogVisible: Boolean
        get() = windowStateConfiguration.filterDialogVisible
        set(value) {
            windowStateConfiguration.filterDialogVisible = value
        }

    val filterDialogState: FilterDialogState
        get() = windowStateConfiguration.filterDialogState

    fun setFilterDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setFilterDialogBounds(x, y, width, height)
    }

    val editDownloadDialogState: EditDownloadDialogState
        get() = windowStateConfiguration.editDownloadDialogState

    fun setEditDownloadDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setEditDownloadDialogBounds(x, y, width, height)
    }

    val addDownloadDialogPosition: AddDownloadDialogPosition
        get() = windowStateConfiguration.addDownloadDialogPosition

    fun setAddDownloadDialogPosition(x: Int, y: Int) {
        windowStateConfiguration.setAddDownloadDialogPosition(x, y)
    }

    val manageAboDialogState: ManageAboDialogState
        get() = windowStateConfiguration.manageAboDialogState

    fun setManageAboDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setManageAboDialogBounds(x, y, width, height)
    }

    val duplicateFilmDetailsDialogState: DuplicateFilmDetailsDialogState
        get() = windowStateConfiguration.duplicateFilmDetailsDialogState

    fun setDuplicateFilmDetailsDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setDuplicateFilmDetailsDialogBounds(x, y, width, height)
    }

    val duplicateStatisticsDialogState: DuplicateStatisticsDialogState
        get() = windowStateConfiguration.duplicateStatisticsDialogState

    fun setDuplicateStatisticsDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setDuplicateStatisticsDialogBounds(x, y, width, height)
    }

    val editHistoryDialogState: EditHistoryDialogState
        get() = windowStateConfiguration.editHistoryDialogState

    fun setEditHistoryDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setEditHistoryDialogBounds(x, y, width, height)
    }

    val bookmarkDialogBounds: BookmarkDialogBounds
        get() = windowStateConfiguration.bookmarkDialogBounds

    fun setBookmarkDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        windowStateConfiguration.setBookmarkDialogBounds(x, y, width, height)
    }

    fun getDownloadToolbarState(toolbarId: String, defaultOrientation: Int): DownloadToolbarState =
        downloadConfiguration.getDownloadToolbarState(toolbarId, defaultOrientation)

    fun setDownloadToolbarState(toolbarId: String, state: DownloadToolbarState) {
        downloadConfiguration.setDownloadToolbarState(toolbarId, state)
    }

    val defaultFontState: DefaultFontState
        get() = mainWindowConfiguration.defaultFontState

    fun setDefaultFontState(family: String?, size: Int) {
        mainWindowConfiguration.setDefaultFontState(family, size)
    }

    fun clearDefaultFontState() {
        mainWindowConfiguration.clearDefaultFontState()
    }

    fun clearAddDownloadDialogSize() {
        windowStateConfiguration.clearAddDownloadDialogSize()
    }

    var useTray: Boolean
        get() = mainWindowConfiguration.useTray
        set(value) {
            mainWindowConfiguration.useTray = value
        }

    var exitDialogAction: String?
        get() = mainWindowConfiguration.exitDialogAction
        set(value) {
            mainWindowConfiguration.exitDialogAction = value
        }

    var zappLivestreamsTabVisible: Boolean
        get() = mainWindowConfiguration.zappLivestreamsTabVisible
        set(value) {
            mainWindowConfiguration.zappLivestreamsTabVisible = value
        }

    var audiothekTabVisible: Boolean
        get() = audiothekConfiguration.audiothekTabVisible
        set(value) {
            audiothekConfiguration.audiothekTabVisible = value
        }

    var audiothekOnlineSearch: Boolean
        get() = audiothekConfiguration.audiothekOnlineSearch
        set(value) {
            audiothekConfiguration.audiothekOnlineSearch = value
        }

    var audiothekSearchHistory: String
        get() = audiothekConfiguration.audiothekSearchHistory
        set(value) {
            audiothekConfiguration.audiothekSearchHistory = value
        }

    var audiothekTableState: String
        get() = audiothekConfiguration.audiothekTableState
        set(value) {
            audiothekConfiguration.audiothekTableState = value
        }

    var onlineSearchTabVisible: Boolean
        get() = onlineSearchConfiguration.onlineSearchTabVisible
        set(value) {
            onlineSearchConfiguration.onlineSearchTabVisible = value
        }

    var onlineSearchArdSearchHistory: String
        get() = onlineSearchConfiguration.ardSearchHistory
        set(value) {
            onlineSearchConfiguration.ardSearchHistory = value
        }

    var onlineSearchArdUrlHistory: String
        get() = onlineSearchConfiguration.ardUrlHistory
        set(value) {
            onlineSearchConfiguration.ardUrlHistory = value
        }

    var onlineSearchZdfSearchHistory: String
        get() = onlineSearchConfiguration.zdfSearchHistory
        set(value) {
            onlineSearchConfiguration.zdfSearchHistory = value
        }

    var onlineSearchZdfUrlHistory: String
        get() = onlineSearchConfiguration.zdfUrlHistory
        set(value) {
            onlineSearchConfiguration.zdfUrlHistory = value
        }

    val mainWindowMaximized: Boolean
        get() = windowStateConfiguration.mainWindowMaximized

    fun getMainWindowBounds(minimumWidth: Int, minimumHeight: Int): MainWindowBounds =
        windowStateConfiguration.getMainWindowBounds(minimumWidth, minimumHeight)

    fun setMainWindowResizedState(maximized: Boolean, width: Int, height: Int) {
        windowStateConfiguration.setMainWindowResizedState(maximized, width, height)
    }

    fun setMainWindowMovedState(maximized: Boolean, x: Int, y: Int) {
        windowStateConfiguration.setMainWindowMovedState(maximized, x, y)
    }

    var buttonsPanelVisible: Boolean
        get() = mainWindowConfiguration.buttonsPanelVisible
        set(value) {
            mainWindowConfiguration.buttonsPanelVisible = value
        }

    private fun initializeTimedEventWriting() {
        writerLock.withLock {
            timedEventWritingEnabled = true
        }
        config.addEventListener(ConfigurationEvent.ANY, timerTaskListener)
    }

    private fun loadOrCreateConfiguration() {
        try {
            handler.load()
            updateNewerDefaults()
        } catch (_: ConfigurationException) {
            createDefaultConfigSettings()
        }
    }

    private fun createFileHandler(): FileHandler =
        FileHandler(config).apply {
            encoding = "UTF-8"
            path = StandardLocations.getApplicationSettingsFile().toString()
        }

    private fun writeVersionMetadata() {
        config.withLock(LockMode.WRITE) {
            val version = Konstanten.MVVERSION
            setProperty(CONFIG_MAJOR, version.major)
            setProperty(CONFIG_MINOR, version.minor)
            setProperty(CONFIG_PATCH, version.patch)
        }
    }

    fun writeConfiguration() {
        try {
            config.removeEventListener(ConfigurationEvent.ANY, timerTaskListener)
            withWriterLock {
                timedEventWritingEnabled = false
                cancelPendingWriterTask(mayInterruptIfRunning = true)
                handler.save()
            }
        } catch (configurationException: ConfigurationException) {
            logger.debug("Something went wrong while saving the config.", configurationException)
        }
    }

    fun cleanupConfiguration(dryRun: Boolean): ApplicationConfigurationCleanupStatistics {
        config.removeEventListener(ConfigurationEvent.ANY, timerTaskListener)
        return try {
            withWriterLock {
                timedEventWritingEnabled = false
                cancelPendingWriterTask(mayInterruptIfRunning = true)

                val settingsPath = StandardLocations.getApplicationSettingsFile()
                val backupPath = if (dryRun) null else createSettingsBackup(settingsPath)
                val statistics = ApplicationConfigurationCleanupService(config).cleanup(settingsPath, backupPath, dryRun)
                if (!dryRun) {
                    handler.save()
                }
                statistics
            }
        } finally {
            writerLock.withLock {
                timedEventWritingEnabled = true
                config.addEventListener(ConfigurationEvent.ANY, timerTaskListener)
            }
        }
    }

    private fun createSettingsBackup(settingsPath: Path): Path? {
        if (Files.notExists(settingsPath)) {
            return null
        }
        val timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"))
        val backupPath = settingsPath.resolveSibling("${settingsPath.fileName}.cleanup-backup-$timestamp")
        Files.copy(settingsPath, backupPath, StandardCopyOption.COPY_ATTRIBUTES, StandardCopyOption.REPLACE_EXISTING)
        return backupPath
    }

    private fun <T> withWriterLock(action: () -> T): T = writerLock.withLock(action)

    private fun cancelPendingWriterTask(mayInterruptIfRunning: Boolean) {
        future?.cancel(mayInterruptIfRunning)
        future = null
    }

    private fun createDefaultConfigSettings() {
        try {
            generalConfiguration.setDefaultUserAgent()
            externalProgramsConfiguration.setDefaultJDownloaderUrl()
            filmListConfiguration.setDefaultLuceneDirectoryMode()
            downloadConfiguration.setDefaultCdnAwareDirectDownload()
            generalConfiguration.geographicLocation = Country.DE
            handler.save()
        } catch (configurationException: ConfigurationException) {
            logger.error("Something went wrong while creating the default config.", configurationException)
        } catch (noSuchElementException: NoSuchElementException) {
            logger.error("A config element is missing.", noSuchElementException)
            exitProcess(2)
        }
    }

    private fun updateNewerDefaults() {
        generalConfiguration.ensureGeographicLocationDefault()
        mainWindowConfiguration.ensureInstallTabSwitchListenerDefault()
        externalProgramsConfiguration.ensureJDownloaderUrlDefault()
        filmListConfiguration.ensureLuceneDirectoryModeDefault()
        downloadConfiguration.ensureCdnAwareDirectDownloadDefault()
    }

    /**
     * This class will issue a timer to write config to file 5 seconds after onEvent call. In case
     * this listener is called several times in a row the timer will get reset in order to ensure that
     * config is written only once.
     */
    private inner class TimerTaskListener : EventListener<ConfigurationEvent> {
        private fun launchWriterTaskLocked() {
            try {
                val scheduledTask = AtomicReference<ScheduledFuture<*>?>()
                val newFuture = TimerPool.schedule(
                    {
                        try {
                            withWriterLock {
                                if (future !== scheduledTask.get()) {
                                    return@withWriterLock
                                }
                                try {
                                    logger.trace("Writing app configuration file")
                                    handler.save()
                                } finally {
                                    if (future === scheduledTask.get()) {
                                        future = null
                                    }
                                }
                            }
                        } catch (configurationException: ConfigurationException) {
                            logger.error("writing app config file:", configurationException)
                        }
                    },
                    5.seconds,
                )
                scheduledTask.set(newFuture)
                future = newFuture
            } catch (ex: RejectedExecutionException) {
                logger.error("TimerPool: can't schedule timer task", ex)
                future = null
            }
        }

        override fun onEvent(configurationEvent: ConfigurationEvent) {
            if (!configurationEvent.isBeforeUpdate) {
                writerLock.withLock {
                    if (!timedEventWritingEnabled) {
                        return@withLock
                    }
                    cancelPendingWriterTask(mayInterruptIfRunning = false)
                    launchWriterTaskLocked()
                }
            }
        }
    }

    data class MainWindowBounds(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
    }

    data class BandwidthMonitorDialogState(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredBounds(): Boolean = width > 0 && height > 0 && x != Int.MIN_VALUE && y != Int.MIN_VALUE

        companion object {
            fun empty(): BandwidthMonitorDialogState =
                BandwidthMonitorDialogState(Int.MIN_VALUE, Int.MIN_VALUE, -1, -1)
        }
    }

    data class MemoryMonitorDialogState(
        val visible: Boolean,
        val x: Int,
        val y: Int,
        val width: Int,
        val height: Int,
    ) {
        fun visible(): Boolean = visible
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredBounds(): Boolean = width > 0 && height > 0 && x != Int.MIN_VALUE && y != Int.MIN_VALUE
    }

    data class FilmInfoDialogState(
        val visible: Boolean,
        val x: Int,
        val y: Int,
        val width: Int,
        val height: Int,
    ) {
        fun visible(): Boolean = visible
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredBounds(): Boolean = width > 50 && height > 50 && x != Int.MIN_VALUE && y != Int.MIN_VALUE
    }

    data class LoadFilmListDialogState(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredBounds(): Boolean = width >= 100 && height >= 100 && x != Int.MIN_VALUE && y != Int.MIN_VALUE

        companion object {
            fun empty(): LoadFilmListDialogState =
                LoadFilmListDialogState(Int.MIN_VALUE, Int.MIN_VALUE, -1, -1)
        }
    }

    data class SettingsDialogState(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredSize(): Boolean = width > 0 && height > 0

        companion object {
            fun empty(): SettingsDialogState = SettingsDialogState(0, 0, 0, 0)
        }
    }

    data class FilterDialogState(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredBounds(): Boolean =
            x != Int.MIN_VALUE && y != Int.MIN_VALUE && width != -1 && height != -1

        companion object {
            fun empty(): FilterDialogState = FilterDialogState(Int.MIN_VALUE, Int.MIN_VALUE, -1, -1)
        }
    }

    data class EditDownloadDialogState(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredLocation(): Boolean = x != Int.MIN_VALUE && y != Int.MIN_VALUE
        fun hasStoredSize(): Boolean = width != -1 && height != -1

        companion object {
            fun empty(): EditDownloadDialogState =
                EditDownloadDialogState(Int.MIN_VALUE, Int.MIN_VALUE, -1, -1)
        }
    }

    data class AddDownloadDialogPosition(val x: Int, val y: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun hasStoredPosition(): Boolean = x != Int.MIN_VALUE && y != Int.MIN_VALUE

        companion object {
            fun empty(): AddDownloadDialogPosition = AddDownloadDialogPosition(Int.MIN_VALUE, Int.MIN_VALUE)
        }
    }

    data class ManageAboDialogState(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredBounds(): Boolean =
            x != Int.MIN_VALUE && y != Int.MIN_VALUE && width != -1 && height != -1

        companion object {
            fun empty(): ManageAboDialogState = ManageAboDialogState(Int.MIN_VALUE, Int.MIN_VALUE, -1, -1)
        }
    }

    data class DuplicateFilmDetailsDialogState(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredBounds(): Boolean =
            x != Int.MIN_VALUE && y != Int.MIN_VALUE && width != -1 && height != -1

        companion object {
            fun empty(): DuplicateFilmDetailsDialogState =
                DuplicateFilmDetailsDialogState(Int.MIN_VALUE, Int.MIN_VALUE, -1, -1)
        }
    }

    data class DuplicateStatisticsDialogState(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredBounds(): Boolean =
            x != Int.MIN_VALUE && y != Int.MIN_VALUE && width != -1 && height != -1

        companion object {
            fun empty(): DuplicateStatisticsDialogState =
                DuplicateStatisticsDialogState(Int.MIN_VALUE, Int.MIN_VALUE, -1, -1)
        }
    }

    data class EditHistoryDialogState(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
        fun hasStoredBounds(): Boolean =
            x != Int.MIN_VALUE && y != Int.MIN_VALUE && width != -1 && height != -1

        companion object {
            fun empty(): EditHistoryDialogState = EditHistoryDialogState(Int.MIN_VALUE, Int.MIN_VALUE, -1, -1)
        }
    }

    data class BookmarkDialogBounds(val x: Int, val y: Int, val width: Int, val height: Int) {
        fun x(): Int = x
        fun y(): Int = y
        fun width(): Int = width
        fun height(): Int = height
    }

    data class DownloadToolbarState(
        val floating: Boolean,
        val x: Int,
        val y: Int,
        val orientation: Int,
    ) {
        fun floating(): Boolean = floating
        fun x(): Int = x
        fun y(): Int = y
        fun orientation(): Int = orientation
    }

    data class DefaultFontState(val family: String?, val size: Int) {
        fun family(): String? = family
        fun size(): Int = size
        fun hasStoredFont(): Boolean = family != null && size > 0

        companion object {
            fun empty(): DefaultFontState = DefaultFontState(null, -1)
        }
    }

    companion object {
        private val logger = LogManager.getLogger()

        @field:ApplicationConfigKey
        private const val CONFIG_MAJOR = "config.major"

        @field:ApplicationConfigKey
        private const val CONFIG_MINOR = "config.minor"

        @field:ApplicationConfigKey
        private const val CONFIG_PATCH = "config.patch"

        @JvmStatic
        fun getInstance(): ApplicationConfiguration = ConfigHolder.INSTANCE

        private fun createXmlConfiguration(): XMLConfiguration =
            XMLConfiguration().apply {
                conversionHandler = CustomConversionHandler()
                synchronizer = ReadWriteSynchronizer()
                rootElementName = "settings"
                isThrowExceptionOnMissing = true
            }

    }

    private object ConfigHolder {
        val INSTANCE: ApplicationConfiguration = ApplicationConfiguration()
    }
}
