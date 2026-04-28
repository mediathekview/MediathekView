/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek

import com.formdev.flatlaf.FlatLaf
import com.jidesoft.utils.ThreadCheckingRepaintManager
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
import mediathek.cli.CliShutdownSignal
import mediathek.cli.DownloadAndQuitRunner
import mediathek.config.*
import mediathek.controller.SenderFilmlistLoadApprover
import mediathek.controller.history.SeenHistoryMigrator
import mediathek.daten.IndexedFilmList
import mediathek.gui.dialog.DialogStarteinstellungen
import mediathek.gui.tabs.tab_film.filter.FilmLengthSlider
import mediathek.logging.SwingAppender
import mediathek.mac.MediathekGuiMac
import mediathek.mainwindow.MediathekGui
import mediathek.tool.*
import mediathek.tool.affinity.Affinity
import mediathek.tool.dns.IPvPreferenceMode
import mediathek.tool.migrator.SettingsMigrator
import mediathek.windows.MediathekGuiWindows
import mediathek.windows.WindowsVersionHelper
import mediathek.x11.MediathekGuiX11
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import org.apache.logging.log4j.core.Filter
import org.apache.logging.log4j.core.LoggerContext
import org.apache.logging.log4j.core.appender.AsyncAppender
import org.apache.logging.log4j.core.appender.ConsoleAppender
import org.apache.logging.log4j.core.appender.FileAppender
import org.apache.logging.log4j.core.config.AppenderRef
import org.apache.logging.log4j.core.filter.ThresholdFilter
import org.apache.logging.log4j.core.layout.PatternLayout
import picocli.CommandLine
import java.awt.GraphicsEnvironment
import java.awt.Taskbar
import java.awt.image.BufferedImage
import java.io.File
import java.io.IOException
import java.lang.management.ManagementFactory
import java.net.URL
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.Security
import java.time.format.DateTimeFormatter
import javax.imageio.ImageIO
import javax.swing.*
import kotlin.system.exitProcess

object Main {
    private const val LOG4J_SHUTDOWN_CALLBACK_REGISTRY = "mediathek.tool.Log4jShutdownCallbackRegistry"
    private const val MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR = "apple.laf.useScreenMenuBar"
    private val logger: Logger = LogManager.getLogger(Main::class.java)

    private var singleInstanceWatcher: SingleInstance? = null

    init {
        System.setProperty("log4j.shutdownCallbackRegistry", LOG4J_SHUTDOWN_CALLBACK_REGISTRY)
    }

    @JvmStatic
    fun main(args: Array<String>) = runBlocking {
        setupEnvironmentProperties()

        val parseResult = parseCommandLine(args)
        if (GraphicsEnvironment.isHeadless() && !Config.isDownloadAndQuit()) {
            System.err.println("Diese Version von MediathekView unterstützt keine Kommandozeilenausführung.")
            exitProcess(1)
        }
        configureStartup(parseResult, args)
        printDirectoryPaths()

        if (Config.isDownloadAndQuit()) {
            CliShutdownSignal.install(DownloadAndQuitRunner::requestShutdown).use {
                installSingleInstanceHandler(false)
                performBackgroundStartup(cleanupMediaDb = !GraphicsEnvironment.isHeadless())
                loadConfigurationDataCli()
                migrateSeenHistory()
                Daten.getInstance().launchHistoryDataLoading()
                Daten.getInstance().waitForHistoryDataLoadingToComplete()
                withContext(Dispatchers.IO) {
                    Daten.getInstance().listeBookmarkList.loadFromFile()
                }
                val exitCode = try {
                    DownloadAndQuitRunner.run()
                } finally {
                    Daten.getInstance().starterClass.shutdown()
                    ApplicationConfiguration.getInstance().writeConfiguration()
                }
                exitProcess(exitCode)
            }
        }

        initializeSwingEnvironment()
        showSplashScreenIfEnabled()

        performBackgroundStartup(cleanupMediaDb = true)

        loadConfigurationData()
        activateNewSenders()
        activateNewMaxFilmLength()

        migrateSeenHistory()
        Daten.getInstance().launchHistoryDataLoading()
        withContext(Dispatchers.IO) {
            Daten.getInstance().listeBookmarkList.loadFromFile()
            removeLuceneIndexDirectory()
        }

        // enable modern search on demand
        val useModernSearch = ApplicationConfiguration.getConfiguration()
            .getBoolean(ApplicationConfiguration.APPLICATION_USE_MODERN_SEARCH, false)
        if (useModernSearch) {
            Daten.getInstance().listeFilmeNachBlackList = IndexedFilmList()
        }

        startGuiMode()
    }

    private suspend fun parseCommandLine(args: Array<String>): CommandLine.ParseResult {
        val cmd = CommandLine(Config::class.java)

        try {
            val parseResult = cmd.parseArgs(*args)
            if (parseResult.isUsageHelpRequested) {
                cmd.usage(System.out)
                exitProcess(cmd.commandSpec.exitCodeOnUsageHelp())
            }

            Config.setPortableMode(parseResult.hasMatchedPositional(0))
            if (Config.isPortableMode()) {
                StandardLocations.portableBaseDirectory = Config.baseFilePath
            }

            return parseResult
        } catch (ex: CommandLine.ParameterException) {
            cmd.err.use { err ->
                val errStr = ex.message + "\n\n" + ex.commandLine.usageMessage
                if (GraphicsEnvironment.isHeadless()) {
                    err.println(errStr)
                } else {
                    withContext(Dispatchers.Swing) {
                        JOptionPane.showMessageDialog(
                            null,
                            errStr,
                            "Fehlerhafte Kommandozeilenparameter",
                            JOptionPane.ERROR_MESSAGE
                        )
                    }
                }
                err.println(ex.message)
                if (!CommandLine.UnmatchedArgumentException.printSuggestions(ex, err)) {
                    ex.commandLine.usage(err)
                }
                exitProcess(cmd.commandSpec.exitCodeOnInvalidInput())
            }
        } catch (ex: Exception) {
            logger.error("Command line parse error:", ex)
            exitProcess(cmd.commandSpec.exitCodeOnExecutionException())
        }
    }

    private fun configureStartup(parseResult: CommandLine.ParseResult, args: Array<String>) {
        setupLogging()

        val level = when {
            Config.isEnhancedLoggingEnabled() && Config.isDebugModeEnabled() -> Level.TRACE
            Config.isEnhancedLoggingEnabled() -> Level.DEBUG
            else -> Level.INFO
        }
        registerSwingAppender(level)

        printPortableModeInfo()
        configureDnsPreferenceMode(parseResult)
        setupCpuAffinity()
        printVersionInformation()
        printJvmParameters()
        printArguments(args)
    }

    private suspend fun initializeSwingEnvironment() = withContext(Dispatchers.Swing) {
        if (SystemUtils.IS_OS_LINUX && !Config.isDisableFlatLafDecorations()) {
            // enable custom window decorations
            JFrame.setDefaultLookAndFeelDecorated(true)
            JDialog.setDefaultLookAndFeelDecorated(true)
        }

        setupDockIcon()

        UIManager.put("TabbedPane.showTabSeparators", true)
        registerFlatLafCustomization()
        DarkModeSetup.setup()

        if (SystemUtils.IS_OS_LINUX) {
            checkUiScaleSetting()
        }

        if (!Config.isDisableJvmParameterChecks()) {
            checkJVMSettings()
        }

        if (SystemUtils.IS_OS_WINDOWS) {
            checkWindows10OrGreater()
        }

        installSingleInstanceHandler()
    }

    private suspend fun showSplashScreenIfEnabled() = withContext(Dispatchers.Swing) {
        val splashScreen = when {
            isDebuggerAttached() -> {
                logger.warn("Debugger detected -> Splash screen disabled...")
                null
            }

            Config.isSplashScreenDisabled() -> {
                logger.warn("Splash screen disabled...")
                null
            }

            else -> SplashScreen()
        }

        SplashScreenLifecycle.set(splashScreen)
        SplashScreenLifecycle.show()
    }

    private suspend fun performBackgroundStartup(cleanupMediaDb: Boolean) = withContext(Dispatchers.IO) {
        migrateOldConfigSettings()
        if (cleanupMediaDb) {
            removeMediaDb()
        }
        deleteOldFilmDatabaseFiles()
        deleteOldUserAgentsDatabase()
    }

    private fun loadConfigurationDataCli() {
        if (!Daten.getInstance().allesLaden()) {
            logger.error("CLI download mode requires an existing valid configuration and does not support interactive setup or repair.")
            exitProcess(1)
        }
    }

    /**
     * Ensures that old film lists in .mediathek directory get deleted because they were moved to
     * ~/Library/Caches/MediathekView
     * In portable mode we MUST NOT delete the files.
     */
    private fun cleanupOsxFiles() {
        if (!Config.isPortableMode()) {
            try {
                val oldFilmList = StandardLocations.getSettingsDirectory().resolve(Konstanten.JSON_DATEI_FILME)
                Files.deleteIfExists(oldFilmList)
            } catch (_: IOException) {
            }
        }
    }

    /**
     * Remove the old and now unsupported mediafile to trash.
     * CAUTION: At least some UI MUST BE INITILIZED, otherwise on macOS VM will crash in native code!!!!
     */
    private fun removeMediaDb() {
        val mediaDbPath = StandardLocations.getSettingsDirectory().resolve("mediadb.txt")
        if (Files.exists(mediaDbPath)) {
            logger.info("Moving old unsupported media database to trash.")
            FileUtils.moveToTrash(mediaDbPath)
        }
    }

    private fun printJvmParameters() {
        logger.debug("=== JavaVM Parameter ===")
        val runtimeMXBean = ManagementFactory.getRuntimeMXBean()
        val jvmArgs = runtimeMXBean.inputArguments
        for (arg in jvmArgs) {
            logger.debug(arg)
        }
        logger.debug("========================")
    }

    private fun printArguments(arguments: Array<String>) {
        for (argument in arguments) {
            logger.info("Startparameter: {}", argument)
        }
    }

    private fun registerSwingAppender(minLevel: Level) {
        val appenderName = "SwingAppender"

        val ctx = LogManager.getContext(false) as LoggerContext
        val config = ctx.configuration

        if (config.appenders.containsKey(appenderName)) {
            return
        }

        val appender = SwingAppender.createAppender(appenderName, null)
        appender.start()
        config.addAppender(appender)

        // Attach to root logger
        val rootLogger = config.rootLogger
        rootLogger.addAppender(appender, minLevel, null)

        ctx.updateLoggers()
    }

    private fun setupLogging() {
        val loggerContext = LogManager.getContext(false) as LoggerContext
        val config = loggerContext.configuration
        val fileName = "/mediathekview.log"
        val path = if (!Config.isPortableMode()) {
            "${StandardLocations.getSettingsDirectory()}$fileName"
        } else {
            "${Config.baseFilePath}$fileName"
        }

        val consolePattern = if (Config.isEnhancedLoggingEnabled() || Config.isDebugModeEnabled()) {
            PatternLayout.newBuilder().withPattern("[%-5level] [%t] %c - %msg%n").build()
        } else {
            PatternLayout.newBuilder().withPattern(". %msg%n").build()
        }

        val consoleAppender = ConsoleAppender.createDefaultAppenderForLayout(consolePattern)
        //for normal users only show INFO and higher messages
        if (!Config.isEnhancedLoggingEnabled() && !Config.isDebugModeEnabled()) {
            val thresholdFilter = ThresholdFilter.createFilter(Level.INFO, Filter.Result.ACCEPT, Filter.Result.DENY)
            consoleAppender.addFilter(thresholdFilter)
        }
        consoleAppender.start()

        val fileAppenderBuilder = FileAppender.newBuilder()
            .setName("LogFile")
            .withAppend(false)
            .withFileName(path)
            .setLayout(PatternLayout.newBuilder().withPattern("%-5p %d  [%t] %C{2} (%F:%L) - %m%n").build())
            .setConfiguration(config)

        //regular users may have DEBUG output in log file but not TRACE
        if (!Config.isEnhancedLoggingEnabled() && !Config.isDebugModeEnabled()) {
            val thresholdFilter = ThresholdFilter.createFilter(Level.DEBUG, Filter.Result.ACCEPT, Filter.Result.DENY)
            fileAppenderBuilder.setFilter(thresholdFilter)
        }

        var asyncAppender: AsyncAppender? = null
        if (!Config.isFileLoggingDisabled()) {
            val fileAppender = fileAppenderBuilder.build()
            fileAppender.start()
            config.addAppender(fileAppender)

            asyncAppender = AsyncAppender.newBuilder()
                .setName("Async")
                .setAppenderRefs(arrayOf(AppenderRef.createAppenderRef(fileAppender.name, null, null)))
                .setConfiguration(config)
                .setIncludeLocation(true)
                .setBlocking(false)
                .build()

            asyncAppender.start()
            config.addAppender(asyncAppender)
        }

        val rootLogger = loggerContext.rootLogger
        rootLogger.level = Level.TRACE
        rootLogger.addAppender(consoleAppender)
        if (!Config.isFileLoggingDisabled()) {
            rootLogger.addAppender(asyncAppender)
        }

        loggerContext.updateLoggers()
    }

    private fun setupEnvironmentProperties() {
        System.setProperty("file.encoding", "UTF-8")

        //enable full strength crypto if not already done
        Security.setProperty("crypto.policy", "unlimited")

        if (SystemUtils.IS_OS_MAC_OSX) {
            System.setProperty("apple.awt.application.name", Konstanten.PROGRAMMNAME)
            System.setProperty("apple.awt.application.appearance", "system")
        }
    }

    private fun printVersionInformation() {
        val buildInfo = BuildInfo.current()
        logger.info("Programmstart: {}", DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(RuntimeStatistics.startZeit))
        logger.info("Version: {}", Konstanten.MVVERSION)
        logger.info("Build Git: {}", buildInfo.formatForDisplay())

        logger.info("=== Java Information ===")

        logger.info("Vendor: {}", SystemUtils.JAVA_VENDOR)
        logger.info("VMname: {}", SystemUtils.JAVA_VM_NAME)
        logger.info("Version: {}", SystemUtils.JAVA_VERSION)
        logger.info("Runtime Version: {}", SystemUtils.JAVA_RUNTIME_VERSION)
        val runtime = Runtime.getRuntime()
        logger.info("Maximum Memory: {} MB", runtime.maxMemory() / FileUtils.ONE_MB)

        logger.info("Operating System: {}", SystemUtils.OS_NAME)
        logger.info("OS Version: {}", SystemUtils.OS_VERSION)
        logger.info("OS Arch: {}", SystemUtils.OS_ARCH)
        if (DarkModeDetector.hasDarkModeDetectionSupport()) {
            logger.info("OS Dark Mode enabled: {}", DarkModeDetector.isDarkMode())
        } else {
            logger.info("OS Dark Mode detection not supported")
        }
        logger.info("OS Available Processors: {}", runtime.availableProcessors())
    }

    /**
     * Migrate old settings stored in mediathek.xml to new app config
     */
    private fun migrateOldConfigSettings() {
        val settingsDir = StandardLocations.getSettingsDirectory().toString()
        if (settingsDir.isNotEmpty()) {
            val settingsDirectoryPath = Paths.get(settingsDir)
            if (Files.exists(settingsDirectoryPath)) {
                //convert existing settings
                val settingsFile = settingsDirectoryPath.resolve(Konstanten.CONFIG_FILE)
                if (Files.exists(settingsFile)) {
                    logger.trace("migrating old config settings {}", settingsFile.toAbsolutePath().toString())
                    try {
                        val migrator = SettingsMigrator(settingsFile)
                        migrator.migrate()
                    } catch (e: Exception) {
                        logger.error("settings migration error", e)
                    }
                }
            } else {
                logger.trace("nothing to migrate")
            }
        }
    }

    private fun printPortableModeInfo() {
        if (Config.isPortableMode()) {
            logger.info("Configuring baseFilePath {} for portable mode", Config.baseFilePath)
        } else {
            logger.info("Configuring for non-portable mode")
        }
    }

    private fun setupCpuAffinity() {
        try {
            val numCpus = Config.getNumCpus()
            if (numCpus != 0) {
                val affinity = Affinity.affinityImpl
                affinity.setDesiredCpuAffinity(numCpus)
            }
        } catch (e: Exception) {
            logger.error("Failed to set cpu affinity", e)
        }
    }

    /**
     * Install dock icon when supported.
     */
    private fun setupDockIcon() {
        try {
            if (Taskbar.isTaskbarSupported()) {
                val taskbar = Taskbar.getTaskbar()
                if (taskbar.isSupported(Taskbar.Feature.ICON_IMAGE)) {
                    val url: URL? = Main::class.java.getResource("/mediathek/res/MediathekView.png")
                    if (url != null) {
                        val appImage: BufferedImage = ImageIO.read(url)
                        Taskbar.getTaskbar().iconImage = appImage
                    }
                }
            }
        } catch (ex: IOException) {
            logger.error("OS X Application image could not be loaded", ex)
        }
    }

    /**
     * Check if Shenandoah GC settings are supplied to JVM.
     * Otherwise display warning dialog.
     */
    private fun checkJVMSettings() {
        val runtimeMXBean = ManagementFactory.getRuntimeMXBean()
        val paramList = runtimeMXBean.inputArguments

        if (!JvmSettingsValidator.hasRequiredJvmSettings(paramList)) {
            logger.warn("Detected incorrect JVM parameters! Please modify your settings")
            if (!Config.isDebugModeEnabled()) {
                //show error dialog
                JOptionPane.showMessageDialog(
                    null,
                    JvmSettingsValidator.getErrorMessageString(),
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.WARNING_MESSAGE
                )
            }
        }
    }

    /**
     * Check if a non-floating point scale factor is set on Linux.
     * Java 18 VM does not support fractional scaling.
     */
    private fun checkUiScaleSetting() {
        val strScale = System.getProperty("sun.java2d.uiScale")
        if (strScale != null) {
            try {
                strScale.toInt()
            } catch (_: NumberFormatException) {
                // not an int -> show warning
                // fractional scale is NOT supported under Linux, must use integer only.
                val scaleFactor = strScale.toFloat()
                logger.trace("old uiScale factor {}", scaleFactor)
                val newScale = scaleFactor.toInt()
                logger.trace("new uiScale factor {}", newScale)

                JOptionPane.showMessageDialog(
                    null,
                    "<html>" +
                        "Sie verwenden den Parameter <i>-Dsun.java2d.uiScale=$strScale</i>.<br>" +
                        "<b>Java unter Linux unterstützt nur ganzzahlige Skalierung!</b><br><br>" +
                        "Sie sollten <i>-Dsun.java2d.uiScale=$newScale</i> oder größer verwenden falls die Schriftgröße zu klein ist.",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.WARNING_MESSAGE
                )
            }
        }
    }

    private fun configureDnsPreferenceMode(parseResult: CommandLine.ParseResult) {
        val config = ApplicationConfiguration.getConfiguration()
        if (parseResult.hasMatchedOption("dpm")) {
            logger.trace("Dns preference mode set via CLI, storing config value")
            config.setProperty(
                ApplicationConfiguration.APPLICATION_NETWORKING_DNS_MODE,
                Config.getDnsIpPreferenceMode().toString()
            )
        } else {
            logger.trace("Dns preference mode NOT set, using config setting")
            val mode = IPvPreferenceMode.fromString(
                config.getString(
                    ApplicationConfiguration.APPLICATION_NETWORKING_DNS_MODE,
                    Config.getDnsIpPreferenceMode().toString()
                )
            )
            Config.setDnsIpPreferenceMode(mode)
        }
        logger.trace("Setting DNS selector to mode: {}", Config.getDnsIpPreferenceMode().toString())
    }

    private fun registerFlatLafCustomization() {
        if (!SystemUtils.IS_OS_MAC_OSX) {
            val settings = StandardLocations.getSettingsDirectory().resolve("flatlaf")
            logger.info("Registering {} as custom FlatLaf config folder", settings)
            FlatLaf.registerCustomDefaultsSource(settings.toFile())
        }
    }

    private fun checkWindows10OrGreater() {
        try {
            if (!WindowsVersionHelper.IsWindows10OrGreater()) {
                JOptionPane.showMessageDialog(
                    null,
                    "<html>MediathekView benötigt mindestens Windows 10 zum Start.<br/>" +
                        "<b>Die Nutzung erfolgt auf eigenes Risiko ohne Support!!</b><br/><br/>" +
                        "Die nächste MV-Version wird nicht mehr unter diesem Betriebssystem starten!</html>",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.WARNING_MESSAGE
                )
            }
        } catch (ex: Throwable) {
            logger.error("Error while checking Windows version", ex)
        }
    }

    private suspend fun activateNewMaxFilmLength() = withContext(Dispatchers.Swing) {
        val alreadyActivated = ApplicationConfiguration.getConfiguration()
            .getBoolean(Konstanten.NEW_FILMLENGTH_ACTIVATED_QUESTION_CONFIG_KEY, false)
        if (!alreadyActivated) {
            val filterConfig = FilterConfiguration()
            val activeFilter = filterConfig.currentFilter

            try {
                val filtersNeedingMigration = filterConfig.availableFilters.stream()
                    .filter { filter -> filterConfig.setCurrentFilter(filter).filmLengthMax == 120.0 }
                    .toList()

                if (filtersNeedingMigration.isEmpty()) {
                    ApplicationConfiguration.getConfiguration()
                        .setProperty(Konstanten.NEW_FILMLENGTH_ACTIVATED_QUESTION_CONFIG_KEY, true)
                    return@withContext
                }

                SplashScreenLifecycle.hide()
                val optionPane = JOptionPane(
                    "<html>Die maximale Filterlänge wurde <b>von 120 auf 240 Minuten</b> erhöht.<br/>" +
                        "Die Filter wurden damals nicht automatisch angepasst.<br/><br/>" +
                        "Soll MediathekView einmalig alle Filter anpassen?</html>",
                    JOptionPane.QUESTION_MESSAGE,
                    JOptionPane.YES_NO_OPTION
                )
                val dialog = optionPane.createDialog(Konstanten.PROGRAMMNAME)
                dialog.isAlwaysOnTop = true
                dialog.isModal = true
                dialog.isResizable = true
                dialog.defaultCloseOperation = JDialog.DISPOSE_ON_CLOSE
                dialog.isVisible = true
                val result = optionPane.value
                if (result != null) {
                    if (result as Int == JOptionPane.YES_OPTION) {
                        logger.info("Evaluating max film length for new maximum...")
                        for (filter in filtersNeedingMigration) {
                            val currentFilter = filterConfig.setCurrentFilter(filter)
                            logger.info(
                                "Patched max film length in filter: {}",
                                filterConfig.getFilterName(currentFilter.currentFilterID)
                            )
                            currentFilter.setFilmLengthMax(FilmLengthSlider.UNLIMITED_VALUE.toDouble())
                        }
                    }
                    ApplicationConfiguration.getConfiguration()
                        .setProperty(Konstanten.NEW_FILMLENGTH_ACTIVATED_QUESTION_CONFIG_KEY, true)
                }
            } finally {
                filterConfig.setCurrentFilter(activeFilter)
            }
            SplashScreenLifecycle.show()
        }
    }

    /**
     * Activate all senders when MediathekView adds additional ones.
     * For newer versions configKey must be adapted.
     */
    private suspend fun activateNewSenders() = withContext(Dispatchers.Swing) {
        val alreadyActivated = ApplicationConfiguration.getConfiguration()
            .getBoolean(Konstanten.NEW_SENDER_ACTIVATED_QUESTION_CONFIG_KEY, false)
        if (!alreadyActivated) {
            val hasNewSendersToActivate =
                !SenderFilmlistLoadApprover.senderSet.containsAll(SenderListBoxModel.providedSenderList)
            if (!hasNewSendersToActivate) {
                ApplicationConfiguration.getConfiguration()
                    .setProperty(Konstanten.NEW_SENDER_ACTIVATED_QUESTION_CONFIG_KEY, true)
                return@withContext
            }

            SplashScreenLifecycle.hide()
            val optionPane = JOptionPane(
                "<html>Diese Version unterstützt neue Sender, die in den Einstellungen aktiviert werden müssen.<br/>" +
                    "Soll MediathekView einmalig alle Sender aktivieren?</html>",
                JOptionPane.QUESTION_MESSAGE,
                JOptionPane.YES_NO_OPTION
            )
            val dialog = optionPane.createDialog(Konstanten.PROGRAMMNAME)
            dialog.isAlwaysOnTop = true
            dialog.isModal = true
            dialog.isResizable = true
            dialog.defaultCloseOperation = JDialog.DISPOSE_ON_CLOSE
            dialog.isVisible = true
            val result = optionPane.value
            if (result != null) {
                if (result as Int == JOptionPane.YES_OPTION) {
                    logger.info("Activating new senders...")
                    SenderFilmlistLoadApprover.approveAll()
                }
                ApplicationConfiguration.getConfiguration()
                    .setProperty(Konstanten.NEW_SENDER_ACTIVATED_QUESTION_CONFIG_KEY, true)
            }

            SplashScreenLifecycle.show()
        }
    }

    /**
     * Remove modern search index when not in use.
     */
    private fun removeLuceneIndexDirectory() {
        //when modern search is not in use, delete unused film index directory as a precaution
        val indexPath = StandardLocations.getFilmIndexPath()
        if (Files.exists(indexPath)) {
            try {
                FileUtils.deletePathRecursively(indexPath)
            } catch (e: IOException) {
                logger.error("Failed to remove Lucene index directory", e)
            }
        }
    }

    /**
     * Checks if the application has an debugger attached to it.
     *
     * @return true if debugger was detected, false othewise.
     */
    private fun isDebuggerAttached(): Boolean {
        return ManagementFactory.getRuntimeMXBean().inputArguments.toString().contains("-agentlib:jdwp")
    }

    /**
     * Migrate the old text file history to new database format
     */
    private suspend fun migrateSeenHistory() {
        try {
            withContext(Dispatchers.IO) {
                SeenHistoryMigrator().use { migrator ->
                    if (migrator.needsMigration()) {
                        migrator.migrate()
                    }
                }
            }
        } catch (e: Exception) {
            logger.error("migrateSeenHistory", e)
            SplashScreenLifecycle.close()
            if (Config.isDownloadAndQuit() || GraphicsEnvironment.isHeadless()) {
                logger.error("Die Migration der Historie ist fehlgeschlagen. Das Programm wird beendet.")
            } else {
                withContext(Dispatchers.Swing) {
                    SwingErrorDialog.showExceptionMessage(
                        null,
                        """
                            <html>Bei der Migration der Historie der Filme ist ein Fehler aufgetreten.<br>
                            Das Programm kann nicht fortfahren und wird beendet.<br><br>
                            Bitte überprüfen Sie die Fehlermeldung und suchen Sie Hilfe im Forum.</html>
                        """.trimIndent(),
                        e
                    )
                }
            }
            exitProcess(99)
        }
    }

    private suspend fun loadConfigurationData() = withContext(Dispatchers.Swing) {
        if (!Daten.getInstance().allesLaden()) {
            // erster Start
            ReplaceList.init() // einmal ein Muster anlegen, für Linux/OS X ist es bereits aktiv!
            SplashScreenLifecycle.close()

            val dialog = DialogStarteinstellungen(null)
            if (dialog.showDialog() == DialogStarteinstellungen.ResultCode.CANCELLED) {
                //show termination dialog
                JOptionPane.showMessageDialog(
                    null,
                    "<html>Sie haben die Einrichtung des Programms abgebrochen.<br>" +
                        "MediathekView muss deshalb beendet werden.</html>",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.ERROR_MESSAGE
                )

                deleteSettingsDirectory()
                exitProcess(1)
            }
            MVConfig.loadSystemParameter()
        }
    }

    private fun deleteOldUserAgentsDatabase() {
        try {
            val settingsPath = StandardLocations.getSettingsDirectory()
            val agentDb = settingsPath.resolve("user_agents.mv.db")
            Files.deleteIfExists(agentDb)
        } catch (e: IOException) {
            logger.error("Error deleting old user agent database occured", e)
        }
    }

    private fun deleteOldFilmDatabaseFiles() {
        val settingsPath = StandardLocations.getSettingsDirectory()
        val dbFolder = settingsPath.resolve("database")
        val traceFile = settingsPath.resolve("databasemediathekview.trace.db")

        if (!Files.exists(dbFolder)) {
            return
        }

        try {
            Files.walk(dbFolder).use { walk ->
                walk.sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    .forEach(File::delete)
            }

            Files.deleteIfExists(traceFile)
        } catch (ex: Exception) {
            logger.error("Got an error deleting old database directory", ex)
        }
    }

    @Suppress("ResultOfMethodCallIgnored")
    private fun deleteSettingsDirectory() {
        try {
            Files.walk(StandardLocations.getSettingsDirectory()).use { walk ->
                walk.sorted(Comparator.reverseOrder())
                    .map(Path::toFile)
                    .forEach(File::delete)
            }
        } catch (ex: Exception) {
            logger.error("Got an error deleting settings directory", ex)
        }
    }

    private fun printDirectoryPaths() {
        logger.trace("Programmpfad: {}", GuiFunktionenProgramme.getPathToApplicationJar())
        logger.info("Verzeichnis Einstellungen: {}", StandardLocations.getSettingsDirectory())
    }

    /**
     * Prevent startup of multiple instances of the app.
     */
    private fun installSingleInstanceHandler(showDialog: Boolean = true) {
        singleInstanceWatcher = SingleInstance()
        if (singleInstanceWatcher?.isAppAlreadyActive() == true) {
            val message = "Es dürfen nicht mehrere MediathekView-Instanzen gleichzeitig laufen.\n" +
                "Bitte beenden Sie zuerst das andere Programm."
            if (showDialog) {
                JOptionPane.showMessageDialog(
                    null,
                    message,
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.ERROR_MESSAGE
                )
            } else {
                logger.error(message)
            }
            exitProcess(1)
        }
    }

    private fun checkForOfficialOSXAppUse() {
        val osxOfficialApp = System.getProperty(Konstanten.MACOS_OFFICIAL_APP)
        if (osxOfficialApp.isNullOrEmpty() || osxOfficialApp.equals("false", ignoreCase = true)) {
            logger.warn("WARN: macOS app NOT launched from official launcher!")
        }
    }

    private suspend fun startGuiMode() = withContext(Dispatchers.Swing) {
        SplashScreenLifecycle.update(UIProgressState.INIT_FX)

        SplashScreenLifecycle.update(UIProgressState.FILE_CLEANUP)
        if (SystemUtils.IS_OS_MAC_OSX) {
            checkForOfficialOSXAppUse()
            System.setProperty(MAC_SYSTEM_PROPERTY_APPLE_LAF_USE_SCREEN_MENU_BAR, true.toString())
            cleanupOsxFiles()
        }

        if (Config.isDebugModeEnabled() || Config.isInstallThreadCheckingRepaintManager()) {
            // use for debugging EDT violations
            RepaintManager.setCurrentManager(ThreadCheckingRepaintManager())
            logger.debug("Swing Thread checking repaint manager installed.")
        }

        SplashScreenLifecycle.update(UIProgressState.START_UI)
        val window = getPlatformWindow()
        SplashScreenLifecycle.close()
        window.isVisible = true
        /*
            on windows and linux there is a strange behaviour that the main window gets sent behind
            other open windows after the splash screen is closed.
         */
        if (!SystemUtils.IS_OS_MAC_OSX) {
            window.toFront()
            window.requestFocusInWindow()
        }
    }

    private fun getPlatformWindow(): MediathekGui {
        return when {
            SystemUtils.IS_OS_MAC_OSX -> MediathekGuiMac()
            SystemUtils.IS_OS_WINDOWS -> MediathekGuiWindows()
            SystemUtils.IS_OS_LINUX -> MediathekGuiX11()
            else -> {
                JOptionPane.showMessageDialog(
                    null,
                    """
                        Sie führen MediathekView auf einem nicht unterstützten Betriebssystem aus.
                        Es werden nur macOS, Windows und Linux unterstützt.

                        Das Programm wird beendet, da die Funktionsfähigkeit nicht gewährleistet werden kann.
                    """.trimIndent(),
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.ERROR_MESSAGE
                )
                exitProcess(2)
            }
        }
    }

    private object DarkModeSetup {
        private fun getCurrentLookAndFeel(darkMode: Boolean): LookAndFeel {
            return if (darkMode) {
                DarkModeFactory.lookAndFeel
            } else {
                LightModeFactory.lookAndFeel
            }
        }

        private fun setupFlatLaf() {
            val darkMode = ApplicationConfiguration.getConfiguration()
                .getBoolean(ApplicationConfiguration.APPLICATION_DARK_MODE, false)
            FlatLaf.setup(getCurrentLookAndFeel(darkMode))
        }

        fun setup() {
            if (DarkModeDetector.hasDarkModeDetectionSupport()) {
                logger.trace("setting up dark mode system laf")
                val useSystemMode = ApplicationConfiguration
                    .getConfiguration()
                    .getBoolean(ApplicationConfiguration.APPLICATION_USE_SYSTEM_DARK_MODE, false)
                if (useSystemMode) {
                    FlatLaf.setup(getCurrentLookAndFeel(DarkModeDetector.isDarkMode()))
                } else {
                    setupFlatLaf()
                }
            } else {
                logger.trace("dark mode detection not supported, using config")
                setupFlatLaf()
            }
        }
    }
}
