package mediathek.mainwindow

import mediathek.audiothek.ui.main.AudiothekPanel
import mediathek.config.CommandLineOptions
import mediathek.config.Daten
import mediathek.config.SettingsResetService
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.history.SeenHistoryController
import mediathek.gui.actions.ManageAboAction
import mediathek.gui.actions.MemoryMonitorAction
import mediathek.gui.actions.ShowBandwidthUsageAction
import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.gui.tabs.tab_film.GuiFilme
import mediathek.shutdown.ComputerShutdown
import mediathek.tool.RuntimeStatistics
import java.awt.Cursor

class MainWindowShutdownCoordinator(
    private val owner: MediathekGui,
    private val daten: Daten,
    private val showMemoryMonitorAction: MemoryMonitorAction,
    private val showBandwidthUsageAction: ShowBandwidthUsageAction,
    private val manageAboAction: ManageAboAction,
    private val tabFilme: GuiFilme,
    private val tabDownloads: GuiDownloads,
    private val tabAudiothek: AudiothekPanel,
    private val computerShutdown: ComputerShutdown,
    private val resetSettingsOnQuit: Boolean,
    private val closeAutomaticFilmlistUpdate: Runnable,
    private val closeProgramUpdateChecker: Runnable,
    private val closeSystemTray: Runnable,
    private val closeNotificationCenter: Runnable,
    private val shutdownTimerPool: Runnable,
    private val waitForCommonPoolToComplete: Runnable,
    private val edtRunner: ShutdownEdtRunner,
) {
    fun shutdown(shutdownComputer: Boolean) {
        runOnEdt("Show shutdown wait cursor") {
            owner.cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
        }

        try {
            shutdownSteps().shutdown()
        } finally {
            runOnEdt("Restore default cursor") {
                owner.cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
            }
        }

        if (shutdownComputer) {
            runInBackground("Request computer shutdown", computerShutdown::requestShutdown)
        }
    }

    private fun shutdownSteps(): ShutdownCoordinator =
        ShutdownCoordinator(edtRunner)
            .background("Close automatic filmlist update", closeAutomaticFilmlistUpdate)
            .background("Close program update checker", closeProgramUpdateChecker)
            .edt("Close memory monitor", showMemoryMonitorAction::closeMemoryMonitor)
            .edt("Close bandwidth monitor", ::closeBandwidthMonitor)
            .edt("Close abo dialog", manageAboAction::closeDialog)
            .background("Perform history maintenance", ::performHistoryMaintenance)
            .background("Save bookmark list") { daten.listeBookmarkList.saveToFile() }
            .background("Stop starter thread") { daten.downloadStartCoordinator.shutdown() }
            .edt("Close system tray", closeSystemTray)
            .background("Close notification center", closeNotificationCenter)
            .edt("Save tab Filme data", tabFilme::disposePanel)
            .edt("Save tab Download data", tabDownloads::tabelleSpeichern)
            .edt("Dispose tab Audiothek", tabAudiothek::disposePanel)
            .background("Stop all downloads") { daten.listeDownloads.requestStopForShutdown() }
            .background("Save app data", daten::allesSpeichern)
            .background("Close seen history database", SeenHistoryController::closeSharedStore)
            .edt("Close main window", owner::dispose)
            .background("Write app config") { ApplicationConfiguration.getInstance().writeConfiguration() }
            .background("Shutdown timer pool", shutdownTimerPool)
            .background("Wait for common pool", waitForCommonPoolToComplete)
            .apply {
                if (resetSettingsOnQuit) {
                    background("Move settings directory aside for reset", SettingsResetService::moveSettingsDirectoryAside)
                }
            }
            .background("Print runtime statistics", ::printRuntimeStatistics)

    private fun closeBandwidthMonitor() {
        showBandwidthUsageAction.dialogOptional.ifPresent { dialog ->
            dialog.dispose()
            // Preserve the visible state because it was open when the app quit.
            ApplicationConfiguration.getInstance().bandwidthMonitorVisible = true
        }
    }

    private fun performHistoryMaintenance() {
        SeenHistoryController().use { history ->
            history.performMaintenance()
        }
    }

    private fun printRuntimeStatistics() {
        RuntimeStatistics.printRuntimeStatistics()
        if (CommandLineOptions.isEnhancedLoggingEnabled()) {
            RuntimeStatistics.printDataUsageStatistics()
        }
    }

    private fun ShutdownCoordinator.edt(description: String, action: Runnable): ShutdownCoordinator =
        register(ShutdownStep(description, ShutdownThread.EDT, action))

    private fun ShutdownCoordinator.edt(description: String, action: () -> Unit): ShutdownCoordinator =
        edt(description, Runnable(action))

    private fun ShutdownCoordinator.background(description: String, action: Runnable): ShutdownCoordinator =
        register(ShutdownStep(description, ShutdownThread.BACKGROUND, action))

    private fun ShutdownCoordinator.background(description: String, action: () -> Unit): ShutdownCoordinator =
        background(description, Runnable(action))

    private fun runOnEdt(description: String, action: () -> Unit) {
        ShutdownCoordinator(edtRunner)
            .edt(description, action)
            .shutdown()
    }

    private fun runInBackground(description: String, action: Runnable) {
        ShutdownCoordinator(edtRunner)
            .background(description, action)
            .shutdown()
    }
}
