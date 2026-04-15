package mediathek.gui.actions.export

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FileDialogs.chooseSaveFileLocation
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JOptionPane
import javax.swing.ProgressMonitor

abstract class AbstractExportFilmlistAction(
    actionName: String,
    private val saveDialogTitle: String,
    private val exportSettings: FilmlistExportSettings
) : AbstractAction(actionName) {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)

    override fun actionPerformed(e: ActionEvent) {
        isEnabled = false
        val monitor = createProgressMonitor()
        val selectedFile = chooseSaveFileLocation(MediathekGui.ui(), saveDialogTitle, "")

        when {
            selectedFile == null -> {
                showCancelled()
                isEnabled = true
            }

            !DiskSpaceUtil.enoughDiskSpace(selectedFile) -> {
                showInsufficientDiskSpace()
                isEnabled = true
            }

            else -> {
                FilmlistExportWorker(
                    selectedFile = selectedFile,
                    exportSettings = exportSettings,
                    uiScope = uiScope,
                    onProgress = monitor::setProgress,
                    onCompletion = ::handleCompletion
                ).execute()
            }
        }
    }

    private fun createProgressMonitor() = ProgressMonitor(MediathekGui.ui(), "Exportiere Filmliste", "", 0, 100).apply {
        millisToPopup = 100
        millisToDecideToPopup = 100
    }

    private fun handleCompletion(success: Boolean) {
        if (success) {
            JOptionPane.showMessageDialog(
                MediathekGui.ui(),
                "Der Export wurde erfolgreich abgeschlossen.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE
            )
        } else {
            JOptionPane.showMessageDialog(
                MediathekGui.ui(),
                "Es gab einen Fehler beim Export der Filmliste.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE
            )
        }
        isEnabled = true
    }

    private fun showCancelled() {
        JOptionPane.showMessageDialog(
            MediathekGui.ui(),
            "Der Export wurde abgebrochen.",
            Konstanten.PROGRAMMNAME,
            JOptionPane.WARNING_MESSAGE
        )
    }

    private fun showInsufficientDiskSpace() {
        JOptionPane.showMessageDialog(
            MediathekGui.ui(),
            "Nicht genügend freier Speicher auf dem gewählten Laufwerk.\nVorgang wurde abgebrochen.",
            Konstanten.PROGRAMMNAME,
            JOptionPane.ERROR_MESSAGE
        )
    }
}
