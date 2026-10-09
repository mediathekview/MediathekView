package mediathek.gui.actions.export

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.daten.ListeFilme
import mediathek.tool.FileDialogs.chooseSaveFileLocation
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.ProgressMonitor

abstract class AbstractExportFilmlistAction(
    actionName: String,
    private val films: ListeFilme,
    private val saveDialogTitle: String,
    private val exportSettings: FilmlistExportSettings,
    private val parent: JFrame,
) : AbstractAction(actionName) {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)

    override fun actionPerformed(e: ActionEvent) {
        isEnabled = false
        val monitor = createProgressMonitor()
        val selectedFile = chooseSaveFileLocation(parent, saveDialogTitle, "")

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
                    films = films,
                    selectedFile = selectedFile,
                    exportSettings = exportSettings,
                    uiScope = uiScope,
                    onProgress = monitor::setProgress,
                    onCompletion = ::handleCompletion
                ).execute()
            }
        }
    }

    private fun createProgressMonitor() = ProgressMonitor(parent, "Exportiere Filmliste", "", 0, 100).apply {
        millisToPopup = 100
        millisToDecideToPopup = 100
    }

    private fun handleCompletion(success: Boolean) {
        if (success) {
            JOptionPane.showMessageDialog(
                parent,
                "Der Export wurde erfolgreich abgeschlossen.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE
            )
        } else {
            JOptionPane.showMessageDialog(
                parent,
                "Es gab einen Fehler beim Export der Filmliste.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE
            )
        }
        isEnabled = true
    }

    private fun showCancelled() {
        JOptionPane.showMessageDialog(
            parent,
            "Der Export wurde abgebrochen.",
            Konstanten.PROGRAMMNAME,
            JOptionPane.WARNING_MESSAGE
        )
    }

    private fun showInsufficientDiskSpace() {
        JOptionPane.showMessageDialog(
            parent,
            "Nicht genügend freier Speicher auf dem gewählten Laufwerk.\nVorgang wurde abgebrochen.",
            Konstanten.PROGRAMMNAME,
            JOptionPane.ERROR_MESSAGE
        )
    }
}
