package mediathek.gui.actions.export

import mediathek.config.Konstanten
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FileDialogs.Companion.chooseSaveFileLocation
import java.awt.event.ActionEvent
import java.beans.PropertyChangeEvent
import javax.swing.AbstractAction
import javax.swing.JOptionPane
import javax.swing.ProgressMonitor

/**
 * Exports the current film list to JSON file.
 */
class ExportReadableFilmlistAction : AbstractAction() {
    init {
        putValue(NAME, "Lesbare Filmliste...")
    }

    override fun actionPerformed(e: ActionEvent) {
        isEnabled = false
        val monitor = ProgressMonitor(MediathekGui.ui(), "Exportiere Filmliste", "", 0, 100)
        monitor.millisToPopup = 100
        monitor.millisToDecideToPopup = 100
        val selectedFile = chooseSaveFileLocation(MediathekGui.ui(), "Lesbare Filmliste sichern", "")
        if (selectedFile != null) {
            val worker = FilmlistExportWorker(this, selectedFile, compressSender = true, compressThema = true)
            worker.addPropertyChangeListener { evt: PropertyChangeEvent ->
                if ("progress" == evt.propertyName) {
                    val progress = evt.newValue as Int
                    monitor.setProgress(progress)
                }
            }
            worker.execute()
        } else {
            JOptionPane.showMessageDialog(MediathekGui.ui(),
                                          "Export wurde abgebrochen",
                                          Konstanten.PROGRAMMNAME,
                                          JOptionPane.WARNING_MESSAGE)
            isEnabled = true
        }
    }
}