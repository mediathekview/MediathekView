package mediathek.gui.actions.import_actions

import mediathek.config.Konstanten
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FileDialogs.Companion.chooseLoadFileLocation
import mediathek.tool.SwingErrorDialog
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JOptionPane

class ImportOldAbosAction : AbstractAction() {
    init {
        putValue(NAME, "Alte Abos...")
        putValue(SHORT_DESCRIPTION, "Ermöglicht den Import der Abos aus einer alten Konfigurationsdatei.")
    }

    override fun actionPerformed(e: ActionEvent) {
        val selectedFile = chooseLoadFileLocation(MediathekGui.ui(), " Konfigurationsdatei öffnen", "")
        if (selectedFile != null) {
            try {
                val configReader = OldConfigFileImporter()
                val (foundAbos) = configReader.importAboBlacklist(selectedFile.absolutePath,
                                                                  importAbo = true,
                                                                  importBlacklist = false,
                                                                  importReplaceList = false)
                val text = "Es wurden $foundAbos Einträge importiert."
                JOptionPane.showMessageDialog(MediathekGui.ui(), text, Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE)
            }
            catch (ex: Exception) {
                val text = """
                    Es trat ein Fehler beim Import der Abos auf.
                    Sollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.
                    """.trimIndent()
                SwingErrorDialog.showExceptionMessage(MediathekGui.ui(), text, ex)
            }
        } else {
            JOptionPane.showMessageDialog(MediathekGui.ui(), "Der Import wurde abgebrochen.", Konstanten.PROGRAMMNAME,
                                          JOptionPane.WARNING_MESSAGE)
        }
    }
}