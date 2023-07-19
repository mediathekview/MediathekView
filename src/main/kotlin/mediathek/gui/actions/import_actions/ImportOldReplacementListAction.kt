package mediathek.gui.actions.import_actions

import mediathek.config.Konstanten
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FileDialogs.Companion.chooseLoadFileLocation
import mediathek.tool.SwingErrorDialog
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JOptionPane

class ImportOldReplacementListAction : AbstractAction() {
    init {
        putValue(NAME, "Alte Ersetzungstabelle...")
        putValue(SHORT_DESCRIPTION, "Ermöglicht den Import der Ersetzungstabelle aus einer alten Konfigurationsdatei.")
    }

    override fun actionPerformed(e: ActionEvent) {
        val selectedFile = chooseLoadFileLocation(MediathekGui.ui(), " Konfigurationsdatei öffnen", "")
        if (selectedFile != null) {
            try {
                val configReader = OldConfigFileImporter()
                val (_, _, foundReplaceListEntries) = configReader.importAboBlacklist(selectedFile.absolutePath,
                                                                                      importAbo = false,
                                                                                      importBlacklist = false,
                                                                                      importReplaceList = true)
                val text = "Es wurden $foundReplaceListEntries Einträge importiert."
                JOptionPane.showMessageDialog(MediathekGui.ui(), text, Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE)
            }
            catch (ex: Exception) {
                val text = """
                    Es trat ein Fehler beim Import der Ersetzungstabelle auf.
                    Sollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.
                    """.trimIndent()
                SwingErrorDialog.showExceptionMessage(MediathekGui.ui(), text, ex)
            }
        } else {
            JOptionPane.showMessageDialog(MediathekGui.ui(),
                                          "Der Import der Ersetzungstabelle wurde abgebrochen.",
                                          Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE)
        }
    }
}