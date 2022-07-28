package mediathek.gui.actions.import_actions

import mediathek.config.Konstanten
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FileDialogs.Companion.chooseLoadFileLocation
import mediathek.tool.SwingErrorDialog
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JOptionPane

class ImportOldBlacklistAction : AbstractAction() {
    init {
        putValue(NAME, "Alte Blacklist...")
        putValue(SHORT_DESCRIPTION, "Ermöglicht den Import der Blacklist aus einer alten Konfigurationsdatei.")
    }

    override fun actionPerformed(e: ActionEvent) {
        val selectedFile = chooseLoadFileLocation(MediathekGui.ui(), " Konfigurationsdatei öffnen", "")
        if (selectedFile != null) {
            try {
                val configReader = OldConfigFileImporter()
                val (_, foundBlacklistEntries) = configReader.importAboBlacklist(selectedFile.absolutePath,
                                                                                 importAbo = false,
                                                                                 importBlacklist = true,
                                                                                 importReplaceList = false)
                val text = "Es wurden $foundBlacklistEntries Einträge importiert."
                JOptionPane.showMessageDialog(MediathekGui.ui(), text, Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE)
            }
            catch (ex: Exception) {
                val text = """
                    Es trat ein Fehler beim Import der Blacklist auf.
                    Sollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.
                    """.trimIndent()
                SwingErrorDialog.showExceptionMessage(MediathekGui.ui(), text, ex)
            }
        } else {
            JOptionPane.showMessageDialog(MediathekGui.ui(),
                                          "Der Import der Blacklist wurde abgebrochen.",
                                          Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE)
        }
    }
}