package mediathek.gui.actions.import_actions

import mediathek.config.Konstanten
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.tool.FileDialogs.chooseLoadFileLocation
import mediathek.tool.ReplacementRules
import mediathek.tool.SwingErrorDialog
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JFrame
import javax.swing.JOptionPane

class ImportOldBlacklistAction(
    private val parent: JFrame,
    private val abos: AboServices,
    private val blacklist: BlacklistServices,
    private val replacementRules: ReplacementRules,
) : AbstractAction() {
    init {
        putValue(NAME, "Alte Blacklist...")
        putValue(SHORT_DESCRIPTION, "Ermöglicht den Import der Blacklist aus einer alten Konfigurationsdatei.")
    }

    override fun actionPerformed(e: ActionEvent) {
        val selectedFile = chooseLoadFileLocation(parent, " Konfigurationsdatei öffnen", "")
        if (selectedFile != null) {
            try {
                val configReader = OldConfigFileImporter(abos, blacklist, replacementRules)
                val (_, foundBlacklistEntries) = configReader.importAboBlacklist(selectedFile.absolutePath,
                                                                                 importAbo = false,
                                                                                 importBlacklist = true,
                                                                                 importReplaceList = false)
                val text = "Es wurden $foundBlacklistEntries Einträge importiert."
                JOptionPane.showMessageDialog(parent, text, Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE)
            }
            catch (ex: Exception) {
                val text = """
                    Es trat ein Fehler beim Import der Blacklist auf.
                    Sollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.
                    """.trimIndent()
                SwingErrorDialog.showExceptionMessage(parent, text, ex)
            }
        } else {
            JOptionPane.showMessageDialog(parent,
                                          "Der Import der Blacklist wurde abgebrochen.",
                                          Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE)
        }
    }
}
