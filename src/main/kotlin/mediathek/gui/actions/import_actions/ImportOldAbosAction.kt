package mediathek.gui.actions.import_actions

import mediathek.config.Konstanten
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.i18n.UiMessages
import mediathek.tool.FileDialogs.chooseLoadFileLocation
import mediathek.tool.ReplacementRules
import mediathek.tool.SwingErrorDialog
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JFrame
import javax.swing.JOptionPane

class ImportOldAbosAction(
    private val parent: JFrame,
    private val abos: AboServices,
    private val blacklist: BlacklistServices,
    private val replacementRules: ReplacementRules,
) : AbstractAction() {
    init {
        putValue(NAME, UiMessages.text("action.importOldAbosAction.text"))
        putValue(SHORT_DESCRIPTION, UiMessages.text("action.importOldAbosAction.description"))
    }

    override fun actionPerformed(e: ActionEvent) {
        val selectedFile = chooseLoadFileLocation(parent, " Konfigurationsdatei öffnen", "")
        if (selectedFile != null) {
            try {
                val configReader = OldConfigFileImporter(abos, blacklist, replacementRules)
                val (foundAbos) = configReader.importAboBlacklist(selectedFile.absolutePath,
                                                                  importAbo = true,
                                                                  importBlacklist = false,
                                                                  importReplaceList = false)
                val text = "Es wurden $foundAbos Einträge importiert."
                JOptionPane.showMessageDialog(parent, text, Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE)
            }
            catch (ex: Exception) {
                val text = """
                    Es trat ein Fehler beim Import der Abos auf.
                    Sollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.
                    """.trimIndent()
                SwingErrorDialog.showExceptionMessage(parent, text, ex)
            }
        } else {
            JOptionPane.showMessageDialog(parent, "Der Import wurde abgebrochen.", Konstanten.PROGRAMMNAME,
                                          JOptionPane.WARNING_MESSAGE)
        }
    }
}
