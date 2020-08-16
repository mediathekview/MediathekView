package mediathek.gui.history

import mediathek.config.Daten
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.Action
import javax.swing.JFrame
import javax.swing.JOptionPane

class ResetDownloadHistoryAction(private val owner: JFrame) : AbstractAction() {
    override fun actionPerformed(e: ActionEvent) {
        val ret = JOptionPane.showConfirmDialog(owner, """
     Sind Sie sicher dass Sie alle Einträge der Download-Historie löschen wollen?
     Dies kann nicht rückgängig gemacht werden.
     """.trimIndent(), "Download-Historie löschen", JOptionPane.YES_NO_OPTION)
        if (ret == JOptionPane.OK_OPTION) {
            Daten.getInstance().seenHistoryController.alleLoeschen()
        }
    }

    init {
        putValue(Action.NAME, "Download-Historie zurücksetzen...")
    }
}