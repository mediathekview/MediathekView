package mediathek.gui.history

import mediathek.config.Daten
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.Action
import javax.swing.JFrame
import javax.swing.JOptionPane

class ResetAboHistoryAction(private val owner: JFrame) : AbstractAction() {
    override fun actionPerformed(e: ActionEvent) {
        val ret = JOptionPane.showConfirmDialog(owner, """
     Sind Sie sicher dass Sie alle Einträge der Abo-Historie löschen wollen?
     Dies kann nicht rückgängig gemacht werden.
     """.trimIndent(), "Abo-Historie löschen", JOptionPane.YES_NO_OPTION)
        if (ret == JOptionPane.OK_OPTION) {
            Daten.getInstance().aboHistoryController.removeAll()
        }
    }

    init {
        putValue(Action.NAME, "Abo-Historie zurücksetzen...")
    }
}