package mediathek.gui.actions

import mediathek.mainwindow.MediathekGui
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ResetFilterDialogPosition(private val mediathekGui: MediathekGui) : AbstractAction() {
    init {
        putValue(NAME, "Filterdialog-Position zurücksetzen")
    }

    override fun actionPerformed(e: ActionEvent) {
        mediathekGui.tabFilme?.filmActionPanel?.filterDialog?.setLocation(100, 100)
    }
}