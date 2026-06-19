package mediathek.gui.actions

import mediathek.mainwindow.FilmBookmarkHost
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ResetFilterDialogPosition(private val host: FilmBookmarkHost) : AbstractAction() {
    init {
        putValue(NAME, "Filterdialog-Position zurücksetzen")
    }

    override fun actionPerformed(e: ActionEvent) {
        host.resetFilterDialogPosition()
    }
}
