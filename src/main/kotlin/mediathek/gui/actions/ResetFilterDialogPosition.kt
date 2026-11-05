package mediathek.gui.actions

import mediathek.i18n.UiMessages
import mediathek.mainwindow.FilmBookmarkHost
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ResetFilterDialogPosition(private val host: FilmBookmarkHost) : AbstractAction() {
    init {
        putValue(NAME, UiMessages.text("action.resetFilterDialogPosition.text"))
    }

    override fun actionPerformed(e: ActionEvent) {
        host.resetFilterDialogPosition()
    }
}
