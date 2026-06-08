package mediathek.gui.actions

import mediathek.gui.dialog.AboutDialog
import mediathek.mainwindow.MediathekGui
import mediathek.swing.centerOnScreen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ShowAboutAction : AbstractAction() {
    override fun actionPerformed(e: ActionEvent?) {
        val dialog = AboutDialog(MediathekGui.ui())
        dialog.centerOnScreen()
        dialog.isVisible = true
        dialog.dispose()
    }

    init {
        putValue(NAME, "Über dieses Programm...")
    }
}
