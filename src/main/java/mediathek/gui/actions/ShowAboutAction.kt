package mediathek.gui.actions

import mediathek.gui.dialog.about.AboutDialog
import mediathek.mainwindow.MediathekGui
import mediathek.tool.GuiFunktionen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ShowAboutAction : AbstractAction() {
    override fun actionPerformed(e: ActionEvent?) {
        val dialog = AboutDialog(MediathekGui.ui())
        GuiFunktionen.centerOnScreen(dialog, false)
        dialog.isVisible = true
        dialog.dispose()
    }

    init {
        putValue(NAME, "Über dieses Programm...")
    }
}