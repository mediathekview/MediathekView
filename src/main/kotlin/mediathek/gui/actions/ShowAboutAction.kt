package mediathek.gui.actions

import mediathek.gui.dialog.AboutDialog
import mediathek.mainwindow.MainWindowHandle
import mediathek.swing.centerOnScreen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ShowAboutAction(
    private val owner: MainWindowHandle,
) : AbstractAction() {
    override fun actionPerformed(e: ActionEvent?) {
        val dialog = AboutDialog(owner.ownerFrame())
        dialog.centerOnScreen()
        dialog.isVisible = true
        dialog.dispose()
    }

    init {
        putValue(NAME, "Über dieses Programm...")
    }
}
