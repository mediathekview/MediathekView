package mediathek.gui.dialog.reset

import mediathek.gui.dialog.StandardCloseDialog
import mediathek.mainwindow.SettingsResetHost
import javax.swing.JComponent

class ResetSettingsDialog(
    private val host: SettingsResetHost,
) : StandardCloseDialog(host.ownerFrame(), "Programm zurücksetzen", true) {
    init {
        isResizable = false
    }

    override fun createContentPanel(): JComponent = ResetSettingsPanel(host)
}
