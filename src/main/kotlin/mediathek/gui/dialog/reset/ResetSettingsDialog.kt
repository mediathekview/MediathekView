package mediathek.gui.dialog.reset

import mediathek.gui.dialog.StandardCloseDialog
import javax.swing.JComponent
import javax.swing.JFrame

class ResetSettingsDialog(
    private val owner: JFrame?,
) : StandardCloseDialog(owner, "Programm zurücksetzen", true) {
    init {
        isResizable = false
    }

    override fun createContentPanel(): JComponent = ResetSettingsPanel(owner)
}
