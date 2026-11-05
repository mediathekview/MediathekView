package mediathek.gui.actions

import mediathek.gui.dialog.AboutDialog
import mediathek.i18n.UiMessages
import mediathek.swing.centerOnScreen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JFrame

class ShowAboutAction(
    private val parent: JFrame,
) : AbstractAction() {
    override fun actionPerformed(e: ActionEvent?) {
        val dialog = AboutDialog(parent)
        dialog.centerOnScreen()
        dialog.isVisible = true
        dialog.dispose()
    }

    init {
        putValue(NAME, UiMessages.text("action.showAboutAction.text"))
    }
}
