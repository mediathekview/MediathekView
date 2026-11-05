package mediathek.gui.actions

import mediathek.i18n.UiMessages
import mediathek.update.ProgrammUpdateSuchen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JFrame

class ShowProgramInfosAction(
    private val parent: JFrame,
) : AbstractAction() {
    init {
        putValue(NAME, UiMessages.text("action.showProgramInfosAction.text"))
    }

    override fun actionPerformed(event: ActionEvent?) {
        ProgrammUpdateSuchen(ownerProvider = { parent }).checkVersion(
            showAlert = false,
            showProgramInformation = true,
            showAllInformation = false,
            silent = false,
        )
    }
}
