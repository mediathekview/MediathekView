package mediathek.gui.actions

import mediathek.update.ProgrammUpdateSuchen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JFrame

class SearchProgramUpdateAction(
    private val parent: JFrame,
) : AbstractAction() {
    init {
        putValue(NAME, "Nach Update suchen...")
    }

    override fun actionPerformed(event: ActionEvent?) {
        ProgrammUpdateSuchen(ownerProvider = { parent }).checkVersion(
            showAlert = true,
            showProgramInformation = false,
            showAllInformation = false,
            silent = false,
        )
    }
}
