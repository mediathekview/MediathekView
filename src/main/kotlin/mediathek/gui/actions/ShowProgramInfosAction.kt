package mediathek.gui.actions

import mediathek.update.ProgrammUpdateSuchen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JFrame

class ShowProgramInfosAction(
    private val parent: JFrame,
) : AbstractAction() {
    init {
        putValue(NAME, "Programminfos anzeigen...")
    }

    override fun actionPerformed(event: ActionEvent?) {
        ProgrammUpdateSuchen(ownerProvider = { parent }).checkVersion(false, true, false, false)
    }
}
