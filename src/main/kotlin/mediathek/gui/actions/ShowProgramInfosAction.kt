package mediathek.gui.actions

import mediathek.update.ProgrammUpdateSuchen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ShowProgramInfosAction : AbstractAction() {
    init {
        putValue(NAME, "Programminfos anzeigen...")
    }

    override fun actionPerformed(event: ActionEvent?) {
        ProgrammUpdateSuchen().checkVersion(false, true, false, false)
    }
}
