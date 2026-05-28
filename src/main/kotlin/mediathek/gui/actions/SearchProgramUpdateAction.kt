package mediathek.gui.actions

import mediathek.update.ProgrammUpdateSuchen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class SearchProgramUpdateAction : AbstractAction() {
    init {
        putValue(NAME, "Nach Update suchen...")
    }

    override fun actionPerformed(event: ActionEvent?) {
        ProgrammUpdateSuchen().checkVersion(true, false, false, false)
    }
}
