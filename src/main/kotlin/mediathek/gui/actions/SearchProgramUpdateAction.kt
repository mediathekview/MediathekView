package mediathek.gui.actions

import mediathek.mainwindow.MainWindowHandle
import mediathek.update.ProgrammUpdateSuchen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class SearchProgramUpdateAction(
    private val owner: MainWindowHandle,
) : AbstractAction() {
    init {
        putValue(NAME, "Nach Update suchen...")
    }

    override fun actionPerformed(event: ActionEvent?) {
        ProgrammUpdateSuchen(owner::ownerFrame).checkVersion(true, false, false, false)
    }
}
