package mediathek.gui.actions

import mediathek.mainwindow.MainWindowHandle
import mediathek.update.ProgrammUpdateSuchen
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ShowProgramInfosAction(
    private val owner: MainWindowHandle,
) : AbstractAction() {
    init {
        putValue(NAME, "Programminfos anzeigen...")
    }

    override fun actionPerformed(event: ActionEvent?) {
        ProgrammUpdateSuchen(owner::ownerFrame).checkVersion(false, true, false, false)
    }
}
