package mediathek.gui.actions

import mediathek.gui.tabs.tab_downloads.GuiDownloads
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import mediathek.i18n.UiMessages

class StopDownloadsAction(private val guiDownloads: GuiDownloads) : AbstractAction() {
    init {
        putValue(NAME, UiMessages.text("action.stopDownloadsAction.text"))
    }

    override fun actionPerformed(e: ActionEvent?) {
        guiDownloads.stoppen(false)
    }
}
