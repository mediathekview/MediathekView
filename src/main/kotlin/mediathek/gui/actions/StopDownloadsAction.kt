package mediathek.gui.actions

import mediathek.gui.tabs.tab_downloads.GuiDownloads
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class StopDownloadsAction(private val guiDownloads: GuiDownloads) : AbstractAction() {
    init {
        putValue(NAME, "Ausgewählte Downloads stoppen")
    }

    override fun actionPerformed(e: ActionEvent?) {
        guiDownloads.stoppen(false)
    }
}
