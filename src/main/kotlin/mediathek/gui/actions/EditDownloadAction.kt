package mediathek.gui.actions

import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class EditDownloadAction(private val guiDownloads: GuiDownloads) : AbstractAction() {
    init {
        putValue(NAME, "Download ändern...")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/pen-to-square.svg"))
    }

    override fun actionPerformed(e: ActionEvent?) {
        guiDownloads.editDownload()
    }
}
