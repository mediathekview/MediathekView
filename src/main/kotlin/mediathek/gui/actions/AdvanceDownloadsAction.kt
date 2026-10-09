package mediathek.gui.actions

import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class AdvanceDownloadsAction(private val guiDownloads: GuiDownloads) : AbstractAction() {
    init {
        putValue(NAME, "Downloads vorziehen")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-up.svg"))
    }

    override fun actionPerformed(e: ActionEvent?) {
        guiDownloads.downloadsVorziehen()
    }
}
