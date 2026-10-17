package mediathek.gui.actions

import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.i18n.UiMessages
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class OpenTargetFolderAction(private val guiDownloads: GuiDownloads) : AbstractAction() {
    init {
        putValue(NAME, UiMessages.text("action.openTargetFolderAction.text"))
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"))
    }

    override fun actionPerformed(e: ActionEvent?) {
        guiDownloads.zielordnerOeffnen()
    }
}
