package mediathek.gui.actions

import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.i18n.UiMessages
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class StartDownloadsAction(private val guiDownloads: GuiDownloads) : AbstractAction() {
    init {
        putValue(NAME, UiMessages.text("action.startDownloadsAction.text"))
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/caret-down.svg"))
    }

    override fun actionPerformed(e: ActionEvent?) {
        guiDownloads.starten(false)
    }
}
