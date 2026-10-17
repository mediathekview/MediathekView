package mediathek.gui.actions

import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.i18n.UiMessages
import mediathek.swing.IconUtils
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class StartAllDownloadsAction(private val guiDownloads: GuiDownloads) : AbstractAction() {
    init {
        putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.ANGLE_DOUBLE_DOWN))
        putValue(SHORT_DESCRIPTION, UiMessages.text("action.startAllDownloadsAction.description"))
        putValue(NAME, UiMessages.text("action.startAllDownloadsAction.text"))
    }

    override fun actionPerformed(e: ActionEvent?) {
        guiDownloads.starten(true)
    }
}
