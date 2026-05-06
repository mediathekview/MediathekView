package mediathek.gui.actions

import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.swing.IconUtils
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import javax.swing.AbstractAction
import javax.swing.KeyStroke

class RefreshDownloadListAction(private val guiDownloads: GuiDownloads) : AbstractAction() {
    init {
        putValue(NAME, "Liste der Downloads aktualisieren")
        putValue(SHORT_DESCRIPTION, "Downloadliste aktualisieren")
        putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_W, KeyEvent.CTRL_DOWN_MASK))
        putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.REDO_ALT))
    }

    override fun actionPerformed(e: ActionEvent?) {
        guiDownloads.updateDownloads()
    }
}
