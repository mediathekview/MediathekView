package mediathek.gui.actions

import mediathek.mainwindow.MediathekGui
import mediathek.swing.IconUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignF
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ManageBookmarkAction(
    private val mediathekGui: MediathekGui,
) : AbstractAction() {
    init {
        putValue(NAME, "Merkliste verwalten...")
        putValue(SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignF.FILE_DOCUMENT))
        putValue(SHORT_DESCRIPTION, "Merkliste verwalten")
    }

    override fun actionPerformed(event: ActionEvent?) {
        mediathekGui.tabFilme.showManageBookmarkWindow()
    }
}
