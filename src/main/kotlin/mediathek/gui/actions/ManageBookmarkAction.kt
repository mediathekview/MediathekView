package mediathek.gui.actions

import mediathek.mainwindow.FilmBookmarkHost
import mediathek.swing.IconUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignF
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ManageBookmarkAction(
    private val host: FilmBookmarkHost,
) : AbstractAction() {
    init {
        putValue(NAME, "Merkliste verwalten...")
        putValue(SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignF.FILE_DOCUMENT))
        putValue(SHORT_DESCRIPTION, "Merkliste verwalten")
    }

    override fun actionPerformed(event: ActionEvent?) {
        host.showManageBookmarkWindow()
    }
}
