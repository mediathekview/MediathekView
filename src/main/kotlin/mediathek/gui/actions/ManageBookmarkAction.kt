package mediathek.gui.actions

import mediathek.i18n.UiMessages
import mediathek.mainwindow.FilmBookmarkHost
import mediathek.swing.IconUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignF
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ManageBookmarkAction(
    private val host: FilmBookmarkHost,
) : AbstractAction() {
    init {
        putValue(NAME, UiMessages.text("action.manageBookmarkAction.text"))
        putValue(SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignF.FILE_DOCUMENT))
        putValue(SHORT_DESCRIPTION, UiMessages.text("action.manageBookmarkAction.description"))
    }

    override fun actionPerformed(event: ActionEvent?) {
        host.showManageBookmarkWindow()
    }
}
