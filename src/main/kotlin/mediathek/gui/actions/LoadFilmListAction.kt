package mediathek.gui.actions

import mediathek.mainwindow.MediathekGui
import mediathek.swing.IconUtils
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import javax.swing.AbstractAction
import javax.swing.KeyStroke

class LoadFilmListAction(
    private val mediathekGui: MediathekGui,
) : AbstractAction() {
    init {
        putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0))
        putValue(SMALL_ICON, IconUtils.windowBarSpecificToolbarIcon(FontAwesomeSolid.CLOUD_DOWNLOAD_ALT))
        putValue(NAME, "Neue Filmliste laden...")
        putValue(SHORT_DESCRIPTION, "Neue Filmliste laden")
    }

    override fun actionPerformed(event: ActionEvent?) {
        mediathekGui.performFilmListLoadOperation(false)
    }
}
