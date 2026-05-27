package mediathek.gui.actions

import mediathek.mainwindow.MediathekGui
import mediathek.swing.IconUtils
import mediathek.tool.GuiFunktionen
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import javax.swing.AbstractAction
import javax.swing.KeyStroke

class ShowFilmInformationAction : AbstractAction() {
    init {
        putValue(NAME, "Filminformation anzeigen")
        putValue(SHORT_DESCRIPTION, "Filminformation anzeigen")
        putValue(SMALL_ICON, IconUtils.windowBarSpecificToolbarIcon(FontAwesomeSolid.INFO_CIRCLE))
        putValue(
            ACCELERATOR_KEY,
            KeyStroke.getKeyStroke(KeyEvent.VK_I, GuiFunktionen.getPlatformControlKey()),
        )
    }

    override fun actionPerformed(event: ActionEvent?) {
        val filmInfoDialog = MediathekGui.ui().filmInfoDialog
        if (!filmInfoDialog.isVisible) {
            filmInfoDialog.showInfo()
        }
    }
}
