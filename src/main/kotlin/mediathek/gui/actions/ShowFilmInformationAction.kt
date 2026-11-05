package mediathek.gui.actions

import mediathek.gui.filmInformation.FilmInfoDialog
import mediathek.i18n.UiMessages
import mediathek.swing.IconUtils
import mediathek.tool.GuiFunktionen
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import javax.swing.AbstractAction
import javax.swing.KeyStroke

class ShowFilmInformationAction(
    private val filmInfoDialogProvider: () -> FilmInfoDialog?,
) : AbstractAction() {
    init {
        putValue(NAME, UiMessages.text("action.showFilmInformationAction.text"))
        putValue(SHORT_DESCRIPTION, UiMessages.text("action.showFilmInformationAction.description"))
        putValue(SMALL_ICON, IconUtils.windowBarSpecificToolbarIcon(FontAwesomeSolid.INFO_CIRCLE))
        putValue(
            ACCELERATOR_KEY,
            KeyStroke.getKeyStroke(KeyEvent.VK_I, GuiFunktionen.getPlatformControlKey()),
        )
    }

    override fun actionPerformed(event: ActionEvent?) {
        val filmInfoDialog = filmInfoDialogProvider() ?: return
        if (!filmInfoDialog.isVisible) {
            filmInfoDialog.showInfo()
        }
    }
}
