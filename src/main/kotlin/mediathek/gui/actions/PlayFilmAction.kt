package mediathek.gui.actions

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.daten.DatenPset
import mediathek.swing.IconUtils
import mediathek.tool.GuiFunktionen
import org.apache.commons.lang3.SystemUtils
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.Component
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import java.util.function.Consumer
import javax.swing.AbstractAction
import javax.swing.JOptionPane
import javax.swing.KeyStroke

class PlayFilmAction(
    private val startFilm: Consumer<DatenPset>,
    private val parentProvider: () -> Component?,
) : AbstractAction() {
    init {
        putValue(NAME, "Film abspielen")
        putValue(SHORT_DESCRIPTION, "Film abspielen")
        putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.PLAY))
        val keyStroke = if (SystemUtils.IS_OS_MAC_OSX) {
            KeyStroke.getKeyStroke(KeyEvent.VK_F6, GuiFunktionen.getPlatformControlKey())
        } else {
            KeyStroke.getKeyStroke(KeyEvent.VK_P, GuiFunktionen.getPlatformControlKey())
        }
        putValue(ACCELERATOR_KEY, keyStroke)
    }

    @Synchronized
    override fun actionPerformed(event: ActionEvent?) {
        val pset = Daten.getInstance().listePset.psetAbspielen
        if (pset != null) {
            startFilm.accept(pset)
        } else {
            JOptionPane.showMessageDialog(
                parentProvider(),
                "Es wurde kein Videoplayer eingerichtet.\n" +
                    "Bitte legen Sie diesen unter \"Einstellungen->Set bearbeiten\" fest.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE,
            )
        }
    }
}
