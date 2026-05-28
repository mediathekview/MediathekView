package mediathek.gui.actions

import mediathek.config.Daten
import mediathek.gui.dialog.DialogLeer
import mediathek.gui.dialogEinstellungen.PanelBlacklist
import mediathek.swing.IconUtils
import mediathek.tool.GuiFunktionen
import org.apache.commons.lang3.SystemUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignL
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import javax.swing.AbstractAction
import javax.swing.JFrame
import javax.swing.KeyStroke

class EditBlacklistAction(
    private val parent: JFrame,
) : AbstractAction() {
    init {
        putValue(NAME, "Blacklist bearbeiten...")
        putValue(SMALL_ICON, IconUtils.windowBarSpecificToolbarIcon(MaterialDesignL.LIST_BOX_OUTLINE))
        putValue(SHORT_DESCRIPTION, "Blacklist bearbeiten")
        val keyStroke = if (SystemUtils.IS_OS_MAC_OSX) {
            KeyStroke.getKeyStroke(KeyEvent.VK_F9, GuiFunktionen.getPlatformControlKey())
        } else {
            KeyStroke.getKeyStroke(KeyEvent.VK_B, GuiFunktionen.getPlatformControlKey())
        }
        putValue(ACCELERATOR_KEY, keyStroke)
    }

    override fun actionPerformed(event: ActionEvent?) {
        DialogLeer(parent, true).apply {
            init("Blacklist", PanelBlacklist(Daten.getInstance(), null, PanelBlacklist::class.java.name + "_3"))
            isVisible = true
        }
    }
}
