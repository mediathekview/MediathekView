package mediathek.gui.actions

import jiconfont.icons.font_awesome.FontAwesome
import jiconfont.swing.IconFontSwing
import mediathek.daten.ListeAbo
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class CreateNewAboAction(private val listeAbo: ListeAbo) : AbstractAction() {
    override fun actionPerformed(e: ActionEvent) {
        listeAbo.addAbo("Neu", "", "", "")
    }

    init {
        putValue(NAME, "Abo anlegen...")
        putValue(SMALL_ICON, IconFontSwing.buildIcon(FontAwesome.PLUS, 16f))
    }
}