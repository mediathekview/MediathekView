package mediathek.gui.actions

import mediathek.daten.ListeAbo
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class CreateNewAboAction(private val listeAbo: ListeAbo) : AbstractAction() {
    override fun actionPerformed(e: ActionEvent?) {
        listeAbo.addAbo("Neu", "", "", "")
    }

    init {
        putValue(NAME, "Abo anlegen...")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg"))
    }
}