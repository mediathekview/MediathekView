package mediathek.gui.actions

import mediathek.gui.statistics.FilmStatisticsDialog
import java.awt.Frame
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ShowFilmStatisticsAction(
    private val owner: Frame,
) : AbstractAction() {
    init {
        putValue(NAME, "Filmlisten-Statistik anzeigen...")
    }

    override fun actionPerformed(event: ActionEvent?) {
        FilmStatisticsDialog(owner, this).isVisible = true
    }
}
