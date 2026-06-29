package mediathek.gui.actions

import mediathek.filmlisten.FilmCatalog
import mediathek.gui.statistics.FilmStatisticsDialog
import java.awt.Frame
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ShowFilmStatisticsAction(
    private val owner: Frame,
    private val filmCatalog: FilmCatalog,
) : AbstractAction() {
    init {
        putValue(NAME, "Filmlisten-Statistik anzeigen...")
    }

    override fun actionPerformed(event: ActionEvent?) {
        FilmStatisticsDialog(owner, filmCatalog, this).isVisible = true
    }
}
