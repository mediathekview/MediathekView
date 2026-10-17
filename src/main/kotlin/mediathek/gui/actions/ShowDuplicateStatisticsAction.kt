package mediathek.gui.actions

import mediathek.filmlisten.FilmCatalog
import mediathek.gui.duplicates.statistics.DuplicateStatisticsDialog
import java.awt.Frame
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import mediathek.i18n.UiMessages

class ShowDuplicateStatisticsAction(
    private val owner: Frame,
    private val filmCatalog: FilmCatalog,
) : AbstractAction() {
    init {
        putValue(NAME, UiMessages.text("action.showDuplicateStatisticsAction.text"))
    }

    override fun actionPerformed(event: ActionEvent?) {
        DuplicateStatisticsDialog(owner, filmCatalog, this).isVisible = true
    }
}
