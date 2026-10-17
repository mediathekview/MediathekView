package mediathek.gui.actions.export

import mediathek.daten.ListeFilme
import javax.swing.JFrame
import mediathek.i18n.UiMessages

class ExportReadableFilmlistAction(films: ListeFilme, parent: JFrame) : AbstractExportFilmlistAction(
    actionName = UiMessages.text("action.exportReadableFilmlistAction.text"),
    films = films,
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = true,
        compressThema = true
    ),
    parent = parent,
)
