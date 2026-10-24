package mediathek.gui.actions.export

import mediathek.daten.ListeFilme
import javax.swing.JFrame
import mediathek.i18n.UiMessages

class ExportDecompressedFilmlistAction(films: ListeFilme, parent: JFrame) : AbstractExportFilmlistAction(
    actionName = UiMessages.text("action.exportDecompressedFilmlistAction.text"),
    films = films,
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = false,
        compressThema = false
    ),
    parent = parent,
)
