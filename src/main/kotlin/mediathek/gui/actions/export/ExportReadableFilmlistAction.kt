package mediathek.gui.actions.export

import mediathek.daten.ListeFilme
import javax.swing.JFrame

class ExportReadableFilmlistAction(films: ListeFilme, parent: JFrame) : AbstractExportFilmlistAction(
    actionName = "Lesbare Filmliste...",
    films = films,
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = true,
        compressThema = true
    ),
    parent = parent,
)
