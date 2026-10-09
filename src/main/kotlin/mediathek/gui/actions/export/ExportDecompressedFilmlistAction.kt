package mediathek.gui.actions.export

import mediathek.daten.ListeFilme
import javax.swing.JFrame

class ExportDecompressedFilmlistAction(films: ListeFilme, parent: JFrame) : AbstractExportFilmlistAction(
    actionName = "Dekomprimierte Filmliste...",
    films = films,
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = false,
        compressThema = false
    ),
    parent = parent,
)
