package mediathek.gui.actions.export

import javax.swing.JFrame

class ExportDecompressedFilmlistAction(parent: JFrame) : AbstractExportFilmlistAction(
    actionName = "Dekomprimierte Filmliste...",
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = false,
        compressThema = false
    ),
    parent = parent,
)
