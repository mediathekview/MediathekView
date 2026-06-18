package mediathek.gui.actions.export

import javax.swing.JFrame

class ExportReadableFilmlistAction(parent: JFrame) : AbstractExportFilmlistAction(
    actionName = "Lesbare Filmliste...",
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = true,
        compressThema = true
    ),
    parent = parent,
)
