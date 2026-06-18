package mediathek.gui.actions.export

import mediathek.mainwindow.MainWindowHandle

class ExportDecompressedFilmlistAction(owner: MainWindowHandle) : AbstractExportFilmlistAction(
    actionName = "Dekomprimierte Filmliste...",
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = false,
        compressThema = false
    ),
    owner = owner,
)
