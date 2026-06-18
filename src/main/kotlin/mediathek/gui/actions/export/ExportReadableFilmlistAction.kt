package mediathek.gui.actions.export

import mediathek.mainwindow.MainWindowHandle

class ExportReadableFilmlistAction(owner: MainWindowHandle) : AbstractExportFilmlistAction(
    actionName = "Lesbare Filmliste...",
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = true,
        compressThema = true
    ),
    owner = owner,
)
