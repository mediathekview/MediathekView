package mediathek.gui.actions.export

class ExportReadableFilmlistAction : AbstractExportFilmlistAction(
    actionName = "Lesbare Filmliste...",
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = true,
        compressThema = true
    )
)
