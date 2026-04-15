package mediathek.gui.actions.export

class ExportDecompressedFilmlistAction : AbstractExportFilmlistAction(
    actionName = "Dekomprimierte Filmliste...",
    saveDialogTitle = "Lesbare Filmliste sichern",
    exportSettings = FilmlistExportSettings(
        compressSender = false,
        compressThema = false
    )
)
