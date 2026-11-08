package mediathek.gui.dialog.reset

import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.gui.dialog.StandardCloseDialog
import mediathek.mainwindow.SettingsResetHost
import java.util.function.BiConsumer

class ResetSettingsDialog(
    host: SettingsResetHost,
    programSets: ProgramSetRepository,
    programSetExporter: BiConsumer<Array<DatenPset>, String>,
) : StandardCloseDialog(
    host.ownerFrame(),
    "Programm zurücksetzen",
    true,
    ResetSettingsPanel(host, programSets, programSetExporter),
) {
    init {
        isResizable = false
    }
}
