package mediathek.gui.dialog.reset

import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.gui.dialog.StandardCloseDialog
import mediathek.mainwindow.SettingsResetHost
import java.util.function.BiConsumer
import javax.swing.JComponent

class ResetSettingsDialog(
    private val host: SettingsResetHost,
    private val programSets: ProgramSetRepository,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
) : StandardCloseDialog(host.ownerFrame(), "Programm zurücksetzen", true) {
    init {
        isResizable = false
    }

    override fun createContentPanel(): JComponent = ResetSettingsPanel(host, programSets, programSetExporter)
}
