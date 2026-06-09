package mediathek.gui.abo

import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.EscapeKeyHandler
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Frame
import javax.swing.JDialog

class ManageAboDialog(owner: Frame?) : JDialog(owner) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val aboPanel: ManageAboPanel
    private var disposed = false

    override fun dispose() {
        if (disposed) {
            super.dispose()
            return
        }
        disposed = true

        aboPanel.tabelleSpeichern()
        saveToConfig()

        super.dispose()
    }

    private fun restoreFromConfig() {
        val state = applicationConfiguration.manageAboDialogState
        if (state.hasStoredBounds()) {
            setSize(state.width, state.height)
            setLocation(state.x, state.y)
        }
    }

    private fun saveToConfig() {
        applicationConfiguration.setManageAboDialogBounds(
            location.x,
            location.y,
            size.width,
            size.height,
        )
    }

    init {
        title = "Abos verwalten"
        defaultCloseOperation = DISPOSE_ON_CLOSE
        isResizable = true
        isModal = true
        aboPanel = ManageAboPanel(this)
        val contentPane = contentPane
        contentPane.layout = BorderLayout()
        contentPane.add(aboPanel, BorderLayout.CENTER)
        minimumSize = Dimension(640, 480)
        pack()

        restoreFromConfig()

        EscapeKeyHandler.installHandler(this) { dispose() }
    }
}
