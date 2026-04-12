package mediathek.gui.abo

import mediathek.tool.ApplicationConfiguration
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.withLock
import org.apache.commons.configuration2.sync.LockMode
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Frame
import javax.swing.JDialog

class ManageAboDialog(owner: Frame?) : JDialog(owner) {
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

    companion object {
        private const val HEIGHT = "manage_abo_dialog.height"
        private const val WIDTH = "manage_abo_dialog.width"
        private const val X = "manage_abo_dialog.x"
        private const val Y = "manage_abo_dialog.y"
    }

    private fun restoreFromConfig() {
        val config = ApplicationConfiguration.getConfiguration()
        try {
            config.withLock(LockMode.READ) {
                val height = getInt(HEIGHT)
                val width = getInt(WIDTH)
                val x = getInt(X)
                val y = getInt(Y)

                setSize(width, height)
                setLocation(x, y)
            }
        }
        catch(_: NoSuchElementException) {
        }
    }

    private fun saveToConfig() {
        val config = ApplicationConfiguration.getConfiguration()
        config.withLock(LockMode.WRITE) {
            setProperty(HEIGHT, size.height)
            setProperty(WIDTH, size.width)
            setProperty(X, location.x)
            setProperty(Y, location.y)
        }
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
        minimumSize = Dimension(640,480)
        pack()

        restoreFromConfig()

        EscapeKeyHandler.installHandler(this) { dispose() }
    }
}
