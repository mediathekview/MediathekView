package mediathek.gui.abo

import mediathek.tool.ApplicationConfiguration
import mediathek.tool.EscapeKeyHandler
import org.apache.commons.configuration2.sync.LockMode
import java.awt.BorderLayout
import java.awt.Frame
import javax.swing.JDialog

class ManageAboDialog(owner: Frame?) : JDialog(owner) {
    private val aboPanel: ManageAboPanel

    override fun dispose() {
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
            config.lock(LockMode.READ)
            val height = config.getInt(HEIGHT)
            val width = config.getInt(WIDTH)
            val x = config.getInt(X)
            val y = config.getInt(Y)

            setSize(width, height)
            setLocation(x,y)
        }
        catch(_: NoSuchElementException) {
        }
        finally {
            config.unlock(LockMode.READ)
        }
    }

    private fun saveToConfig() {
        val config = ApplicationConfiguration.getConfiguration()
        try {
            config.lock(LockMode.WRITE)
            config.setProperty(HEIGHT, size.height)
            config.setProperty(WIDTH, size.width)
            config.setProperty(X, location.x)
            config.setProperty(Y, location.y)
        }
        finally {
            config.unlock(LockMode.WRITE)
        }
    }

    init {
        title = "Abos verwalten"
        defaultCloseOperation = DISPOSE_ON_CLOSE
        isResizable = true
        isModal = true
        aboPanel = ManageAboPanel()
        val contentPane = contentPane
        contentPane.layout = BorderLayout()
        contentPane.add(aboPanel, BorderLayout.CENTER)
        pack()

        restoreFromConfig()

        EscapeKeyHandler.installHandler(this) { dispose() }
    }
}