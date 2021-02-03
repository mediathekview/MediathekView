package mediathek.gui.dialog.about

import javafx.embed.swing.JFXPanel
import javafx.scene.Scene
import mediathek.javafx.tool.JavaFxUtils
import mediathek.tool.EscapeKeyHandler
import java.awt.BorderLayout
import java.awt.Frame
import javax.swing.JDialog
import javax.swing.SwingUtilities

class AboutDialog(owner: Frame?) : JDialog(owner, "Über dieses Programm", true) {
    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        isResizable = false
        EscapeKeyHandler.installHandler(this) { dispose() }
        contentPane.layout = BorderLayout()
        val fxPanel = JFXPanel()
        contentPane.add(fxPanel, BorderLayout.CENTER)
        JavaFxUtils.invokeInFxThreadAndWait {
            fxPanel.scene = Scene(AboutController())
            SwingUtilities.invokeLater { pack() }
        }
    }
}