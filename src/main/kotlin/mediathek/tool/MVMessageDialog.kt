package mediathek.tool

import java.awt.Component
import javax.swing.JOptionPane
import javax.swing.SwingUtilities

object MVMessageDialog {
    fun showMessageDialog(
        parent: Component?,
        message: String,
        title: String,
        messageType: Int,
    ) {
        if (SwingUtilities.isEventDispatchThread()) {
            JOptionPane.showMessageDialog(parent, message, title, messageType)
        } else {
            SwingUtilities.invokeLater {
                JOptionPane.showMessageDialog(parent, message, title, messageType)
            }
        }
    }
}
