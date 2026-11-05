package mediathek.tool

import mediathek.config.Konstanten
import java.awt.Component
import javax.swing.JOptionPane
import mediathek.i18n.UiMessages

object NoSelectionErrorDialog {
    @JvmStatic
    fun show(parent: Component?) {
        JOptionPane.showMessageDialog(
            parent,
            UiMessages.text("settings.noSelection"),
            Konstanten.PROGRAMMNAME,
            JOptionPane.ERROR_MESSAGE
        )
    }
}
