package mediathek.tool

import mediathek.config.Konstanten
import java.awt.Component
import javax.swing.JOptionPane

object NoSelectionErrorDialog {
    @JvmStatic
    fun show(parent: Component?) {
        JOptionPane.showMessageDialog(
            parent,
            "Der Befehl kann nicht ausgeführt werden.\n" +
                "Sie haben keinen Tabelleneintrag ausgewählt.",
            Konstanten.PROGRAMMNAME,
            JOptionPane.ERROR_MESSAGE
        )
    }
}
