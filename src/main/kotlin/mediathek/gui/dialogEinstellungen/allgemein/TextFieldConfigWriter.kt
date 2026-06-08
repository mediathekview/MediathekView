package mediathek.gui.dialogEinstellungen.allgemein

import mediathek.tool.ApplicationConfiguration
import mediathek.tool.withLock
import org.apache.commons.configuration2.sync.LockMode
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import javax.swing.JTextField

class TextFieldConfigWriter(
    private val control: JTextField,
    private val configPropertyKey: String,
) : ActionListener {
    override fun actionPerformed(e: ActionEvent) {
        ApplicationConfiguration.getConfiguration().withLock(LockMode.WRITE) {
            setProperty(configPropertyKey, control.text)
        }
    }
}
