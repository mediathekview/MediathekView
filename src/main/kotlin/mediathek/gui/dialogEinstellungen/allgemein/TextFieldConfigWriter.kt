package mediathek.gui.dialogEinstellungen.allgemein

import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.function.Consumer
import javax.swing.JTextField

class TextFieldConfigWriter(
    private val control: JTextField,
    private val valueWriter: Consumer<String>,
) : ActionListener {
    override fun actionPerformed(e: ActionEvent) {
        valueWriter.accept(control.text)
    }
}
