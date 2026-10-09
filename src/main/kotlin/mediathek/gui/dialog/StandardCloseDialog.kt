package mediathek.gui.dialog

import mediathek.gui.actions.DisposeDialogAction
import mediathek.tool.EscapeKeyHandler
import java.awt.BorderLayout
import java.awt.Frame
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.JDialog

/**
 * A standard swing dialog template with a close button.
 */
abstract class StandardCloseDialog(owner: Frame?, title: String, modal: Boolean) : JDialog(owner, title, modal) {
    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        EscapeKeyHandler.installHandler(this, ::dispose)

        contentPane.apply {
            layout = BorderLayout()
            add(createContentPanel(), BorderLayout.CENTER)
            add(
                ButtonPanel().apply { add(createButtonPanel(), BorderLayout.EAST) },
                BorderLayout.SOUTH,
            )
        }
        pack()
    }

    abstract fun createContentPanel(): JComponent

    private fun createButtonPanel(): ButtonFlowPanel {
        val button = JButton(DisposeDialogAction(this, "Schließen", "Dialog schließen"))
        rootPane.defaultButton = button
        return ButtonFlowPanel().apply { add(button) }
    }
}
