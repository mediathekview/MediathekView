package mediathek.gui.dialog

import java.awt.BorderLayout
import javax.swing.JPanel
import javax.swing.border.EmptyBorder

/**
 * Placeholder {@link JPanel} with an empty border.
 * Used for button panels in a dialog.
 */
class ButtonPanel : JPanel() {
    init {
        layout = BorderLayout()
        border = EmptyBorder(0, 10, 10, 10)
    }
}