package mediathek.gui.dialog.about

import mediathek.tool.EscapeKeyHandler
import java.awt.BorderLayout
import java.awt.Frame
import javax.swing.JDialog

class AboutDialog(owner: Frame?) : JDialog(owner, "Über dieses Programm", true) {
    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        EscapeKeyHandler.installHandler(this) { dispose() }
        contentPane.layout = BorderLayout()
        contentPane.add(AboutDialogPanel(), BorderLayout.CENTER)
        pack()
    }
}