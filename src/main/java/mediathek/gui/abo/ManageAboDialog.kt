package mediathek.gui.abo

import mediathek.config.MVConfig
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.GuiFunktionen
import java.awt.BorderLayout
import java.awt.Frame
import javax.swing.JDialog

class ManageAboDialog(owner: Frame?) : JDialog(owner) {
    private val aboPanel: ManageAboPanel
    override fun dispose() {
        aboPanel.tabelleSpeichern()
        GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_MANAGE_ABO, this)
        super.dispose()
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
        //restore saved size
        GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_GROESSE_MANAGE_ABO, this, owner)
        EscapeKeyHandler.installHandler(this) { dispose() }
    }
}