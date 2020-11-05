package mediathek.gui.dialog

import mediathek.config.Daten
import mediathek.gui.actions.DisposeDialogAction
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.FilmListUpdateType
import mediathek.tool.GuiFunktionen
import java.awt.BorderLayout
import java.awt.FlowLayout
import java.awt.Frame
import javax.swing.JButton
import javax.swing.JDialog
import javax.swing.JPanel
import javax.swing.border.EmptyBorder

class LoadFilmListDialog(owner: Frame?) : JDialog(owner, "Filmliste laden", true) {
    private val contentPanel: PanelFilmlisteLaden
    private fun createButtonPanel(): JPanel {
        val btnContentPanel = JPanel()
        btnContentPanel.border = EmptyBorder(0, 10, 10, 10)
        btnContentPanel.layout = BorderLayout()

        val buttonPanel = JPanel()
        buttonPanel.layout = FlowLayout()

        var btn = JButton(DisposeDialogAction(this,"Schließen", "Dialog schließen"))
        buttonPanel.add(btn)
        getRootPane().defaultButton = btn

        btn = JButton("Filmliste laden")
        btn.addActionListener {
            val filmeLaden = Daten.getInstance().filmeLaden
            if (GuiFunktionen.getImportArtFilme() == FilmListUpdateType.AUTOMATIC) {
                //easy, just load
                filmeLaden.loadFilmlist("", false)
            } else {
                //manual or extend
                val text = contentPanel.jTextFieldUrl.text
                if (contentPanel.jCheckBoxUpdate.isSelected)
                    filmeLaden.updateFilmlist(text)
                else
                    filmeLaden.loadFilmlist(text, false)
            }
            dispose()
        }

        buttonPanel.add(btn)
        btnContentPanel.add(buttonPanel, BorderLayout.EAST)
        return btnContentPanel
    }

    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        contentPanel = PanelFilmlisteLaden(false)
        contentPane.layout = BorderLayout()
        contentPane.add(contentPanel, BorderLayout.CENTER)
        contentPane.add(createButtonPanel(), BorderLayout.SOUTH)
        pack()

        isResizable = false
        EscapeKeyHandler.installHandler(this) { dispose() }
        GuiFunktionen.centerOnScreen(this, false)
    }
}