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

    private class ButtonPanel : JPanel() {
        init {
            layout = BorderLayout()
            border = EmptyBorder(0, 10, 10, 10)
        }
    }

    private class ButtonFlowPanel : JPanel() {
        init {
            layout = FlowLayout()
        }
    }

    private val btnContentPanel = ButtonPanel()

    private fun createButtonPanel() {
        val buttonFlowPanel = ButtonFlowPanel()
        btnContentPanel.add(buttonFlowPanel, BorderLayout.EAST)

        val closeBtn = JButton(DisposeDialogAction(this, "Schließen", "Dialog schließen"))
        buttonFlowPanel.add(closeBtn)
        getRootPane().defaultButton = closeBtn

        val btn = JButton("Filmliste laden")
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

        buttonFlowPanel.add(btn)
    }

    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        contentPane.layout = BorderLayout()
        contentPanel = PanelFilmlisteLaden(false)
        contentPane.add(contentPanel, BorderLayout.CENTER)
        createButtonPanel()
        contentPane.add(btnContentPanel, BorderLayout.SOUTH)
        pack()

        isResizable = false
        EscapeKeyHandler.installHandler(this) { dispose() }
        GuiFunktionen.centerOnScreen(this, false)
    }
}