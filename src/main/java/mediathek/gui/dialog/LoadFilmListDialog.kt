package mediathek.gui.dialog

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.gui.actions.DisposeDialogAction
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.FilmListUpdateType
import mediathek.tool.GuiFunktionen
import org.apache.commons.configuration2.sync.LockMode
import java.awt.BorderLayout
import java.awt.Frame
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.util.*
import javax.swing.JButton
import javax.swing.JDialog
import javax.swing.JOptionPane

class LoadFilmListDialog(owner: Frame?) : JDialog(owner, "Filmliste laden", true) {
    private val contentPanel: PanelFilmlisteLaden

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
                val strUrl = contentPanel.jTextFieldUrl.text
                if (strUrl.contains("mediathekview.de", true)) {
                    JOptionPane.showMessageDialog(this, """
                        Bitte vermeiden Sie das Laden der Filmliste von unseren Servern über eine manuell eingegebene URL.
                        
                        Sie umgehen damit unter Umständen Mechanismen, die eine Lastverteilung auf unseren Servern ermöglichen
                        oder erhalten veraltete Dateien.
                        
                        Nutzen Sie diese Möglichkeit NUR, wenn der reguläre Download OHNE manuelle Adresse nicht funktioniert.
                        Sie können sicher sein, dass wir einen Fehler schnellstmöglich beheben werden.
                    """.trimIndent(), Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE)
                }
                if (contentPanel.jCheckBoxUpdate.isSelected)
                    filmeLaden.updateFilmlist(strUrl)
                else
                    filmeLaden.loadFilmlist(strUrl, false)
            }
            dispose()
        }

        buttonFlowPanel.add(btn)
    }

    private fun restoreWindowSizeFromConfig() {
        val config = ApplicationConfiguration.getConfiguration()
        try {
            config.lock(LockMode.READ)
            val width = config.getInt(ApplicationConfiguration.LoadFilmListDialog.WIDTH)
            val height = config.getInt(ApplicationConfiguration.LoadFilmListDialog.HEIGHT)
            val x = config.getInt(ApplicationConfiguration.LoadFilmListDialog.X)
            val y = config.getInt(ApplicationConfiguration.LoadFilmListDialog.Y)
            setBounds(x, y, width, height)
        } catch (ignored: NoSuchElementException) {
            pack()
            if (width < 100 || height < 100) {
                setSize(640,480)
            }
            GuiFunktionen.centerOnScreen(this, false)
        } finally {
            config.unlock(LockMode.READ)
        }
    }

    private fun registerWindowSizeListener() {
        addComponentListener(object : ComponentAdapter() {
            override fun componentResized(e: ComponentEvent) {
                storeWindowPosition(e)
            }

            override fun componentMoved(e: ComponentEvent) {
                storeWindowPosition(e)
            }

            private fun storeWindowPosition(e: ComponentEvent) {
                val config = ApplicationConfiguration.getConfiguration()
                val component = e.component
                val dims = component.size
                val loc = component.location
                try {
                    config.lock(LockMode.WRITE)
                    config.setProperty(ApplicationConfiguration.LoadFilmListDialog.WIDTH, dims.width)
                    config.setProperty(ApplicationConfiguration.LoadFilmListDialog.HEIGHT, dims.height)
                    config.setProperty(ApplicationConfiguration.LoadFilmListDialog.X, loc.x)
                    config.setProperty(ApplicationConfiguration.LoadFilmListDialog.Y, loc.y)
                } finally {
                    config.unlock(LockMode.WRITE)
                }
            }
        })
    }

    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        contentPane.layout = BorderLayout()
        contentPanel = PanelFilmlisteLaden(false)
        contentPane.add(contentPanel, BorderLayout.CENTER)
        createButtonPanel()
        contentPane.add(btnContentPanel, BorderLayout.SOUTH)

        restoreWindowSizeFromConfig()
        registerWindowSizeListener()

        EscapeKeyHandler.installHandler(this) { dispose() }
    }
}