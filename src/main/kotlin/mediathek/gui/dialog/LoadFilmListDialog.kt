package mediathek.gui.dialog

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.gui.actions.DisposeDialogAction
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden
import mediathek.tool.*
import org.apache.commons.configuration2.sync.LockMode
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.awt.BorderLayout
import java.awt.Frame
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import javax.swing.JButton
import javax.swing.JDialog
import javax.swing.JOptionPane
import javax.swing.JScrollPane

class LoadFilmListDialog(owner: Frame?) : JDialog(owner, "Filmliste laden", true) {
    private val contentPanel: PanelFilmlisteLaden
    private val logger: Logger = LogManager.getLogger()
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
            val immerNeuLaden = contentPanel.hasSenderSelectionChanged()
            if (immerNeuLaden && !contentPanel.updateCheckBox.isSelected) {
                logger.trace("Sender list was changed loading full list...")
            }

            if (GuiFunktionen.getFilmListUpdateType() == FilmListUpdateType.AUTOMATIC) {
                //easy, just load
                filmeLaden.loadFilmlist("", immerNeuLaden)
            } else {
                //manual or extend
                val strUrl = contentPanel.urlTextField.text
                if (strUrl.contains("mediathekview.de", true)) {
                    JOptionPane.showMessageDialog(
                        this, """
                        Bitte vermeiden Sie das Laden der Filmliste von unseren Servern über eine manuell eingegebene URL.
                        
                        Sie umgehen damit unter Umständen Mechanismen, die eine Lastverteilung auf unseren Servern ermöglichen
                        oder erhalten veraltete Dateien.
                        
                        Nutzen Sie diese Möglichkeit NUR, wenn der reguläre Download OHNE manuelle Adresse nicht funktioniert.
                        Sie können sicher sein, dass wir einen Fehler schnellstmöglich beheben werden.
                    """.trimIndent(), Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE
                    )
                }
                if (contentPanel.updateCheckBox.isSelected)
                    filmeLaden.updateFilmlist(strUrl)
                else
                    filmeLaden.loadFilmlist(strUrl, immerNeuLaden)
            }
            dispose()
        }

        buttonFlowPanel.add(btn)
    }

    private fun restoreWindowSizeFromConfig() {
        val config = ApplicationConfiguration.getConfiguration()
        try {
            config.withLock(LockMode.READ) {
                val width = getInt(ApplicationConfiguration.LoadFilmListDialog.WIDTH)
                val height = getInt(ApplicationConfiguration.LoadFilmListDialog.HEIGHT)
                val x = getInt(ApplicationConfiguration.LoadFilmListDialog.X)
                val y = getInt(ApplicationConfiguration.LoadFilmListDialog.Y)
                setBounds(x, y, width, height)
            }
        } catch (_: NoSuchElementException) {
            pack()
            if (width < 100 || height < 100) {
                setSize(640, 480)
            }
            GuiFunktionen.centerOnScreen(this, false)
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
                config.withLock(LockMode.WRITE) {
                    setProperty(ApplicationConfiguration.LoadFilmListDialog.WIDTH, dims.width)
                    setProperty(ApplicationConfiguration.LoadFilmListDialog.HEIGHT, dims.height)
                    setProperty(ApplicationConfiguration.LoadFilmListDialog.X, loc.x)
                    setProperty(ApplicationConfiguration.LoadFilmListDialog.Y, loc.y)
                }
            }
        })
    }

    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        contentPane.layout = BorderLayout()
        contentPanel = PanelFilmlisteLaden(false)
        val scrollPane = JScrollPane(contentPanel)
        contentPane.add(scrollPane, BorderLayout.CENTER)
        createButtonPanel()
        contentPane.add(btnContentPanel, BorderLayout.SOUTH)

        restoreWindowSizeFromConfig()
        registerWindowSizeListener()

        EscapeKeyHandler.installHandler(this) { dispose() }
    }
}
