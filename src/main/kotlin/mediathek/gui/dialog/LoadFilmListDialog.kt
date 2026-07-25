package mediathek.gui.dialog

import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.filmlisten.FilmCatalog
import mediathek.filmlisten.FilmListLoadCoordinator
import mediathek.gui.actions.DisposeDialogAction
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden
import mediathek.swing.centerOnScreen
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.FilmListUpdateType
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

class LoadFilmListDialog(
    owner: Frame?,
    filmCatalog: FilmCatalog,
    private val filmListLoader: FilmListLoadCoordinator,
) : JDialog(owner, "Filmliste laden", true) {
    private val contentPanel: PanelFilmlisteLaden
    private val logger: Logger = LogManager.getLogger()
    private val btnContentPanel = ButtonPanel()

    private fun createButtonPanel() {
        val buttonFlowPanel = ButtonFlowPanel()
        btnContentPanel.add(buttonFlowPanel, BorderLayout.EAST)

        val closeBtn = JButton(DisposeDialogAction(this, "Schließen", "Dialog schließen"))
        buttonFlowPanel.add(closeBtn)
        rootPane.defaultButton = closeBtn

        val btn = JButton("Filmliste laden")
        btn.addActionListener {
            val immerNeuLaden = contentPanel.hasSenderSelectionChanged()
            if (immerNeuLaden && !contentPanel.updateCheckBox.isSelected) {
                logger.trace("Sender list was changed loading full list...")
            }

            if (FilmListUpdateType.fromConfig() == FilmListUpdateType.AUTOMATIC) {
                //easy, just load
                filmListLoader.startFilmlistLoad("", immerNeuLaden)
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
                    filmListLoader.startFilmlistUpdate(strUrl)
                else
                    filmListLoader.startFilmlistLoad(strUrl, immerNeuLaden)
            }
            dispose()
        }

        buttonFlowPanel.add(btn)
    }

    private fun restoreWindowSizeFromConfig() {
        val state = ApplicationConfiguration.getInstance().loadFilmListDialogState
        if (state.hasStoredBounds()) {
            setBounds(state.x, state.y, state.width, state.height)
        } else {
            pack()
            if (width < 100 || height < 100) {
                setSize(640, 480)
            }
            centerOnScreen()
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
                val component = e.component
                val dims = component.size
                val loc = component.location
                ApplicationConfiguration.getInstance()
                    .setLoadFilmListDialogBounds(loc.x, loc.y, dims.width, dims.height)
            }
        })
    }

    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        contentPane.layout = BorderLayout()
        contentPanel = PanelFilmlisteLaden(false, requireNotNull(owner), filmCatalog, filmListLoader)
        val scrollPane = JScrollPane(contentPanel)
        contentPane.add(scrollPane, BorderLayout.CENTER)
        createButtonPanel()
        contentPane.add(btnContentPanel, BorderLayout.SOUTH)

        restoreWindowSizeFromConfig()
        registerWindowSizeListener()

        EscapeKeyHandler.installHandler(this) { dispose() }
    }
}
