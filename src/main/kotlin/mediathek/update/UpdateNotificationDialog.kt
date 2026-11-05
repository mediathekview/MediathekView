package mediathek.update

import mediathek.config.Konstanten
import mediathek.gui.actions.DisposeDialogAction
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.gui.dialog.ButtonFlowPanel
import mediathek.gui.dialog.ButtonPanel
import mediathek.i18n.UiMessages
import mediathek.swing.centerOnScreen
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.Version
import org.apache.logging.log4j.LogManager
import java.awt.BorderLayout
import java.awt.Frame
import java.net.URI
import java.net.URISyntaxException
import javax.swing.JButton
import javax.swing.JDialog

class UpdateNotificationDialog(owner: Frame?, title: String?, private val version: Version) :
    JDialog(owner, title, true) {
    private val panel = UpdateNotificationPanel()

    private fun createButtonPanel(): ButtonFlowPanel {
        val pnl = ButtonFlowPanel()
        val dlBtn = JButton(UiMessages.text("update.download"))
        dlBtn.addActionListener {
            try {
                UrlHyperlinkAction.openURI(URI(Konstanten.ADRESSE_DOWNLOAD))
                dispose()
            } catch (e: URISyntaxException) {
                logger.error(e)
            }
        }
        val btn = JButton(DisposeDialogAction(this, UiMessages.text("update.close"), UiMessages.text("update.closeHint")))
        rootPane.defaultButton = btn
        pnl.add(dlBtn)
        pnl.add(btn)
        return pnl
    }

    private fun setupDialogInformation() {
        val label = UiMessages.format("update.available", version.toString(), Konstanten.MVVERSION.toString())
        panel.releaseInfoLabel.text = label
    }

    companion object {
        private val logger = LogManager.getLogger()
    }

    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        EscapeKeyHandler.installHandler(this) { dispose() }
        setupDialogInformation()

        val contentPane = contentPane
        contentPane.layout = BorderLayout()
        contentPane.add(panel, BorderLayout.CENTER)
        val buttonPanel = ButtonPanel()
        buttonPanel.add(createButtonPanel(), BorderLayout.EAST)
        contentPane.add(buttonPanel, BorderLayout.SOUTH)
        pack()
        centerOnScreen()
    }
}
