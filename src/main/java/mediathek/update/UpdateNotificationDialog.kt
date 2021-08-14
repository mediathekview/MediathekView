package mediathek.update

import javafx.application.Platform
import javafx.scene.Scene
import javafx.scene.web.WebEngine
import javafx.scene.web.WebView
import mediathek.config.Konstanten
import mediathek.gui.actions.DisposeDialogAction
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.gui.dialog.ButtonFlowPanel
import mediathek.gui.dialog.ButtonPanel
import mediathek.mainwindow.MediathekGui
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.GuiFunktionen
import mediathek.tool.Version
import org.apache.logging.log4j.LogManager
import java.awt.BorderLayout
import java.awt.Frame
import java.net.URI
import java.net.URISyntaxException
import java.util.*
import javax.swing.JButton
import javax.swing.JDialog

class UpdateNotificationDialog(owner: Frame?, title: String?, private val version: Version) :
    JDialog(owner, title, true) {
    private val panel = UpdateNotificationPanel()
    private var browser: WebView? = null
    private lateinit var webEngine: WebEngine

    private fun createButtonPanel(): ButtonFlowPanel {
        val pnl = ButtonFlowPanel()
        val dlBtn = JButton("Download")
        dlBtn.addActionListener {
            try {
                val uri = URI(Konstanten.ADRESSE_DOWNLOAD)
                UrlHyperlinkAction.openURL(MediathekGui.ui(), uri.toString())
                dispose()
            } catch (e: URISyntaxException) {
                logger.error(e)
            }
        }
        val btn = JButton(DisposeDialogAction(this, "Schließen", "Dialog schließen"))
        getRootPane().defaultButton = btn
        pnl.add(dlBtn)
        pnl.add(btn)
        return pnl
    }

    private fun setupFxWebView() {
        Platform.runLater {
            browser = WebView()
            val scene = Scene(browser)
            webEngine = browser!!.engine
            webEngine.load(Objects.requireNonNull(Konstanten.WEBSITE_BASE_URL.resolve("changelogs")).toString())
            panel.fxPanel.scene = scene
        }
    }

    private fun setupDialogInformation() {
        val label = ("MediathekView " + version + " ist verfügbar - "
                + "Sie haben Version " + Konstanten.MVVERSION)
        panel.releaseInfoLabel.text = label
    }

    companion object {
        private val logger = LogManager.getLogger()
    }

    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        EscapeKeyHandler.installHandler(this) { dispose() }
        setupDialogInformation()
        setupFxWebView()
        val contentPane = contentPane
        contentPane.layout = BorderLayout()
        contentPane.add(panel, BorderLayout.CENTER)
        val buttonPanel = ButtonPanel()
        buttonPanel.add(createButtonPanel(), BorderLayout.EAST)
        contentPane.add(buttonPanel, BorderLayout.SOUTH)
        pack()
        GuiFunktionen.centerOnScreen(this, false)
    }
}