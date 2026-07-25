package mediathek.mainwindow

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.Country
import mediathek.gui.actions.UrlHyperlinkAction
import org.apache.logging.log4j.LogManager
import java.awt.Font
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.JEditorPane
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.UIManager
import javax.swing.event.HyperlinkEvent
import kotlin.time.Duration.Companion.seconds

class AustrianVlcCheck(val owner: JFrame) {
    private val job = SupervisorJob()
    private val scope = CoroutineScope(job + Dispatchers.Swing)
    private var pendingDialogJob: Job? = null

    init {
        owner.addWindowListener(object : WindowAdapter() {
            override fun windowClosed(e: WindowEvent) {
                job.cancel()
            }
        })
    }

    private fun getFontWeight(font: Font): String {
        return if (font.isBold) "bold" else "normal"
    }

    private fun showSwingMessage() {
        val font = UIManager.getFont("Label.font")
        val boldFont = getFontWeight(font)
        val style = "font-family:${font.family};font-weight:$boldFont;font-size: ${font.size}pt;"
        val msg =
            "<html><body style=\"$style\">Um den ORF erfolgreich zu nutzen sind zusätzliche Einstellungen erforderlich.<br>Bitte lesen Sie sich hierzu den <a href=\"\">Link</a> durch.</body></html>"

        val ep = JEditorPane("text/html", msg)
        ep.isEditable = false
        ep.background = UIManager.getColor("Label.background")
        ep.addHyperlinkListener { e: HyperlinkEvent ->
            if (e.eventType == HyperlinkEvent.EventType.ACTIVATED) {
                try {
                    ApplicationConfiguration.getInstance().showOrfConfigHelp = false
                    UrlHyperlinkAction.openURL(Konstanten.ORF_TUTORIAL_LINK)
                } catch (ex: Exception) {
                    logger.error("Failed to display ORF tutorial", ex)
                }
            }
        }

        JOptionPane.showMessageDialog(owner, ep, "Wichtige Information für ORF", JOptionPane.WARNING_MESSAGE)
    }

    fun perform() {
        logger.trace("ORF setup tutorial display check started")
        pendingDialogJob?.cancel()

        if (shouldShowOrfHint()) {
            pendingDialogJob = scope.launch {
                delay(5.seconds)
                if (shouldShowOrfHint() && owner.isDisplayable) {
                    showSwingMessage()
                }
            }
        }
        logger.trace("ORF setup tutorial display check finished")
    }

    private fun shouldShowOrfHint(): Boolean {
        return ApplicationConfiguration.getInstance().showOrfConfigHelp &&
            ApplicationConfiguration.getInstance().geographicLocation == Country.AT
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}
