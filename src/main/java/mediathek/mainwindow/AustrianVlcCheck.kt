package mediathek.mainwindow

import mediathek.config.Konstanten
import mediathek.daten.Country
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.TimerPool
import org.apache.logging.log4j.LogManager
import java.awt.Font
import java.util.concurrent.TimeUnit
import javax.swing.*
import javax.swing.event.HyperlinkEvent

class AustrianVlcCheck(val owner: JFrame) {
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
        ep.setBackground(UIManager.getColor("Label.background"))
        ep.addHyperlinkListener { e: HyperlinkEvent ->
            if (e.eventType == HyperlinkEvent.EventType.ACTIVATED) {
                try {
                    ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_SHOW_ORF_CONFIG_HELP, false)
                    UrlHyperlinkAction.openURL(MediathekGui.ui(), Konstanten.ORF_TUTORIAL_LINK)
                } catch (ex: Exception) {
                    logger.error("Failed to display ORF tutorial", ex)
                }
            }
        }

        JOptionPane.showMessageDialog(owner, ep, "Wichtige Information für ORF", JOptionPane.WARNING_MESSAGE)
    }

    fun perform() {
        logger.trace("ORF setup tutorial display check started")
        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_SHOW_ORF_CONFIG_HELP, true)) {
            //we haven´t shown the config help dialog before
            if (ApplicationConfiguration.getInstance().geographicLocation == Country.AT) {
                logger.trace("Launching info dialog in 15 seconds...")
                TimerPool.timerPool.schedule({
                    SwingUtilities.invokeLater { showSwingMessage() }
                }, 15, TimeUnit.SECONDS)
            }
        }
        logger.trace("ORF setup tutorial display check finished")
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}