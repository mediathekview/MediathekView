package mediathek.gui.actions

import mediathek.config.Konstanten
import mediathek.mainwindow.MediathekGui
import mediathek.tool.SVGIconUtilities
import mediathek.tool.SwingErrorDialog
import java.awt.Desktop
import java.awt.event.ActionEvent
import java.io.IOException
import java.net.URI
import java.net.URISyntaxException
import javax.swing.AbstractAction
import javax.swing.JOptionPane

class ShowOnlineFaqAction(private val ui: MediathekGui) : AbstractAction() {
    init {
        putValue(NAME, "Frequently Asked Questions (FAQs) anzeigen...")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"))
    }

    override fun actionPerformed(e: ActionEvent) {
        if (Desktop.isDesktopSupported()) {
            val d = Desktop.getDesktop()
            if (d.isSupported(Desktop.Action.BROWSE)) {
                try {
                    d.browse(URI(Konstanten.ADRESSE_ONLINE_FAQ))
                } catch (ex: IOException) {
                    SwingErrorDialog.showExceptionMessage(
                        ui,
                        "Es trat ein Fehler beim Öffnen der Online-Hilfe auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                        ex
                    )
                } catch (ex: URISyntaxException) {
                    SwingErrorDialog.showExceptionMessage(
                        ui,
                        "Es trat ein Fehler beim Öffnen der Online-Hilfe auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                        ex
                    )
                }
            } else {
                JOptionPane.showMessageDialog(
                    ui,
                    "<html>Ihr Betriebssystem unterstützt das Öffnen des Browsers nicht.<br>" +
                            "Bitte öffnen Sie <b>" + Konstanten.ADRESSE_ONLINE_FAQ + "</b> selbst in Ihrem Browser.</html>",
                    Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE
                )
            }
        }
    }
}
