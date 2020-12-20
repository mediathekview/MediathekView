package mediathek.tool.swing

import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.mainwindow.MediathekGui
import org.apache.logging.log4j.LogManager
import java.awt.Cursor
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.net.URI
import java.net.URISyntaxException
import java.util.concurrent.ExecutionException
import javax.swing.JLabel
import javax.swing.SwingWorker

/**
 * Decorate a [JLabel] as a hyperlink.
 */
object HyperlinkDecorator {
    private const val A_HREF = "<a href=\""
    private const val HREF_CLOSED = "\">"
    private const val HREF_END = "</a>"
    private const val HTML = "<html>"
    private const val HTML_END = "</html>"
    private val logger = LogManager.getLogger()
    private fun makeLinkable(c: JLabel, ml: MouseListener) {
        c.text = placeInsideHtmlTag(createHtmlReference(c.text))
        c.cursor = Cursor(Cursor.HAND_CURSOR)
        c.addMouseListener(ml)
    }

    @JvmStatic
    fun makeLinkable(c: JLabel, link: String) {
        try {
            makeLinkable(c, LinkMouseListener(link))
            c.toolTipText = link
        } catch (e: URISyntaxException) {
            logger.error(e)
        }
    }

    private fun createHtmlReference(s: String): String {
        return A_HREF + s + HREF_CLOSED + s + HREF_END
    }

    private fun placeInsideHtmlTag(s: String): String {
        return HTML + s + HTML_END
    }

    private class LinkMouseListener(link: String) : MouseAdapter() {
        private val link: URI = URI(link)
        override fun mouseClicked(evt: MouseEvent) {
            LinkRunner(link).execute()
        }
    }

    private class LinkRunner(u: URI) : SwingWorker<Void?, Void?>() {
        private val uri: URI = u

        @Throws(Exception::class)
        override fun doInBackground(): Void? {
            UrlHyperlinkAction.openURL(MediathekGui.ui(), uri.toString())
            return null
        }

        override fun done() {
            try {
                get()
            } catch (ee: ExecutionException) {
                logger.error(ee)
            } catch (ee: InterruptedException) {
                logger.error(ee)
            }
        }

    }
}