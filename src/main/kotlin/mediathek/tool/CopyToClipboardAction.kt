package mediathek.tool

import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class CopyToClipboardAction(private val webSiteUrl: String) : AbstractAction() {
    init {
        putValue(NAME, "In Zwischenablage kopieren")
    }

    override fun actionPerformed(e: ActionEvent) {
        try {
            GuiFunktionen.copyToClipboard(webSiteUrl)
        } catch (ignored: Exception) {
        }
    }
}
