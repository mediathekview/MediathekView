package mediathek.gui.actions

import mediathek.gui.tabs.tab_downloads.GuiDownloads
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class DeleteDownloadAction(private val guiDownloads: GuiDownloads) : AbstractAction() {
    init {
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/xmark.svg"))
        putValue(NAME, "Gespeicherten Film (Datei) löschen")
    }

    override fun actionPerformed(e: ActionEvent?) {
        guiDownloads.deleteDownloadedFile()
    }
}
