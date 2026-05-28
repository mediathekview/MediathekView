package mediathek.gui.actions

import mediathek.config.Konstanten
import mediathek.config.StandardLocations
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FileUtils
import java.awt.event.ActionEvent
import java.nio.file.Paths
import javax.swing.AbstractAction
import javax.swing.JFrame
import javax.swing.JOptionPane

class DeleteLocalFilmlistAction(
    parent: MediathekGui,
) : AbstractAction() {
    private val owner: JFrame = parent

    init {
        putValue(NAME, "Lokale Filmliste löschen")
    }

    override fun actionPerformed(event: ActionEvent?) {
        val filmlistPath = Paths.get(StandardLocations.getFilmlistFilePathString())
        FileUtils.moveToTrash(filmlistPath)
        JOptionPane.showMessageDialog(
            owner,
            "Filmliste wurde gelöscht.\nDas Programm wird nun beendet.",
            Konstanten.PROGRAMMNAME,
            JOptionPane.INFORMATION_MESSAGE,
        )
        MediathekGui.ui().quitApplication()
    }
}
