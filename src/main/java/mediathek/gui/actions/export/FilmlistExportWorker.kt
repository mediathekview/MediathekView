package mediathek.gui.actions.export

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.filmlisten.writer.FilmListWriter
import mediathek.mainwindow.MediathekGui
import java.io.File
import javax.swing.AbstractAction
import javax.swing.JOptionPane
import javax.swing.SwingWorker
import kotlin.math.roundToInt

class FilmlistExportWorker(private val exportAction: AbstractAction, selectedFile: File,
                           compressSender: Boolean, compressThema: Boolean) : SwingWorker<Boolean, Double?>() {
    private val selectedFile: File?
    private val compressSender: Boolean
    private val compressThema: Boolean

    init {
        this.selectedFile = selectedFile
        this.compressSender = compressSender
        this.compressThema = compressThema
    }

    private fun showError() {
        JOptionPane.showMessageDialog(MediathekGui.ui(),
                                      "Es gab einen Fehler beim Export der Filmliste.",
                                      Konstanten.PROGRAMMNAME,
                                      JOptionPane.ERROR_MESSAGE)
    }

    private fun showSuccess() {
        JOptionPane.showMessageDialog(MediathekGui.ui(),
                                      "Der Export wurde erfolgreich abgeschlossen.",
                                      Konstanten.PROGRAMMNAME,
                                      JOptionPane.INFORMATION_MESSAGE)
    }

    override fun done() {
        try {
            val result = get()
            if (result!!)
                showSuccess()
            else
                showError()
        }
        catch (e: Exception) {
            showError()
        }
        exportAction.isEnabled = true
    }

    @Throws(Exception::class)
    override fun doInBackground(): Boolean {
        if (selectedFile != null) {
            val writer = FilmListWriter(true)
            writer.setCompressSenderTag(compressSender)
            writer.setCompressThemaTag(compressThema)
            writer.setDecompressUrls(true)
            writer.writeFilmList(selectedFile.absolutePath,
                                 Daten.getInstance().listeFilme)
            { prog: Double -> progress = (100.0 * prog).roundToInt() }
        }
        return true
    }
}