package mediathek.gui.tasks

import mediathek.config.Daten
import mediathek.config.StandardLocations.getFilmlistFilePath
import mediathek.filmlisten.writer.FilmListWriter
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import javax.swing.JLabel
import javax.swing.JProgressBar
import javax.swing.SwingUtilities
import javax.swing.SwingWorker
import kotlin.math.roundToInt

class FilmlistWriterWorker(progLabel: JLabel, private val progressBar: JProgressBar) : SwingWorker<Void?, Int?>(),
    PropertyChangeListener {
    init {
        addPropertyChangeListener(this)
        SwingUtilities.invokeLater {
            progLabel.text = "Schreibe Filmliste"
            progressBar.isIndeterminate = false
            progressBar.minimum = 0
            progressBar.maximum = 100
            progressBar.value = 0
        }
    }

    override fun doInBackground(): Void? {
        val writer = FilmListWriter(false)
        writer.writeFilmList(getFilmlistFilePath(), Daten.getInstance().listeFilme)
        { prog: Double ->
            progress = (100.0 * prog).roundToInt()
        }
        return null
    }

    override fun propertyChange(evt: PropertyChangeEvent) {
        if (evt.propertyName.equals("progress", ignoreCase = true)) {
            SwingUtilities.invokeLater { progressBar.value = evt.newValue as Int }
        }
    }
}