package mediathek.mainwindow

import mediathek.config.Daten
import org.apache.commons.lang3.time.DurationFormatUtils
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.time.Duration
import java.util.*
import java.util.concurrent.TimeUnit
import javax.swing.JLabel
import javax.swing.Timer

class FilmAgeLabel : JLabel(), ActionListener {
    private data class FilmListAge(val hours: Long, val minutes: Long)

    private var oldAge = FilmListAge(0, 0)

    init {
        toolTipText = "Alter der Filmliste"
        setAgeToLabel()

        //start the update timer
        val timer = Timer(1000, this)
        timer.isRepeats = true
        timer.start()
    }

    private fun calculateFilmListAge(): FilmListAge {
        val duration = Duration.ofSeconds(Daten.getInstance().listeFilme.metaData.ageInSeconds)
        var minutes = duration.toMinutes()
        val hours = minutes / 60
        minutes -= hours * 60
        return FilmListAge(hours, minutes)
    }

    @Throws(IllegalFormatException::class)
    private fun computeAgeString(age: FilmListAge): String {
        return if (age.hours == 0L) {
            "Alter: ${age.minutes}m"
        } else if (age.hours >= 24) {
            val duration = TimeUnit.MILLISECONDS.convert(age.hours * 60 + age.minutes, TimeUnit.MINUTES)
            DurationFormatUtils.formatDuration(duration, "'Alter: 'dd'd' HH'h' mm'm'", true)
        } else {
            "Alter: ${age.hours}h ${age.minutes}m"
        }
    }

    private fun setAgeToLabel() {
        val curAge = calculateFilmListAge()
        if (curAge != oldAge) {
            val result = computeAgeString(curAge)
            text = result
            oldAge = curAge
        }
    }

    override fun actionPerformed(e: ActionEvent) {
        setAgeToLabel()
    }
}