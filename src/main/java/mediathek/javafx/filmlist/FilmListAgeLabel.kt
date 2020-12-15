package mediathek.javafx.filmlist

import javafx.animation.Animation
import javafx.animation.KeyFrame
import javafx.animation.Timeline
import javafx.scene.control.Tooltip
import javafx.util.Duration
import mediathek.config.Daten
import mediathek.javafx.tool.ComputedLabel

/**
 * Label which will compute the age of the filmlist when updated.
 * Update cycle one second.
 */
class FilmListAgeLabel internal constructor() : ComputedLabel() {
    private val timeline = Timeline(KeyFrame(Duration.millis(1_000.0), { setAgeToLabel() }))
    private var oldAge = FilmListAge(0, 0)

    private fun setAgeToLabel() {
        val listAge = calculateAge()
        if (listAge != oldAge) {
            setComputedText(computeAgeString(listAge))
            oldAge = listAge
        }
    }

    private data class FilmListAge(val hours: Long, val minutes: Long)

    private fun calculateAge(): FilmListAge {
        val duration = java.time.Duration.ofSeconds(Daten.getInstance().listeFilme.metaData().ageInSeconds)
        var minutes = duration.toMinutes()
        val hours = minutes / 60
        minutes -= hours * 60
        return FilmListAge(hours, minutes)
    }

    private fun computeAgeString(age: FilmListAge): String {
        return try {
            if (age.hours == 0L)
                String.format("Alter: %dm", age.minutes)
            else
                String.format("Alter: %dh %dm", age.hours, age.minutes)
        } catch (ex: IllegalArgumentException) {
            "Ungültiges Alter"
        }
    }

    init {
        timeline.cycleCount = Animation.INDEFINITE
        timeline.play()
        tooltip = Tooltip("Alter der Filmliste")
    }
}