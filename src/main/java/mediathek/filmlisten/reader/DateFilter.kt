package mediathek.filmlisten.reader

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import java.util.concurrent.TimeUnit

internal class DateFilter(listeFilme: ListeFilme, days: Int) : IDateFilter {
    private val listeFilme: ListeFilme
    private val daysInMillis: Long

    /**
     * @param film film to be checked.
     * @return true if film should be displayed
     */
    private fun checkDate(film: DatenFilm): Boolean {
        val time = film.datumFilm.time
        return time == 0L || time >= System.currentTimeMillis() - daysInMillis
    }

    override fun filter(film: DatenFilm) {
        // do not filter livestreams
        if (film.isLivestream)
            return

        // muss "rückwärts" laufen, da das Datum sonst 2x gebaut werden muss
        // wenns drin bleibt, kann mans noch ändern
        if (!checkDate(film)) {
            listeFilme.remove(film)
        }
    }

    init {
        this.listeFilme = listeFilme
        daysInMillis = TimeUnit.MILLISECONDS.convert(days.toLong(), TimeUnit.DAYS)
    }
}