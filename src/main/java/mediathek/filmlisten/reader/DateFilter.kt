package mediathek.filmlisten.reader

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.tool.datum.DateUtil
import java.time.LocalDate

internal class DateFilter(private val listeFilme: ListeFilme, days: Long) : IDateFilter {
    private val cutoffDate: LocalDate = LocalDate.now().minusDays(days)

    private fun isBeforeOrEqual(date: LocalDate, compareToDate: LocalDate): Boolean {
        return !compareToDate.isAfter(date)
    }

    override fun filter(film: DatenFilm) {
        // do not filter livestreams
        val localDate = DateUtil.convertToLocalDate(film.datumFilm)
        if (film.isLivestream || isBeforeOrEqual(localDate, cutoffDate)) {
            listeFilme.add(film)
        }
    }
}