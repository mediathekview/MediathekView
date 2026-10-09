package mediathek.gui.duplicates

import ca.odell.glazedlists.TransactionList
import mediathek.daten.DatenFilm
import mediathek.tool.withWriteLock
import java.util.stream.Collectors
import java.util.stream.Stream

internal fun Stream<DatenFilm>.countFilmsBySender(): Map<String, Long> =
    collect(Collectors.groupingBy({ film: DatenFilm -> film.sender }, Collectors.counting()))

internal fun replaceFilmStatistics(
    statisticsList: TransactionList<FilmStatistics>,
    statistics: Map<String, Long>,
) {
    statisticsList.withWriteLock {
        statisticsList.withTransaction {
            clear()
            statistics.forEach { (sender, count) ->
                add(FilmStatistics(sender, count))
            }
        }
    }
}
