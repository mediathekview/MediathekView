package mediathek.gui.duplicates

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.TransactionList
import mediathek.daten.DatenFilm
import java.util.stream.Collectors
import java.util.stream.Stream

internal fun Stream<DatenFilm>.countFilmsBySender(): Map<String, Long> =
    collect(Collectors.groupingBy({ film: DatenFilm -> film.sender }, Collectors.counting()))

internal fun replaceFilmStatistics(
    statisticsList: EventList<FilmStatistics>,
    statistics: Map<String, Long>,
) {
    val transactionList = TransactionList(statisticsList)
    val writeLock = transactionList.readWriteLock.writeLock()
    writeLock.lock()
    try {
        transactionList.beginEvent(true)
        transactionList.clear()
        statistics.forEach { (sender, count) ->
            transactionList.add(FilmStatistics(sender, count))
        }
        transactionList.commitEvent()
    } finally {
        writeLock.unlock()
    }
}
