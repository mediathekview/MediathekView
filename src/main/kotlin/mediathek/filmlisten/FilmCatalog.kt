package mediathek.filmlisten

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.SortedList
import mediathek.daten.ListeFilme
import mediathek.gui.duplicates.FilmStatistics
import mediathek.tool.GermanStringSorter
import mediathek.tool.SenderListBoxModel

class FilmCatalog {
    val allFilms: ListeFilme = ListeFilme()
    val allSendersList: EventList<String> = SortedList(SenderListBoxModel.providedSenderList).apply {
        setComparator(GermanStringSorter)
    }
    val duplicateStatistics: EventList<FilmStatistics> = BasicEventList()
    val commonStatistics: EventList<FilmStatistics> = BasicEventList()

    /**
     * The final list of films after all filtering is done.
     * Defaults to no Lucene index unless changed at startup.
     */
    var filteredFilms: ListeFilme = ListeFilme()
}
