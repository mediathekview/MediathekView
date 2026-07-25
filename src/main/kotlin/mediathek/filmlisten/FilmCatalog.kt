package mediathek.filmlisten

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.TransactionList
import mediathek.daten.ListeFilme
import mediathek.gui.duplicates.FilmStatistics
import mediathek.tool.SenderListBoxModel

class FilmCatalog {
    val allFilms: ListeFilme = ListeFilme()
    val allSenders: List<String> = SenderListBoxModel.providedSenders
    val duplicateStatistics = TransactionList<FilmStatistics>(BasicEventList())
    val commonStatistics = TransactionList<FilmStatistics>(BasicEventList())

    /**
     * The final list of films after all filtering is done.
     * Defaults to no Lucene index unless changed at startup.
     */
    var filteredFilms: ListeFilme = ListeFilme()
}
