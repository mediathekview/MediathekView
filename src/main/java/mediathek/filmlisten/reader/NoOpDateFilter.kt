package mediathek.filmlisten.reader

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme

internal class NoOpDateFilter(listeFilme: ListeFilme) : IDateFilter {
    private val listeFilme: ListeFilme

    override fun filter(film: DatenFilm) {
        // just add the film objet to the list as we are not supposed to do any filtering
        listeFilme.add(film)
    }

    init {
        this.listeFilme = listeFilme
    }
}