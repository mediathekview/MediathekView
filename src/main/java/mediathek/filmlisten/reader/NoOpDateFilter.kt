package mediathek.filmlisten.reader

import mediathek.daten.DatenFilm

internal class NoOpDateFilter : IDateFilter {
    override fun filter(film: DatenFilm) {
        // do nothing as we aren´t supposed to filter for dates...
    }
}