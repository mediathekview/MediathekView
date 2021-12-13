package mediathek.filmlisten.reader

import mediathek.daten.DatenFilm

internal interface IDateFilter {
    fun filter(film: DatenFilm)
}