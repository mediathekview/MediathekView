package mediathek.gui.messages.history

import mediathek.daten.DatenFilm

class FilmSeenStateChangedEvent(
    val seen: Boolean,
    val films: List<DatenFilm>,
) : HistoryChangedEvent()
