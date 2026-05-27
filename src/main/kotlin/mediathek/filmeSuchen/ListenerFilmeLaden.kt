package mediathek.filmeSuchen

import java.util.EventListener

open class ListenerFilmeLaden : EventListener {
    open fun start(event: ListenerFilmeLadenEvent) = Unit

    open fun progress(event: ListenerFilmeLadenEvent) = Unit

    open fun fertig(event: ListenerFilmeLadenEvent) = Unit

    // dient dem Melden des ersten Mal Laden der Filmliste beim ProgStart
    open fun fertigOnlyOne(event: ListenerFilmeLadenEvent) = Unit
}
