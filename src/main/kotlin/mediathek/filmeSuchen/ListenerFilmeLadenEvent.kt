package mediathek.filmeSuchen

class ListenerFilmeLadenEvent(
    var senderUrl: String,
    var text: String,
    var max: Int,
    var progress: Int,
    val fehler: Boolean,
)
