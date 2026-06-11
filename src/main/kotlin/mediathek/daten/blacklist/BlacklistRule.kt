package mediathek.daten.blacklist

data class BlacklistRule(
    var sender: String = "",
    var thema: String = "",
    var titel: String = "",
    var thema_titel: String = ""
)
