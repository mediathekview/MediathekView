package mediathek.daten.blacklist

data class BlacklistRule(
    var sender: String = "",
    var thema: String = "",
    var titel: String = "",
    var thema_titel: String = "",
    var active: Boolean = true,
) {
    fun criteria(): BlacklistRuleCriteria =
        BlacklistRuleCriteria(sender, thema, titel, thema_titel)

    fun hasSameCriteria(other: BlacklistRule): Boolean =
        criteria() == other.criteria()
}

data class BlacklistRuleCriteria(
    val sender: String,
    val thema: String,
    val titel: String,
    val thema_titel: String,
)
