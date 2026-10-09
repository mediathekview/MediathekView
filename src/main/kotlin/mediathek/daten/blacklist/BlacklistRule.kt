package mediathek.daten.blacklist

data class BlacklistRule(
    var sender: String = "",
    var thema: String = "",
    var titel: String = "",
    var topicTitle: String = "",
    var active: Boolean = true,
) {
    fun criteria(): BlacklistRuleCriteria =
        BlacklistRuleCriteria(sender, thema, titel, topicTitle)

    fun hasSameCriteria(other: BlacklistRule): Boolean =
        criteria() == other.criteria()
}

data class BlacklistRuleCriteria(
    val sender: String,
    val thema: String,
    val titel: String,
    val topicTitle: String,
)
