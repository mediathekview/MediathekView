package mediathek.daten.blacklist

@JvmRecord
data class CompiledBlacklistRule(
    val rule: BlacklistRule,
    val pTitel: Array<String>,
    val pThema: Array<String>,
)
