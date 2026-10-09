package mediathek.gui.tabs.tab_online_search

import kotlinx.serialization.json.Json

class OnlineSearchHistory private constructor(
    val entries: List<String>,
) {
    fun withEntry(entry: String, maxSize: Int = DEFAULT_MAX_SIZE): OnlineSearchHistory {
        val normalized = entry.trim()
        if (normalized.isEmpty()) return this
        val updated = listOf(normalized) + entries.filterNot { it.equals(normalized, ignoreCase = true) }
        return OnlineSearchHistory(updated.take(maxSize))
    }

    fun encode(): String = Json.encodeToString(entries)

    companion object {
        private const val DEFAULT_MAX_SIZE = 20

        fun of(entries: List<String>): OnlineSearchHistory = OnlineSearchHistory(entries)

        fun decode(raw: String): OnlineSearchHistory = runCatching {
            OnlineSearchHistory(Json.decodeFromString<List<String>>(raw))
        }.getOrElse { OnlineSearchHistory(emptyList()) }
    }
}
