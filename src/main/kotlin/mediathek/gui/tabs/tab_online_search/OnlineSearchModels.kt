package mediathek.gui.tabs.tab_online_search

import java.time.Duration
import java.time.LocalDateTime

enum class OnlineSearchProvider(val displayName: String) {
    ARD("ARD"),
    ZDF("ZDF"),
}

data class OnlineSearchRequest(
    val provider: OnlineSearchProvider,
    val query: String,
    val nextToken: String? = null,
)

data class OnlineUrlRequest(
    val provider: OnlineSearchProvider,
    val url: String,
)

interface OnlineSearchService {
    suspend fun search(request: OnlineSearchRequest): OnlineSearchPage
    suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult?
}

data class OnlineSearchPage(
    val results: List<OnlineSearchResult>,
    val nextToken: String?,
    val totalResults: Long? = null,
) {
    val hasNextPage: Boolean
        get() = !nextToken.isNullOrBlank()
}

data class OnlineSearchResult(
    val provider: OnlineSearchProvider,
    val sender: String,
    val topic: String,
    val title: String,
    val description: String = "",
    val websiteUrl: String = "",
    val normalQualityUrl: String,
    val lowQualityUrl: String = "",
    val highQualityUrl: String = "",
    val subtitleUrl: String = "",
    val broadcastTime: LocalDateTime? = null,
    val duration: Duration? = null,
    val isSignLanguage: Boolean = false,
    val isAudioDescription: Boolean = false,
) {
    val displayTopic: String
        get() = topic.ifBlank { title }

    val hasHighQuality: Boolean
        get() = highQualityUrl.isNotBlank()

    val hasSubtitle: Boolean
        get() = subtitleUrl.isNotBlank()
}
