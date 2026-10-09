package mediathek.gui.tabs.tab_online_search

import org.jsoup.Jsoup
import java.time.Instant
import java.time.OffsetDateTime

data class ZdfApiToken(
    val value: String,
    val expiresAt: Instant?,
)

object ZdfTokenExtractor {
    private val escapedTokenPattern = Regex(
        "\\\\\"appToken\\\\\"\\s*:\\s*\\{.*?" +
            "\\\\\"apiToken\\\\\"\\s*:\\s*\\\\\"([^\\\\\"]+)\\\\\"" +
            "(?:.*?\\\\\"expiresAt\\\\\"\\s*:\\s*\\\\\"([^\\\\\"]+)\\\\\")?",
    )
    private val plainTokenPattern = Regex(
        "\"appToken\"\\s*:\\s*\\{.*?" +
            "\"apiToken\"\\s*:\\s*\"([^\"]+)\"" +
            "(?:.*?\"expiresAt\"\\s*:\\s*\"([^\"]+)\")?",
    )

    fun extract(html: String): ZdfApiToken? {
        val document = Jsoup.parse(html)
        return document.select("body > script")
            .asSequence()
            .map { it.html() }.firstNotNullOfOrNull { script ->
                escapedTokenPattern.find(script)?.toToken()
                    ?: plainTokenPattern.find(script)?.toToken()
            }
    }

    private fun MatchResult.toToken(): ZdfApiToken {
        val expiresAt = groupValues.getOrNull(2)
            ?.takeIf { it.isNotBlank() }
            ?.let { OffsetDateTime.parse(it).toInstant() }
        return ZdfApiToken(groupValues[1], expiresAt)
    }
}
