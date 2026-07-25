package mediathek.controller.history

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class SeenHistoryCacheTest {
    @AfterEach
    fun tearDown() {
        SeenHistoryCache.clear()
    }

    @Test
    fun loadMakesSeenUrlsAvailableForExactMembershipChecks() {
        val url = "https://example.org/seen.mp4"

        SeenHistoryCache.load(SeenHistorySource.FILM, setOf(url))

        assertTrue(SeenHistoryCache.isPrepared(SeenHistorySource.FILM))
        assertTrue(SeenHistoryCache.contains(SeenHistorySource.FILM, url))
        assertFalse(SeenHistoryCache.contains(SeenHistorySource.FILM, "https://example.org/unseen.mp4"))
    }

    @Test
    fun removingUrlClearsExactMembership() {
        val url = "https://example.org/removed.mp4"
        SeenHistoryCache.load(SeenHistorySource.FILM, setOf(url))

        SeenHistoryCache.remove(SeenHistorySource.FILM, url)

        assertFalse(SeenHistoryCache.contains(SeenHistorySource.FILM, url))
    }

    @Test
    fun markSeenUpdatesPreparedExactMembership() {
        val url = "https://example.org/newly-seen.mp4"
        SeenHistoryCache.load(SeenHistorySource.FILM, emptySet())

        SeenHistoryCache.add(SeenHistorySource.FILM, url)

        assertTrue(SeenHistoryCache.contains(SeenHistorySource.FILM, url))
    }
}
