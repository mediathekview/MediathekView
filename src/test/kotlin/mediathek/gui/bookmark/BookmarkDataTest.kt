package mediathek.gui.bookmark

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.tool.MessageBus
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.beans.PropertyChangeListener
import java.nio.file.Path
import kotlin.io.path.writeText

internal class BookmarkDataTest {
    @Test
    fun propertyChangeListenerCanBeDetached() {
        val bookmark = BookmarkData()
        var notifications = 0
        val listener = PropertyChangeListener { notifications++ }

        bookmark.addPropertyChangeListener(listener)
        bookmark.note = "attached"
        bookmark.removePropertyChangeListener(listener)
        bookmark.note = "detached"

        assertEquals(1, notifications)
    }

    @Test
    fun refreshMatchesPersistedHashesAndMigratesLegacyUrlsInCatalogOrder(@TempDir directory: Path) {
        val firstHashMatch = DatenFilm().apply {
            sender = "ARTE"
            thema = "München"
            urlNormalQuality = "https://example.org/episode.mp4"
            websiteUrl = "https://example.org/Übersicht"
        }
        val secondHashMatch = DatenFilm(firstHashMatch)
        val firstUrlMatch = DatenFilm().apply { urlNormalQuality = "https://example.org/MixedCase.mp4" }
        val secondUrlMatch = DatenFilm().apply { urlNormalQuality = "https://example.org/mixedcase.mp4" }
        val catalog = ListeFilme().apply {
            addAll(listOf(firstHashMatch, secondHashMatch, firstUrlMatch, secondUrlMatch))
        }
        val hash = firstHashMatch.sha256
        val path = directory.resolve("bookmarks.json")
        path.writeText(
            """
            {"bookmarks":[
              {"filmHashCode":"$hash"},
              {"filmHashCode":"${hash.uppercase()}","url":"${firstHashMatch.urlNormalQuality}"},
              {"filmHashCode":"${"0".repeat(64)}"},
              {"filmHashCode":"invalid"},
              {"url":"HTTPS://EXAMPLE.ORG/MIXEDCASE.MP4"},
              {"url":"https://example.org/expired.mp4"}
            ]}
            """.trimIndent(),
        )
        val stored = BookmarkJsonStore.read(path)
        val bookmarks = BookmarkDataList(catalog)
        try {
            bookmarks.getEventList().addAll(stored)
            bookmarks.refreshFromCurrentFilmList()

            assertSame(firstHashMatch, stored[0].datenFilm)
            assertSame(stored[0], firstHashMatch.bookmark)
            assertNull(secondHashMatch.bookmark)
            assertTrue(stored.subList(1, 4).all { it.datenFilm == null })
            assertSame(firstUrlMatch, stored[4].datenFilm)
            assertSame(stored[4], firstUrlMatch.bookmark)
            assertNull(secondUrlMatch.bookmark)
            assertEquals(firstUrlMatch.sha256, stored[4].filmHashCode)
            assertNull(stored[4].url)
            assertNull(stored[5].datenFilm)
            assertEquals("https://example.org/expired.mp4", stored[5].url)

            BookmarkJsonStore.write(path, stored)
            assertEquals(stored.map { it.filmHashCode }, BookmarkJsonStore.read(path).map { it.filmHashCode })
        } finally {
            MessageBus.messageBus.unsubscribe(bookmarks)
        }
    }
}
