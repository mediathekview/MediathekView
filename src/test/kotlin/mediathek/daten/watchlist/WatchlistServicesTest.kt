package mediathek.daten.watchlist

import kotlinx.coroutines.runBlocking
import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.tool.notification.NotificationMessage
import mediathek.tool.notification.NotificationPublisher
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path

internal class WatchlistServicesTest {
    @TempDir
    lateinit var tempDir: Path

    private lateinit var storageFile: Path
    private lateinit var allFilms: ListeFilme
    private lateinit var publishedMessages: MutableList<NotificationMessage>
    private lateinit var services: WatchlistServices

    @BeforeEach
    fun setUp() {
        storageFile = tempDir.resolve("watchlist.json")
        allFilms = ListeFilme()
        publishedMessages = mutableListOf()
        services = createServices()
    }

    private fun createServices(): WatchlistServices =
        WatchlistServices(allFilms, NotificationPublisher { publishedMessages.add(it) }, storageFile)

    private fun film(
        sender: String,
        thema: String,
        title: String,
        url: String,
        isNew: Boolean,
    ): DatenFilm =
        DatenFilm().apply {
            this.sender = sender
            this.thema = thema
            this.title = title
            this.urlNormalQuality = url
            this.isNew = isNew
        }

    @Test
    fun addEntryPrefillsSeenUrlsSoExistingEpisodesDoNotNotify() = runBlocking {
        allFilms.add(film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/1.mp4", isNew = true))

        services.addEntryFromFilmInternal(allFilms.single(), withTitle = false)
        services.matchNewEpisodes()

        assertTrue(services.notificationsSnapshot().isEmpty())
        assertFalse(services.hasUnseenNotifications)
        assertTrue(publishedMessages.isEmpty())

        val entry = services.entriesSnapshot().single()
        assertEquals("Tagesschau", entry.name)
        assertEquals("ARD", entry.sender)
        assertEquals("Tagesschau", entry.thema)
        assertEquals("", entry.title)
        assertEquals(1, entry.seenUrlKeys.size)
    }

    @Test
    fun addEntryWithTitleStoresTitleFilter() = runBlocking {
        val film = film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/1.mp4", isNew = false)

        services.addEntryFromFilmInternal(film, withTitle = true)

        assertEquals("Tagesschau 20:00 Uhr", services.entriesSnapshot().single().title)
    }

    @Test
    fun addEntryIgnoresDuplicateCriteria() = runBlocking {
        val film = film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/1.mp4", isNew = false)

        services.addEntryFromFilmInternal(film, withTitle = false)
        services.addEntryFromFilmInternal(film, withTitle = false)

        assertEquals(1, services.entriesSnapshot().size)
    }

    @Test
    fun matchCreatesNotificationOnlyForNewMatchingEpisodes() = runBlocking {
        val existing = film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)
        services.addEntryFromFilmInternal(existing, withTitle = false)

        allFilms.add(film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/new.mp4", isNew = true))
        allFilms.add(film("ZDF", "heute", "heute 19:00 Uhr", "https://example.org/other.mp4", isNew = true))
        services.matchNewEpisodes()

        val notification = services.notificationsSnapshot().single()
        val entry = services.entriesSnapshot().single()
        assertEquals(entry.id, notification.entryId)
        assertEquals("Tagesschau", notification.entryName)
        assertEquals("ARD", notification.sender)
        assertEquals("Tagesschau", notification.thema)
        assertEquals("https://example.org/new.mp4", notification.urlNormalQuality)
        assertTrue(services.hasUnseenNotifications)

        val message = publishedMessages.single()
        assertTrue(message.message.contains("Tagesschau"))
    }

    @Test
    fun matchDeduplicatesAcrossRuns() = runBlocking {
        val film = film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/new.mp4", isNew = true)

        services.addEntryFromFilmInternal(film, withTitle = false)
        film.isNew = true
        allFilms.add(film)

        // simulate a fresh episode that was not part of the prefill
        val created = services.entriesSnapshot().single()
        created.seenUrlKeys.clear()

        services.matchNewEpisodes()
        services.matchNewEpisodes()

        assertEquals(1, services.notificationsSnapshot().size)
        assertEquals(1, publishedMessages.size)
    }

    @Test
    fun matchUsesTitleContainsIgnoreCase() = runBlocking {
        val seedFilm = film("ARD", "Tagesschau", "tagesschau", "https://example.org/seed.mp4", isNew = false)
        services.addEntryFromFilmInternal(seedFilm, withTitle = true)

        allFilms.add(film("ARD", "Tagesschau", "TAGESSCHAU 20:00 Uhr", "https://example.org/new.mp4", isNew = true))
        services.matchNewEpisodes()

        assertEquals(1, services.notificationsSnapshot().size)
    }

    @Test
    fun matchWithoutEntriesDoesNothing() = runBlocking {
        allFilms.add(film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/new.mp4", isNew = true))

        services.matchNewEpisodes()

        assertTrue(services.notificationsSnapshot().isEmpty())
        assertFalse(services.hasUnseenNotifications)
        assertTrue(publishedMessages.isEmpty())
    }

    @Test
    fun markAllSeenClearsBadgeButKeepsNotifications() = runBlocking {
        val film = film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/new.mp4", isNew = true)
        services.addEntryFromFilmInternal(film, withTitle = false)
        services.entriesSnapshot().single().seenUrlKeys.clear()
        allFilms.add(film)
        services.matchNewEpisodes()
        assertTrue(services.hasUnseenNotifications)

        services.markAllSeen()

        assertFalse(services.hasUnseenNotifications)
        assertEquals(1, services.notificationsSnapshot().size)
    }

    @Test
    fun removeEntryCascadesOnlyItsOwnNotifications() = runBlocking {
        val ardFilm = film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/ard.mp4", isNew = true)
        val zdfFilm = film("ZDF", "heute", "heute 19:00 Uhr", "https://example.org/zdf.mp4", isNew = true)
        services.addEntryFromFilmInternal(ardFilm, withTitle = false)
        services.addEntryFromFilmInternal(zdfFilm, withTitle = false)
        services.entriesSnapshot().forEach { it.seenUrlKeys.clear() }
        allFilms.add(ardFilm)
        allFilms.add(zdfFilm)
        services.matchNewEpisodes()
        assertEquals(2, services.notificationsSnapshot().size)

        val ardEntry = services.entriesSnapshot().single { it.sender == "ARD" }
        services.removeEntry(ardEntry)

        assertEquals(1, services.entriesSnapshot().size)
        val remaining = services.notificationsSnapshot().single()
        assertEquals("heute", remaining.entryName)
    }

    @Test
    fun removeNotificationRemovesOnlyThatNotification() = runBlocking {
        val film = film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/new.mp4", isNew = true)
        services.addEntryFromFilmInternal(film, withTitle = false)
        services.entriesSnapshot().single().seenUrlKeys.clear()
        allFilms.add(film)
        services.matchNewEpisodes()

        val notification = services.notificationsSnapshot().single()
        services.removeNotification(notification)

        assertTrue(services.notificationsSnapshot().isEmpty())
        assertEquals(1, services.entriesSnapshot().size)
    }

    @Test
    fun findEntryForDistinguishesTitleVariants() = runBlocking {
        val film = film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/1.mp4", isNew = false)
        services.addEntryFromFilmInternal(film, withTitle = false)

        assertNotNull(services.findEntryFor(film, withTitle = false))
        assertNull(services.findEntryFor(film, withTitle = true))

        services.addEntryFromFilmInternal(film, withTitle = true)

        assertNotNull(services.findEntryFor(film, withTitle = true))
    }

    @Test
    fun stateSurvivesReloadAcrossInstances() = runBlocking {
        val film = film("ARD", "Tagesschau", "Tagesschau 20:00 Uhr", "https://example.org/new.mp4", isNew = true)
        services.addEntryFromFilmInternal(film, withTitle = false)
        services.entriesSnapshot().single().seenUrlKeys.clear()
        allFilms.add(film)
        services.matchNewEpisodes()

        val reloaded = createServices()
        reloaded.loadFromFile()

        assertEquals(1, reloaded.entriesSnapshot().size)
        assertEquals(1, reloaded.notificationsSnapshot().size)
        assertTrue(reloaded.hasUnseenNotifications)
        assertEquals(
            services.entriesSnapshot().single().id,
            reloaded.entriesSnapshot().single().id,
        )
    }
}
