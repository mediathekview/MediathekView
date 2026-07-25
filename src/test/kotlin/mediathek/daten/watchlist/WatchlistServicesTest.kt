package mediathek.daten.watchlist

import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.runBlocking
import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.gui.messages.FilmListReadStopEvent
import mediathek.gui.messages.FilmsDownloadStartedEvent
import mediathek.tool.MessageBus
import mediathek.tool.notification.NotificationMessage
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.exists
import kotlin.io.path.readText

internal class WatchlistServicesTest {
    @TempDir
    lateinit var tempDir: Path

    private lateinit var storageFile: Path
    private lateinit var allFilms: ListeFilme
    private lateinit var publishedMessages: MutableList<NotificationMessage>
    private val createdServices = mutableListOf<WatchlistServices>()
    private lateinit var services: WatchlistServices

    @BeforeEach
    fun setUp() {
        storageFile = tempDir.resolve("watchlist.json")
        allFilms = ListeFilme()
        publishedMessages = mutableListOf()
        services = createServices()
    }

    @AfterEach
    fun tearDown() {
        createdServices.forEach(WatchlistServices::close)
        createdServices.clear()
    }

    private fun createServices(
        films: ListeFilme = allFilms,
        persistence: WatchlistPersistence = WatchlistStorage,
    ): WatchlistServices =
        WatchlistServices(
            films,
            publishedMessages::add,
            storageFile,
            persistence,
        ).also(createdServices::add)

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

    private fun tagesschau(url: String, isNew: Boolean, title: String = "Tagesschau 20:00 Uhr"): DatenFilm =
        film("ARD", "Tagesschau", title, url, isNew)

    @Test
    fun existingEpisodesArePrefilledAsSeenSoTheyNeverNotify() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = true)
        allFilms.add(existing)

        services.addEntryFromFilmAndWait(existing, withTitle = false)
        services.matchNewEpisodesAndWait()

        assertTrue(services.notificationsSnapshot().isEmpty())
        assertFalse(services.hasUnseenNotifications)
        assertTrue(publishedMessages.isEmpty())
        assertEquals(setOf(existing.sha256), services.entriesSnapshot().single().seenFilmIds)
    }

    @Test
    fun newEpisodeCreatesNotificationBadgeAndOsSummary() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)
        services.addEntryFromFilmAndWait(existing, withTitle = false)

        val fresh = tagesschau("https://example.org/new.mp4", isNew = true)
        allFilms.add(fresh)
        allFilms.add(film("ZDF", "heute", "heute 19:00 Uhr", "https://example.org/other.mp4", isNew = true))
        services.matchNewEpisodesAndWait()

        val notification = services.notificationsSnapshot().single()
        assertEquals(services.entriesSnapshot().single().id, notification.entryId)
        assertEquals(fresh.sha256, notification.filmId)
        assertEquals("https://example.org/new.mp4", notification.urlNormalQuality)
        assertTrue(services.hasUnseenNotifications)
        assertTrue(publishedMessages.single().message.contains("Tagesschau"))
    }

    @Test
    fun matchingIsIdempotentAcrossRuns() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)
        services.addEntryFromFilmAndWait(existing, withTitle = false)
        allFilms.add(tagesschau("https://example.org/new.mp4", isNew = true))

        services.matchNewEpisodesAndWait()
        services.matchNewEpisodesAndWait()

        assertEquals(1, services.notificationsSnapshot().size)
        assertEquals(1, publishedMessages.size)
    }

    @Test
    fun seenEpisodesStayDeduplicatedInAFreshProcess() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)
        services.addEntryFromFilmAndWait(existing, withTitle = false)
        allFilms.add(tagesschau("https://example.org/new.mp4", isNew = true))
        services.matchNewEpisodesAndWait()
        assertEquals(1, services.notificationsSnapshot().size)

        // Simulate a restart: new film objects with identical data, freshly loaded state.
        val restartedFilms = ListeFilme().apply {
            add(tagesschau("https://example.org/old.mp4", isNew = false))
            add(tagesschau("https://example.org/new.mp4", isNew = true))
        }
        val restarted = createServices(films = restartedFilms)
        restarted.loadFromFile()
        publishedMessages.clear()

        restarted.matchNewEpisodesAndWait()

        assertEquals(1, restarted.notificationsSnapshot().size)
        assertTrue(publishedMessages.isEmpty(), "restart must not re-notify already seen episodes")
    }

    @Test
    fun persistedSeenIdsAreContentDerivedAndNotProcessLocal() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)

        services.addEntryFromFilmAndWait(existing, withTitle = false)

        val persistedId = WatchlistStorage.read(storageFile).entries.single().seenFilmIds.single()
        // A compressed URL key such as "~0/old.mp4" depends on process-local dictionary state.
        assertTrue(persistedId.matches(Regex("[0-9a-f]{64}")), "expected content hash, got $persistedId")
        assertEquals(existing.sha256, persistedId)
        assertEquals(
            tagesschau("https://example.org/old.mp4", isNew = false).sha256,
            persistedId,
            "an equal film must yield the same persisted identity",
        )
    }

    @Test
    fun overlappingEntriesNotifyOnlyOncePerEpisode() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)
        services.addEntryFromFilmAndWait(existing, withTitle = false)
        services.addEntryFromFilmAndWait(existing, withTitle = true)
        assertEquals(2, services.entriesSnapshot().size)

        val fresh = tagesschau("https://example.org/new.mp4", isNew = true)
        allFilms.add(fresh)
        services.matchNewEpisodesAndWait()

        assertEquals(1, services.notificationsSnapshot().size)
        assertEquals(1, publishedMessages.size)
        // Both entries must remember the episode, otherwise the next run notifies again.
        assertTrue(services.entriesSnapshot().all { entry -> fresh.sha256 in entry.seenFilmIds })
        services.matchNewEpisodesAndWait()
        assertEquals(1, services.notificationsSnapshot().size)
    }

    @Test
    fun titleFilterMatchesCaseInsensitiveSubstring() = runBlocking {
        val seed = tagesschau("https://example.org/seed.mp4", isNew = false, title = "tagesschau")
        allFilms.add(seed)
        services.addEntryFromFilmAndWait(seed, withTitle = true)

        allFilms.add(tagesschau("https://example.org/new.mp4", isNew = true, title = "TAGESSCHAU 20:00 Uhr"))
        services.matchNewEpisodesAndWait()

        assertEquals(1, services.notificationsSnapshot().size)
    }

    @Test
    fun matchingWithoutEntriesDoesNothing() = runBlocking {
        allFilms.add(tagesschau("https://example.org/new.mp4", isNew = true))

        services.matchNewEpisodesAndWait()

        assertTrue(services.notificationsSnapshot().isEmpty())
        assertFalse(services.hasUnseenNotifications)
        assertTrue(publishedMessages.isEmpty())
    }

    @Test
    fun acknowledgeReturnsExactlyTheAcknowledgedSnapshotAndClearsBadge() = runBlocking {
        givenPendingNotification()

        val acknowledged = services.acknowledgeNotifications()

        assertEquals(services.notificationsSnapshot(), acknowledged)
        assertEquals(1, acknowledged.size)
        assertFalse(services.hasUnseenNotifications)
        assertEquals(1, services.notificationsSnapshot().size, "acknowledging must not drop rows")
    }

    @Test
    fun removingLastNotificationAlsoClearsTheBadge() = runBlocking {
        givenPendingNotification()

        services.removeNotificationAndWait(services.notificationsSnapshot().single())

        assertTrue(services.notificationsSnapshot().isEmpty())
        assertFalse(services.hasUnseenNotifications)
        assertEquals(1, services.entriesSnapshot().size)
    }

    @Test
    fun removingEntryCascadesItsNotificationsAndClearsStaleBadge() = runBlocking {
        givenPendingNotification()
        val entryId = services.entriesSnapshot().single().id

        services.removeEntryAndWait(entryId)

        assertTrue(services.entriesSnapshot().isEmpty())
        assertTrue(services.notificationsSnapshot().isEmpty())
        assertFalse(services.hasUnseenNotifications)
    }

    @Test
    fun removingEntryKeepsNotificationsOfOtherEntries() = runBlocking {
        val ardFilm = tagesschau("https://example.org/ard-old.mp4", isNew = false)
        val zdfFilm = film("ZDF", "heute", "heute 19:00 Uhr", "https://example.org/zdf-old.mp4", isNew = false)
        allFilms.add(ardFilm)
        allFilms.add(zdfFilm)
        services.addEntryFromFilmAndWait(ardFilm, withTitle = false)
        services.addEntryFromFilmAndWait(zdfFilm, withTitle = false)
        allFilms.add(tagesschau("https://example.org/ard-new.mp4", isNew = true))
        allFilms.add(film("ZDF", "heute", "heute 19:00 Uhr", "https://example.org/zdf-new.mp4", isNew = true))
        services.matchNewEpisodesAndWait()
        assertEquals(2, services.notificationsSnapshot().size)

        val ardEntry = services.entriesSnapshot().single { entry -> entry.sender == "ARD" }
        services.removeEntryAndWait(ardEntry.id)

        assertEquals("heute", services.notificationsSnapshot().single().entryName)
        assertTrue(services.hasUnseenNotifications)
    }

    @Test
    fun findEntryForDistinguishesTitleVariants() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        services.addEntryFromFilmAndWait(existing, withTitle = false)

        assertNotNull(services.findEntryFor(existing, withTitle = false))
        assertNull(services.findEntryFor(existing, withTitle = true))

        services.addEntryFromFilmAndWait(existing, withTitle = true)

        assertNotNull(services.findEntryFor(existing, withTitle = true))
    }

    @Test
    fun duplicateCriteriaAreIgnored() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)

        services.addEntryFromFilmAndWait(existing, withTitle = false)
        services.addEntryFromFilmAndWait(existing, withTitle = false)

        assertEquals(1, services.entriesSnapshot().size)
    }

    @Test
    fun filmListReadStopEventTriggersMatching() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)
        services.addEntryFromFilmAndWait(existing, withTitle = false)
        allFilms.add(tagesschau("https://example.org/new.mp4", isNew = true))

        MessageBus.messageBus.publish(FilmListReadStopEvent())
        services.awaitIdle()

        assertEquals(1, services.notificationsSnapshot().size)
        assertTrue(services.hasUnseenNotifications)
    }

    @Test
    fun concurrentOperationsPersistEveryEntryExactlyOnce() = runBlocking {
        val shows = (1..24).map { index ->
            film("ARD", "Show $index", "Folge $index", "https://example.org/$index.mp4", isNew = false)
        }
        shows.forEach(allFilms::add)

        shows.map { show -> async { services.addEntryFromFilm(show, withTitle = false).join() } }.awaitAll()
        services.awaitIdle()

        assertEquals(shows.size, services.entriesSnapshot().size)

        val reloaded = createServices()
        reloaded.loadFromFile()
        assertEquals(shows.size, reloaded.entriesSnapshot().size)
        assertEquals(
            services.entriesSnapshot().map(DatenWatchlistEntry::id).toSet(),
            reloaded.entriesSnapshot().map(DatenWatchlistEntry::id).toSet(),
        )
    }

    @Test
    fun closeFlushesStateThatCouldNotBeWrittenEarlier() = runBlocking {
        val failingPersistence = FailingWritePersistence()
        val flushingServices = createServices(persistence = failingPersistence)
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)

        failingPersistence.failWrites = true
        flushingServices.addEntryFromFilmAndWait(existing, withTitle = false)
        assertFalse(storageFile.exists(), "failed write must not produce a file")

        failingPersistence.failWrites = false
        flushingServices.close()

        val reloaded = createServices()
        reloaded.loadFromFile()
        assertEquals(1, reloaded.entriesSnapshot().size)
    }

    @Test
    fun closeStopsReactingToFilmListEvents() = runBlocking {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)
        services.addEntryFromFilmAndWait(existing, withTitle = false)
        services.close()
        allFilms.add(tagesschau("https://example.org/new.mp4", isNew = true))

        MessageBus.messageBus.publish(FilmListReadStopEvent())

        assertTrue(services.notificationsSnapshot().isEmpty())
    }

    @Test
    fun unreadableFileIsQuarantinedAndStateStartsEmpty() = runBlocking {
        Files.writeString(storageFile, "{ broken")
        val recovering = createServices()

        recovering.loadFromFile()

        assertTrue(recovering.entriesSnapshot().isEmpty())
        val quarantined = tempDir.resolve("watchlist.json.corrupt")
        assertTrue(quarantined.exists())
        assertEquals("{ broken", quarantined.readText())

        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        recovering.addEntryFromFilmAndWait(existing, withTitle = false)
        assertEquals(1, WatchlistStorage.read(storageFile).entries.size)
    }

    @Test
    fun unsupportedFileVersionIsPreservedAndWritesAreDisabled() = runBlocking {
        val futureContent = """
            {
              "version": 99,
              "entries": [ { "id": "future", "sender": "ZDF", "thema": "heute" } ]
            }
        """.trimIndent()
        Files.writeString(storageFile, futureContent)
        val guarded = createServices()

        guarded.loadFromFile()
        guarded.addEntryFromFilmAndWait(tagesschau("https://example.org/old.mp4", isNew = false), withTitle = false)

        assertEquals(futureContent, storageFile.readText(), "a newer file must never be overwritten")
    }

    @Test
    fun markFilmsSeenByDownloadRemovesNotification() = runBlocking {
        givenPendingNotification()
        val notification = services.notificationsSnapshot().single()
        val downloadedFilm = allFilms.snapshot().single { film -> film.sha256 == notification.filmId }

        services.markFilmsSeenByDownloadAndWait(listOf(downloadedFilm))

        assertTrue(services.notificationsSnapshot().isEmpty())
        assertFalse(services.hasUnseenNotifications)
    }

    @Test
    fun markFilmsSeenByDownloadDoesNotAffectUnrelatedEntries() = runBlocking {
        val ardFilm = tagesschau("https://example.org/ard-old.mp4", isNew = false)
        val zdfFilm = film("ZDF", "heute", "heute 19:00 Uhr", "https://example.org/zdf-old.mp4", isNew = false)
        allFilms.add(ardFilm)
        allFilms.add(zdfFilm)
        services.addEntryFromFilmAndWait(ardFilm, withTitle = false)
        services.addEntryFromFilmAndWait(zdfFilm, withTitle = false)
        allFilms.add(tagesschau("https://example.org/ard-new.mp4", isNew = true))
        allFilms.add(film("ZDF", "heute", "heute 19:00 Uhr", "https://example.org/zdf-new.mp4", isNew = true))
        services.matchNewEpisodesAndWait()
        assertEquals(2, services.notificationsSnapshot().size)

        val ardNotification = services.notificationsSnapshot().single { it.sender == "ARD" }
        val ardDownload = allFilms.snapshot().single { film -> film.sha256 == ardNotification.filmId }
        val zdfEntryBefore = services.entriesSnapshot().single { entry -> entry.sender == "ZDF" }
        assertFalse(ardNotification.filmId in zdfEntryBefore.seenFilmIds)

        services.markFilmsSeenByDownloadAndWait(listOf(ardDownload))

        assertEquals(1, services.notificationsSnapshot().size)
        assertEquals("ZDF", services.notificationsSnapshot().single().sender)
        val zdfEntryAfter = services.entriesSnapshot().single { entry -> entry.sender == "ZDF" }
        assertFalse(ardNotification.filmId in zdfEntryAfter.seenFilmIds)
    }

    @Test
    fun downloadedFilmIsAddedOnlyToMatchingEntriesBeforeNewEpisodeScan() = runBlocking {
        val ardFilm = tagesschau("https://example.org/ard-old.mp4", isNew = false)
        val zdfFilm = film("ZDF", "heute", "heute 19:00 Uhr", "https://example.org/zdf-old.mp4", isNew = false)
        allFilms.add(ardFilm)
        allFilms.add(zdfFilm)
        services.addEntryFromFilmAndWait(ardFilm, withTitle = false)
        services.addEntryFromFilmAndWait(zdfFilm, withTitle = false)
        val ardDownload = tagesschau("https://example.org/ard-new.mp4", isNew = true)
        val zdfNewFilm = film("ZDF", "heute", "heute 19:00 Uhr", "https://example.org/zdf-new.mp4", isNew = true)
        allFilms.add(ardDownload)
        allFilms.add(zdfNewFilm)

        services.markFilmsSeenByDownloadAndWait(listOf(ardDownload))

        val entries = services.entriesSnapshot()
        assertTrue(ardDownload.sha256 in entries.single { entry -> entry.sender == "ARD" }.seenFilmIds)
        assertFalse(ardDownload.sha256 in entries.single { entry -> entry.sender == "ZDF" }.seenFilmIds)

        services.matchNewEpisodesAndWait()
        assertEquals(listOf("ZDF"), services.notificationsSnapshot().map(WatchlistNotification::sender))
    }

    @Test
    fun markFilmsSeenByDownloadIsIdempotent() = runBlocking {
        givenPendingNotification()
        val notification = services.notificationsSnapshot().single()
        val downloadedFilm = allFilms.snapshot().single { film -> film.sha256 == notification.filmId }

        services.markFilmsSeenByDownloadAndWait(listOf(downloadedFilm))
        val sizeAfterFirst = services.entriesSnapshot().single().seenFilmIds.size
        services.markFilmsSeenByDownloadAndWait(listOf(downloadedFilm))

        assertTrue(services.notificationsSnapshot().isEmpty())
        assertEquals(sizeAfterFirst, services.entriesSnapshot().single().seenFilmIds.size)
    }

    @Test
    fun filmsDownloadStartedEventTriggersMarkSeenByDownload() = runBlocking {
        givenPendingNotification()
        val notification = services.notificationsSnapshot().single()
        val downloadedFilm = allFilms.snapshot().single { film -> film.sha256 == notification.filmId }

        MessageBus.messageBus.publish(FilmsDownloadStartedEvent(listOf(downloadedFilm)))
        services.awaitIdle()

        assertTrue(services.notificationsSnapshot().isEmpty())
        assertFalse(services.hasUnseenNotifications)
    }

    @Test
    fun markFilmsSeenByDownloadIsPersisted() = runBlocking {
        givenPendingNotification()
        val notification = services.notificationsSnapshot().single()
        val downloadedFilm = allFilms.snapshot().single { film -> film.sha256 == notification.filmId }

        services.markFilmsSeenByDownloadAndWait(listOf(downloadedFilm))

        val reloaded = createServices()
        reloaded.loadFromFile()
        assertTrue(reloaded.notificationsSnapshot().isEmpty())
        assertTrue(notification.filmId in reloaded.entriesSnapshot().single().seenFilmIds)
    }

    @Test
    fun markFilmsSeenByDownloadWithEmptyFilmsDoesNothing() = runBlocking {
        givenPendingNotification()

        services.markFilmsSeenByDownloadAndWait(emptyList())

        assertEquals(1, services.notificationsSnapshot().size)
        assertTrue(services.hasUnseenNotifications)
    }

    @Test
    fun markFilmsSeenByDownloadClearsBadgeWhenAllNotificationsRemoved() = runBlocking {
        givenPendingNotification()
        val notification = services.notificationsSnapshot().single()
        val downloadedFilm = allFilms.snapshot().single { film -> film.sha256 == notification.filmId }
        assertTrue(services.hasUnseenNotifications)

        services.markFilmsSeenByDownloadAndWait(listOf(downloadedFilm))

        assertFalse(services.hasUnseenNotifications)
    }

    private suspend fun givenPendingNotification() {
        val existing = tagesschau("https://example.org/old.mp4", isNew = false)
        allFilms.add(existing)
        services.addEntryFromFilmAndWait(existing, withTitle = false)
        allFilms.add(tagesschau("https://example.org/new.mp4", isNew = true))
        services.matchNewEpisodesAndWait()
        assertTrue(services.hasUnseenNotifications)
    }

    private class FailingWritePersistence : WatchlistPersistence {
        var failWrites = false

        override fun read(storagePath: Path): WatchlistSnapshot = WatchlistStorage.read(storagePath)

        override fun write(storagePath: Path, snapshot: WatchlistSnapshot) {
            if (failWrites) {
                throw java.io.IOException("write failure for test")
            }
            WatchlistStorage.write(storagePath, snapshot)
        }

        override fun quarantine(storagePath: Path): Path = WatchlistStorage.quarantine(storagePath)
    }
}
