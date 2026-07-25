package mediathek.daten.blacklist

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.filmlisten.FilmCatalog
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class BlacklistServicesTest {
    private val config = ApplicationConfiguration.getInstance()
    private val previousBlacklistEnabled = config.isBlacklistEnabled
    private val previousHideFutureFilms = config.blacklistDoNotShowFutureFilms
    private val previousHideGeoBlockedFilms = config.blacklistDoNotShowGeoblockedFilms
    private val previousMinimumFilmLengthMinutes = config.blacklistMinimumFilmLengthMinutes
    private val previousEvaluateDuplicates = config.evaluateFilmDuplicates

    @AfterEach
    fun tearDown() {
        config.isBlacklistEnabled = previousBlacklistEnabled
        config.blacklistDoNotShowFutureFilms = previousHideFutureFilms
        config.blacklistDoNotShowGeoblockedFilms = previousHideGeoBlockedFilms
        config.blacklistMinimumFilmLengthMinutes = previousMinimumFilmLengthMinutes
        config.evaluateFilmDuplicates = previousEvaluateDuplicates
    }

    @Test
    fun `applyToFilmList bulk-adds prepared filtered films`() {
        config.isBlacklistEnabled = false
        config.blacklistDoNotShowFutureFilms = false
        config.blacklistDoNotShowGeoblockedFilms = false
        config.blacklistMinimumFilmLengthMinutes = 0
        config.evaluateFilmDuplicates = false
        val filteredFilms = CountingFilmList()
        val filmCatalog = FilmCatalog().apply {
            allFilms.addAll(listOf(film("First"), film("Second")))
            this.filteredFilms = filteredFilms
        }

        BlacklistServices(filmCatalog).applyToFilmList()

        assertEquals(1, filteredFilms.addAllCalls)
        assertEquals(0, filteredFilms.addCalls)
        assertEquals(filmCatalog.allFilms.toList(), filteredFilms.toList())
    }

    private class CountingFilmList : ListeFilme() {
        var addCalls = 0
            private set
        var addAllCalls = 0
            private set

        override fun add(element: DatenFilm): Boolean {
            addCalls++
            return super.add(element)
        }

        override fun addAll(elements: Collection<DatenFilm>): Boolean {
            addAllCalls++
            return super.addAll(elements)
        }
    }

    private fun film(title: String): DatenFilm =
        DatenFilm().apply {
            sender = "ARD"
            thema = "Test"
            this.title = title
            urlNormalQuality = "https://example.invalid/$title.mp4"
        }
}