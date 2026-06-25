package mediathek.filmlisten

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class FilmListImportApplierTest {
    @Test
    fun `collectFilmUrls returns normal quality urls`() {
        val first = film("ARD", "First")
        val second = film("ZDF", "Second")
        val films = filmList(first, second)

        val urls = FilmListImportApplier.collectFilmUrls(films)

        assertEquals(setOf(first.urlNormalQuality, second.urlNormalQuality), urls)
    }

    @Test
    fun `applyImportedFilms keeps existing url films not new and marks unknown urls new`() {
        val known = film("ARD", "Known").apply { isNew = true }
        val unknown = film("ARD", "Unknown")
        val films = filmList(known, unknown)
        val oldFilmUrls = setOf(known.urlNormalQuality)

        FilmListImportApplier.applyImportedFilms(films, ListeFilme(), oldFilmUrls)

        assertFalse(known.isNew)
        assertTrue(unknown.isNew)
    }

    @Test
    fun `applyImportedFilms merges diff list and transfers metadata`() {
        val oldFilm = film("ARD", "Old")
        val replacement = film("ARD", "Replacement")
        val films = filmList(oldFilm)
        val diffList = filmList(replacement).apply {
            metaData.datum = "16.05.2026, 12:00"
            metaData.id = "diff-list"
        }

        FilmListImportApplier.applyImportedFilms(films, diffList, oldFilmUrls = emptySet())

        assertEquals(2, films.size)
        assertTrue(films.contains(oldFilm))
        assertTrue(films.contains(replacement))
        assertEquals("16.05.2026, 12:00", films.metaData.datum)
        assertEquals("diff-list", films.metaData.id)
        assertTrue(diffList.isEmpty())
        assertTrue(oldFilm.isNew)
        assertTrue(replacement.isNew)
    }

    private fun filmList(vararg films: DatenFilm): ListeFilme =
        ListeFilme().apply {
            metaData.datum = "15.05.2026, 12:00"
            metaData.id = "test-list"
            addAll(films)
        }

    private fun film(sender: String, title: String): DatenFilm =
        DatenFilm().apply {
            this.sender = sender
            thema = "Thema"
            this.title = title
            urlNormalQuality = "https://example.test/$sender/$title.mp4"
            websiteUrl = "https://example.test/$sender/$title"
        }
}
