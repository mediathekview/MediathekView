package mediathek.filmlisten

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class FilmListImportApplierTest {
    @Test
    fun `collectFilmUrlKeys returns stored normal quality URLs without materializing caches`() {
        val first = film("ARD", "First")
        val second = film("ZDF", "Second")
        val films = filmList(first, second)
        clearLazyCaches(first, second)

        val urls = FilmListImportApplier.collectFilmUrlKeys(films)

        assertEquals(setOf(first.storedNormalQualityUrl, second.storedNormalQualityUrl), urls)
        assertLazyCachesEmpty(first, second)
    }

    @Test
    fun `applyImportedFilms keeps existing url films not new and marks unknown urls new`() {
        val known = film("ARD", "Known").apply { isNew = true }
        val unknown = film("ARD", "Unknown")
        val films = filmList(known, unknown)
        val oldFilmUrlKeys = setOf(known.storedNormalQualityUrl)

        FilmListImportApplier.applyImportedFilms(films, ListeFilme(), oldFilmUrlKeys)

        assertFalse(known.isNew)
        assertTrue(unknown.isNew)
    }

    @Test
    fun `applyImportedFilms clears stale new flags when film urls existed before`() {
        val first = film("ARD", "First").apply { isNew = true }
        val second = film("ZDF", "Second").apply { isNew = true }
        val films = filmList(first, second)
        val oldFilmUrlKeys = setOf(first.storedNormalQualityUrl, second.storedNormalQualityUrl)

        FilmListImportApplier.applyImportedFilms(films, ListeFilme(), oldFilmUrlKeys)

        assertFalse(first.isNew)
        assertFalse(second.isNew)
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

        FilmListImportApplier.applyImportedFilms(films, diffList, oldFilmUrlKeys = emptySet())

        assertEquals(2, films.size)
        assertTrue(films.contains(oldFilm))
        assertTrue(films.contains(replacement))
        assertEquals("16.05.2026, 12:00", films.metaData.datum)
        assertEquals("diff-list", films.metaData.id)
        assertTrue(diffList.isEmpty())
        assertTrue(oldFilm.isNew)
        assertTrue(replacement.isNew)
    }

    @Test
    fun `diff merge caches compact identities without materializing expanded URLs`() {
        val replaced = film("ARD", "Replaced")
        val untouched = film("ZDF", "Untouched")
        val replacement = film("ARD", "Replaced")
        val films = filmList(replaced, untouched)
        val diffList = filmList(replacement)
        clearLazyCaches(replaced, untouched, replacement)
        val oldFilmUrlKeys = FilmListImportApplier.collectFilmUrlKeys(films)

        FilmListImportApplier.applyImportedFilms(films, diffList, oldFilmUrlKeys)

        assertEquals(listOf(replacement, untouched), films)
        assertFalse(replacement.isNew)
        assertFalse(untouched.isNew)
        assertUrlCachesEmpty(replaced, untouched, replacement)
        assertIdentityUsesStoredUrls(replaced, untouched, replacement)
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

    private fun clearLazyCaches(vararg films: DatenFilm) {
        films.forEach { film ->
            setPrivateField(film, "normalQualityUrlCache", null)
            setPrivateField(film, "filmIdentityCache", null)
        }
    }

    private fun assertLazyCachesEmpty(vararg films: DatenFilm) {
        films.forEach { film ->
            assertNull(privateField(film, "normalQualityUrlCache"))
            assertNull(privateField(film, "filmIdentityCache"))
        }
    }

    private fun assertUrlCachesEmpty(vararg films: DatenFilm) {
        films.forEach { film -> assertNull(privateField(film, "normalQualityUrlCache")) }
    }

    private fun assertIdentityUsesStoredUrls(vararg films: DatenFilm) {
        films.forEach { film ->
            val identity = privateField(film, "filmIdentityCache") as DatenFilm.FilmIdentity
            assertEquals(film.storedNormalQualityUrl, identity.storedNormalQualityUrl)
            assertEquals(film.storedWebsiteUrl, identity.storedWebsiteUrl)
            assertTrue(identity.storedNormalQualityUrl.startsWith("~"))
            assertTrue(identity.storedWebsiteUrl?.startsWith("~") == true)
        }
    }

    private fun privateField(film: DatenFilm, name: String): Any? =
        DatenFilm::class.java.getDeclaredField(name).let { field ->
            field.isAccessible = true
            field.get(film)
        }

    private fun setPrivateField(film: DatenFilm, name: String, value: Any?) {
        DatenFilm::class.java.getDeclaredField(name).let { field ->
            field.isAccessible = true
            field.set(film, value)
        }
    }
}
