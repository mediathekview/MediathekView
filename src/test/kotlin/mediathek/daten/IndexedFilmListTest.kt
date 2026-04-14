package mediathek.daten

import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test

internal class IndexedFilmListTest {

    @Test
    fun `getFilmByFilmNr returnsMatchingFilmAfterIndexBuild`() {
        val list = IndexedFilmList()
        val firstFilm = createFilm("Alpha")
        val secondFilm = createFilm("Beta")

        list.addAll(listOf(firstFilm, secondFilm))

        assertSame(firstFilm, list.getFilmByFilmNr(firstFilm.filmNr))
        assertSame(secondFilm, list.getFilmByFilmNr(secondFilm.filmNr))
    }

    @Test
    fun `getFilmByFilmNr rebuildsIndexAfterAdd`() {
        val list = IndexedFilmList()
        val firstFilm = createFilm("Alpha")
        list.add(firstFilm)

        assertSame(firstFilm, list.getFilmByFilmNr(firstFilm.filmNr))

        val secondFilm = createFilm("Beta")
        list.add(secondFilm)

        assertSame(secondFilm, list.getFilmByFilmNr(secondFilm.filmNr))
    }

    @Test
    fun `getFilmByFilmNr rebuildsIndexAfterRemoveAndClear`() {
        val list = IndexedFilmList()
        val firstFilm = createFilm("Alpha")
        val secondFilm = createFilm("Beta")
        list.addAll(listOf(firstFilm, secondFilm))

        assertSame(firstFilm, list.getFilmByFilmNr(firstFilm.filmNr))

        list.remove(firstFilm)

        assertNull(list.getFilmByFilmNr(firstFilm.filmNr))
        assertSame(secondFilm, list.getFilmByFilmNr(secondFilm.filmNr))

        list.clear()

        assertNull(list.getFilmByFilmNr(secondFilm.filmNr))
    }

    @Test
    fun `getFilmByFilmNr rebuildsIndexAfterSet`() {
        val list = IndexedFilmList()
        val originalFilm = createFilm("Original")
        list.add(originalFilm)

        assertSame(originalFilm, list.getFilmByFilmNr(originalFilm.filmNr))

        val replacementFilm = createFilm("Replacement")
        list[0] = replacementFilm

        assertNull(list.getFilmByFilmNr(originalFilm.filmNr))
        assertSame(replacementFilm, list.getFilmByFilmNr(replacementFilm.filmNr))
    }

    private fun createFilm(title: String): DatenFilm =
        DatenFilm().apply {
            sender = "ARD"
            thema = "Test"
            this.title = title
            setNormalQualityUrl("https://example.invalid/$title")
        }
}
