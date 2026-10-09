package mediathek.gui.duplicates

import mediathek.daten.DatenFilm
import mediathek.filmlisten.FilmCatalog
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class FilmDuplicateEvaluationTaskTest {

    @Test
    fun marksPenalizedSenderAsDuplicateWhenEquivalentNonPenalizedFilmAppearsLater() {
        val ardFilm = duplicateFilm(sender = "ARD")
        val kikaFilm = duplicateFilm(sender = "KiKA")
        val filmCatalog = filmCatalogWith(ardFilm, kikaFilm)

        FilmDuplicateEvaluationTask(filmCatalog).run()

        assertTrue(ardFilm.isDuplicate)
        assertFalse(kikaFilm.isDuplicate)
        assertEquals(listOf(FilmStatistics("ARD", 1)), filmCatalog.duplicateStatistics.toList())
    }

    @Test
    fun keepsComparatorWinnerWhenSeveralEquivalentFilmsCollide() {
        val zdfFilm = duplicateFilm(sender = "ZDF", thema = "Z Thema")
        val arteFilm = duplicateFilm(sender = "arte", thema = "B Thema")
        val alphaFilm = duplicateFilm(sender = "3sat", thema = "A Thema")
        val filmCatalog = filmCatalogWith(zdfFilm, arteFilm, alphaFilm)

        FilmDuplicateEvaluationTask(filmCatalog).run()

        assertFalse(alphaFilm.isDuplicate)
        assertTrue(arteFilm.isDuplicate)
        assertTrue(zdfFilm.isDuplicate)
        assertEquals(
            listOf(
                FilmStatistics("ZDF", 1),
                FilmStatistics("arte", 1),
            ),
            filmCatalog.duplicateStatistics.toList(),
        )
    }

    @Test
    fun treatsDifferentQualityUrlsAsDifferentDuplicateGroups() {
        val firstFilm = duplicateFilm(sender = "KiKA", highQualityUrl = "https://example.invalid/video-hd-a.mp4")
        val secondFilm = duplicateFilm(sender = "KiKA", highQualityUrl = "https://example.invalid/video-hd-b.mp4")
        val filmCatalog = filmCatalogWith(firstFilm, secondFilm)

        FilmDuplicateEvaluationTask(filmCatalog).run()

        assertFalse(firstFilm.isDuplicate)
        assertFalse(secondFilm.isDuplicate)
        assertEquals(emptyList<FilmStatistics>(), filmCatalog.duplicateStatistics.toList())
    }

    private fun filmCatalogWith(vararg films: DatenFilm): FilmCatalog =
        FilmCatalog().apply {
            allFilms.addAll(films)
        }

    private fun duplicateFilm(
        sender: String,
        thema: String = "Thema",
        normalQualityUrl: String = "https://example.invalid/video.mp4",
        highQualityUrl: String = "https://example.invalid/video-hd.mp4",
        lowQualityUrl: String = "https://example.invalid/video-low.mp4",
    ): DatenFilm =
        DatenFilm().apply {
            this.sender = sender
            this.thema = thema
            title = "Titel"
            urlNormalQuality = normalQualityUrl
            this.highQualityUrl = highQualityUrl
            this.lowQualityUrl = lowQualityUrl
        }
}
