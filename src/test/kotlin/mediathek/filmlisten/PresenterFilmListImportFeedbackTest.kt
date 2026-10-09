package mediathek.filmlisten

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class PresenterFilmListImportFeedbackTest {
    @Test
    fun `showNoUpdateAvailable forwards to current presenter`() {
        val firstPresenter = BlockingFilmListLoadPresenter()
        val secondPresenter = BlockingFilmListLoadPresenter()
        var currentPresenter: FilmListLoadPresenter = firstPresenter
        val feedback = PresenterFilmListImportFeedback { currentPresenter }

        feedback.showNoUpdateAvailable(showDialogs = true)
        currentPresenter = secondPresenter
        feedback.showNoUpdateAvailable(showDialogs = false)

        assertEquals(1, firstPresenter.noUpdateAvailableCount.get())
        assertTrue(firstPresenter.lastNoUpdateShowDialogs == true)
        assertEquals(1, secondPresenter.noUpdateAvailableCount.get())
        assertTrue(secondPresenter.lastNoUpdateShowDialogs == false)
    }
}
