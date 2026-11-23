package mediathek.gui.tabs.tab_film.selection

import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.daten.FilmResolution
import mediathek.gui.tabs.tab_film.table.FilmTableModelBinding
import org.junit.jupiter.api.Test
import org.mockito.Mockito.*

class FilmSelectionControllerTest {
    @Test
    fun `saving from an HD-only list uses the selected download program set`() {
        val film = DatenFilm().apply {
            highQualityUrl = "https://example.org/hd.mp4"
            lowQualityUrl = "https://example.org/low.mp4"
        }
        val programSet = DatenPset("Low quality").apply {
            setSpeichern(true)
            aufloesung = FilmResolution.Enum.LOW
        }
        val table = mock(FilmTableModelBinding::class.java)
        val host = mock(FilmSelectionController.Host::class.java)
        `when`(table.selectedFilms()).thenReturn(listOf(film))
        `when`(host.tableBinding()).thenReturn(table)
        `when`(host.showHighQualityOnly()).thenReturn(true)

        FilmSelectionController(host).saveFilm(programSet)

        verify(host).saveFilms(listOf(film), programSet)
        verify(host, never()).showHighQualityOnly()
    }
}
