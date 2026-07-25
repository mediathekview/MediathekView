package mediathek.mainwindow

import mediathek.daten.DatenFilm
import mediathek.filmlisten.FilmCatalog
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import javax.swing.SwingUtilities

internal class FilmSizeInfoLabelTest {
    @Test
    fun `committed filtered count updates label independently of supplier timing`() {
        val catalog = FilmCatalog().apply {
            repeat(10) { allFilms.add(DatenFilm()) }
        }
        val rowCount = FilmTableRowCountProperty(10)
        val label = FilmSizeInfoLabel(catalog, rowCount)

        label.updateDisplayedFilmCount(3)

        assertEquals("3 Filme (Insgesamt: 10)", label.text)
    }

    @Test
    fun `committed row count property updates the subscribed label`() {
        val catalog = FilmCatalog().apply {
            repeat(10) { allFilms.add(DatenFilm()) }
        }
        val rowCount = FilmTableRowCountProperty(10)
        val label = FilmSizeInfoLabel(catalog, rowCount)

        try {
            onEdt { label.addNotify() }

            rowCount.publish(4)
            onEdt { }

            assertEquals("4 Filme (Insgesamt: 10)", label.text)
        } finally {
            onEdt { label.removeNotify() }
        }
    }

    private fun onEdt(action: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) {
            action()
        } else {
            SwingUtilities.invokeAndWait(action)
        }
    }
}
