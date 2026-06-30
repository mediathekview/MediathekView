package mediathek.tool.models

import mediathek.daten.DatenFilm
import mediathek.tool.datum.DatumFilm
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import javax.swing.event.TableModelEvent

internal class TModelFilmTest {
    @Test
    fun columnDescriptorsPreserveFilmTableMetadataAndValues() {
        val film = DatenFilm().apply {
            sender = "ARD"
            thema = "Thema"
            title = "Titel"
            urlNormalQuality = "https://example.invalid/video.mp4"
            setFilmLengthSeconds(42)
            setFileSize("17")
        }
        val model = TModelFilm()
        model.addAll(listOf(film))

        assertEquals(15, model.columnCount)
        assertEquals("Sender", model.getColumnName(FilmColumn.SENDER.index))
        assertEquals("Größe [MB]", model.getColumnName(FilmColumn.SIZE.index))
        assertEquals(Int::class.javaObjectType, model.getColumnClass(FilmColumn.DURATION.index))
        assertEquals(DatumFilm::class.java, model.getColumnClass(FilmColumn.DATE.index))
        assertEquals(Boolean::class.javaObjectType, model.getColumnClass(FilmColumn.HIGH_QUALITY.index))

        assertEquals("ARD", model.getValueAt(0, FilmColumn.SENDER.index))
        assertEquals(42, model.getValueAt(0, FilmColumn.DURATION.index))
        assertEquals(17, model.getValueAt(0, FilmColumn.SIZE.index))
        assertSame(film, model.getValueAt(0, FilmColumn.REF.index))
    }

    @Test
    fun addAllFiresInsertedRowsForActualInsertedRange() {
        val model = TModelFilm()
        val events = mutableListOf<TableModelEvent>()
        model.addTableModelListener { event -> events += event }

        model.addAll(listOf(DatenFilm(), DatenFilm()))

        val event = events.single()
        assertEquals(TableModelEvent.INSERT, event.type)
        assertEquals(0, event.firstRow)
        assertEquals(1, event.lastRow)
    }

    @Test
    fun addAllWithEmptyListDoesNotFireTableEvent() {
        val model = TModelFilm()
        val events = mutableListOf<TableModelEvent>()
        model.addTableModelListener { event -> events += event }

        model.addAll(emptyList())

        assertTrue(events.isEmpty())
    }
}
