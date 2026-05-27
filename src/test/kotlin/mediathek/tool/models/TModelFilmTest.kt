package mediathek.tool.models

import mediathek.daten.DatenFilm
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import javax.swing.event.TableModelEvent

internal class TModelFilmTest {
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
