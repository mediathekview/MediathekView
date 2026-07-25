package mediathek.gui.duplicates

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.swing.AdvancedTableModel
import mediathek.daten.DatenFilm
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.duplicates.details.DuplicateFilmDetailsDialog
import mediathek.gui.duplicates.overview.FilmDuplicateOverviewDialog
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Assumptions.assumeFalse
import org.junit.jupiter.api.Test
import java.awt.GraphicsEnvironment
import javax.swing.JFrame
import javax.swing.SwingUtilities

internal class DuplicateDialogLifecycleTest {
    @Test
    fun `details dialog disposal detaches its sorted list from the source`() {
        assumeUiAvailable()
        onEdt {
            val owner = JFrame()
            val dialog = DuplicateFilmDetailsDialog(owner, FilmCatalog(), DatenFilm())
            val source = privateField<BasicEventList<DatenFilm>>(dialog, "duplicateList")
            val sorted = privateField<SortedList<DatenFilm>>(dialog, "sortedList")
            val model = privateField<AdvancedTableModel<DatenFilm>>(dialog, "tableModel")
            var sortedEvents = 0
            sorted.addListEventListener { sortedEvents++ }

            assertSame(sorted, modelSource(model))
            dialog.dispose()
            dialog.dispose()
            val eventsAfterDisposal = sortedEvents
            source.add(DatenFilm())

            assertEquals(eventsAfterDisposal, sortedEvents)
            assertModelDisposed(model)
            owner.dispose()
        }
    }

    @Test
    fun `overview dialog disposal releases its table model`() {
        assumeUiAvailable()
        onEdt {
            val owner = JFrame()
            val dialog = FilmDuplicateOverviewDialog(owner, FilmCatalog())
            val source = privateField<EventList<DatenFilm>>(dialog, "filmList")
            val model = privateField<AdvancedTableModel<DatenFilm>>(dialog, "tableModel")

            assertSame(source, modelSource(model))
            dialog.dispose()
            dialog.dispose()

            assertModelDisposed(model)
            owner.dispose()
        }
    }

    private fun assumeUiAvailable() {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing dialog test requires a non-headless environment")
    }

    private fun onEdt(action: () -> Unit) {
        SwingUtilities.invokeAndWait(action)
    }

    @Suppress("UNCHECKED_CAST")
    private inline fun <reified T> privateField(instance: Any, name: String): T {
        val field = instance.javaClass.getDeclaredField(name)
        field.isAccessible = true
        return field.get(instance) as T
    }

    private fun assertModelDisposed(model: AdvancedTableModel<DatenFilm>) {
        assertNull(modelSource(model))
    }

    private fun modelSource(model: AdvancedTableModel<DatenFilm>): Any? {
        val sourceField = model.javaClass.getDeclaredField("source")
        sourceField.isAccessible = true
        return sourceField.get(model)
    }
}
