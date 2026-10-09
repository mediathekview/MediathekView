package ca.odell.glazedlists

import ca.odell.glazedlists.swing.DefaultEventListModel
import ca.odell.glazedlists.swing.DefaultEventSelectionModel
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import javax.swing.SwingUtilities
import javax.swing.event.ListDataEvent
import javax.swing.event.ListDataListener
import javax.swing.event.ListSelectionListener

internal class ListenerDispatchTest {
    @Test
    fun listModelDefersListenersAddedDuringNotification() {
        val source = BasicEventList<String>()
        val model = DefaultEventListModel(source)
        val notifications = mutableListOf<String>()
        val lateListener = ListDataListenerAdapter { notifications += "late" }
        var registered = false

        model.addListDataListener(ListDataListenerAdapter {
            notifications += "initial"
            if (!registered) {
                model.addListDataListener(lateListener)
                registered = true
            }
        })

        SwingUtilities.invokeAndWait { source.add("first") }
        assertEquals(listOf("initial"), notifications)

        SwingUtilities.invokeAndWait { source.add("second") }
        assertEquals(listOf("initial", "initial", "late"), notifications)
    }

    @Test
    fun swingSelectionModelDefersListenersAddedDuringNotification() {
        val source = BasicEventList<String>().apply { addAll(listOf("first", "second")) }
        val model = DefaultEventSelectionModel(source)
        val notifications = mutableListOf<String>()
        val lateListener = ListSelectionListener { notifications += "late" }
        var registered = false

        model.addListSelectionListener(ListSelectionListener {
            notifications += "initial"
            if (!registered) {
                model.addListSelectionListener(lateListener)
                registered = true
            }
        })

        model.setSelectionInterval(0, 0)
        assertEquals(listOf("initial"), notifications)

        model.setSelectionInterval(1, 1)
        assertEquals(listOf("initial", "initial", "late"), notifications)
    }

    @Test
    fun listSelectionDefersListenersAddedDuringNotification() {
        val source = BasicEventList<String>().apply { addAll(listOf("first", "second")) }
        val selection = ListSelection(source)
        val notifications = mutableListOf<String>()
        val lateListener = ListSelection.Listener { _, _ -> notifications += "late" }
        var registered = false

        selection.addSelectionListener { _, _ ->
            notifications += "initial"
            if (!registered) {
                selection.addSelectionListener(lateListener)
                registered = true
            }
        }

        selection.select(0)
        assertEquals(listOf("initial"), notifications)

        selection.select(1)
        assertEquals(listOf("initial", "initial", "late"), notifications)
    }

    private class ListDataListenerAdapter(private val callback: () -> Unit) : ListDataListener {
        override fun intervalAdded(event: ListDataEvent) = callback()

        override fun intervalRemoved(event: ListDataEvent) = callback()

        override fun contentsChanged(event: ListDataEvent) = callback()
    }
}
