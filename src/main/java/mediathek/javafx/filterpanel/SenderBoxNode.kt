package mediathek.javafx.filterpanel

import ca.odell.glazedlists.*
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.javafx.EventObservableList
import ca.odell.glazedlists.javafx.JavaFxThreadProxyEventList
import javafx.event.EventHandler
import javafx.scene.control.ContextMenu
import javafx.scene.control.MenuItem
import mediathek.controller.SenderFilmlistLoadApprover
import mediathek.tool.GermanStringSorter
import org.controlsfx.control.CheckListView

class SenderBoxNode : CheckListView<String?>() {
    internal class ReadOnlyList(val list: EventList<String>) : TransformedList<String, String>(list) {
        init {
            source.addListEventListener(this)
        }

        override fun isWritable(): Boolean {
            return false
        }

        override fun listChanged(listChanges: ListEvent<String>?) {
            updates.forwardEvent(listChanges)
        }
    }

    init {
        //do not display unchecked(unloaded) senders from config...
        val filteredSenderList = FilterList(SenderListBoxModel.providedSenderList)
        filteredSenderList.setMatcher { SenderFilmlistLoadApprover.isApproved(it) }

        val sortedSenderList = SortedList(UniqueList(filteredSenderList))
        sortedSenderList.comparator = GermanStringSorter.getInstance()

        items = EventObservableList(JavaFxThreadProxyEventList(ReadOnlyList(sortedSenderList)))

        val contextMenu = ContextMenu()
        val miClearChecks = MenuItem("Alle Senderfilter zurücksetzen")
        miClearChecks.onAction = EventHandler { checkModel.clearChecks() }
        contextMenu.items.add(miClearChecks)
        setContextMenu(contextMenu)
    }
}
