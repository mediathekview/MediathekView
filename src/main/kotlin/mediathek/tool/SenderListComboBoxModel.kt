package mediathek.tool

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.swing.DefaultEventComboBoxModel

class SenderListComboBoxModel(senders: EventList<String>) : DefaultEventComboBoxModel<String>(
    EventListWithEmptyFirstEntry(senders)
) {
    init {
        selectedItem = ""
    }
}
