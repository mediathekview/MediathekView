package mediathek.tool

import ca.odell.glazedlists.swing.DefaultEventComboBoxModel
import mediathek.config.Daten

class SenderListComboBoxModel : DefaultEventComboBoxModel<String>(
    EventListWithEmptyFirstEntry(Daten.getInstance().allSendersList)
) {
    init {
        selectedItem = ""
    }
}
