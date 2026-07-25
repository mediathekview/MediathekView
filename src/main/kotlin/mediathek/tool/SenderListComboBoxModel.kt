package mediathek.tool

import javax.swing.DefaultComboBoxModel

class SenderListComboBoxModel(senders: List<String>) : DefaultComboBoxModel<String>(
    (listOf("") + senders).toTypedArray(),
) {
    init {
        selectedItem = ""
    }
}
