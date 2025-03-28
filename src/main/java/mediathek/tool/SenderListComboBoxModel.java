package mediathek.tool;

import ca.odell.glazedlists.swing.DefaultEventComboBoxModel;
import mediathek.gui.filterpanel.filterpanel.SenderListBoxModelSwing;

public class SenderListComboBoxModel extends DefaultEventComboBoxModel<String> {

    public SenderListComboBoxModel() {
        super(new EventListWithEmptyFirstEntry(SenderListBoxModelSwing.getReadOnlySenderList()));
        setSelectedItem("");
    }
}
