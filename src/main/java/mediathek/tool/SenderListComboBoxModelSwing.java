package mediathek.tool;

import ca.odell.glazedlists.swing.DefaultEventComboBoxModel;
import mediathek.javaswing.filterpanel.SenderListBoxModelSwing;

public class SenderListComboBoxModelSwing extends DefaultEventComboBoxModel<String> {

    public SenderListComboBoxModelSwing() {
        super(new EventListWithEmptyFirstEntry(SenderListBoxModelSwing.getReadOnlySenderList()));
    }
}
