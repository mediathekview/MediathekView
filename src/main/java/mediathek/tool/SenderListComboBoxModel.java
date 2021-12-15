package mediathek.tool;

import ca.odell.glazedlists.swing.DefaultEventComboBoxModel;

public class SenderListComboBoxModel extends DefaultEventComboBoxModel<String> {

    public SenderListComboBoxModel() {
        super(new SenderListModel());
        setSelectedItem("");
    }
}
