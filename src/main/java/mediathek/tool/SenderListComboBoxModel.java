package mediathek.tool;

import ca.odell.glazedlists.swing.DefaultEventComboBoxModel;
import mediathek.config.Daten;

public class SenderListComboBoxModel extends DefaultEventComboBoxModel<String> {

    public SenderListComboBoxModel() {
        super(new EventListWithEmptyFirstEntry(Daten.getInstance().getAllSendersList()));
        setSelectedItem("");
    }
}
