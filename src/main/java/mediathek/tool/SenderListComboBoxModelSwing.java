package mediathek.tool;

import ca.odell.glazedlists.FilterList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.swing.DefaultEventComboBoxModel;
import mediathek.controller.SenderFilmlistLoadApproverSwing;
import mediathek.gui.filterpanel.SenderListBoxModelSwing;

public class SenderListComboBoxModelSwing extends DefaultEventComboBoxModel<String> {

    public SenderListComboBoxModelSwing() {
        super(createFilteredSenderList());  // Super-Aufruf als erste Anweisung
    }

    private static FilterList<String> createFilteredSenderList() {
        EventList<String> senderList = SenderListBoxModelSwing.getProvidedSenderList();
        FilterList<String> filteredSenderList = new FilterList<>(senderList);
        filteredSenderList.setMatcher(SenderFilmlistLoadApproverSwing::isApproved);
        return filteredSenderList;
    }
}
