package mediathek.tool;

import ca.odell.glazedlists.TransformedList;
import ca.odell.glazedlists.event.ListEvent;
import ca.odell.glazedlists.swing.DefaultEventComboBoxModel;
import mediathek.javafx.filterpanel.SenderListBoxModel;

import javax.swing.*;

public class SenderList extends TransformedList<String, String> {

    public SenderList() {
        super(SenderListBoxModel.SENDER_LIST);
    }

    public static ComboBoxModel<String> getSenderListComboBoxModel() {
        DefaultEventComboBoxModel<String> senderModel = new DefaultEventComboBoxModel<>(new SenderList());
        senderModel.setSelectedItem("");

        return senderModel;
    }

    @Override
    protected boolean isWritable() {
        return false;
    }

    @Override
    public void listChanged(ListEvent<String> listEvent) {
    }

    @Override
    public String get(int index) {
        if (index == 0)
            return "";
        else
            return source.get(index - 1);
    }

    public int size() {
        return this.source.size() + 1;
    }

}
