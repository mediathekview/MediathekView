package mediathek.tool;

import ca.odell.glazedlists.TransformedList;
import ca.odell.glazedlists.event.ListEvent;
import mediathek.javafx.filterpanel.SenderListBoxModel;

/**
 * Read-only model of sender also containing an " " for all sender selection.
 */
public class SenderListModel extends TransformedList<String, String> {

    public SenderListModel() {
        super(SenderListBoxModel.getReadOnlySenderList());
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
