package mediathek.tool;

import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.TransformedList;
import ca.odell.glazedlists.event.ListEvent;

public class SenderList extends TransformedList<String, String> {

    public SenderList(EventList<String> source) {
        super(source);
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
