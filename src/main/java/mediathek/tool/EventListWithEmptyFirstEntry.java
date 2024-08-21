package mediathek.tool;

import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.TransformedList;
import ca.odell.glazedlists.event.ListEvent;

/**
 * Read-only event list which also containing an " " for a "select all" selection.
 */
public class EventListWithEmptyFirstEntry extends TransformedList<String, String> {

    public EventListWithEmptyFirstEntry(EventList<String> sourceList) {
        super(sourceList);
        source.addListEventListener(this);
    }

    @Override
    protected boolean isWritable() {
        return false;
    }

    @Override
    public void listChanged(ListEvent<String> listChanges) {
        updates.forwardEvent(listChanges);
    }

    @Override
    public String get(int index) {
        if (index == 0)
            return "";
        else
            return source.get(index - 1);
    }

    public int size() {
        return source.size() + 1;
    }

}
