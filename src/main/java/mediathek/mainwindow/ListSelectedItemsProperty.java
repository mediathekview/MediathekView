package mediathek.mainwindow;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

public class ListSelectedItemsProperty {
    private final PropertyChangeSupport pcs = new PropertyChangeSupport(this);
    private long selectedItems;

    public ListSelectedItemsProperty(long selectedItems) {
        this.selectedItems = selectedItems;
    }

    public void addSelectedItemsChangeListener(PropertyChangeListener listener) {
        this.pcs.addPropertyChangeListener(listener);
    }

    public void setSelectedItems(long selectedItems) {
        long oldValue = this.selectedItems;
        this.selectedItems = selectedItems;
        this.pcs.firePropertyChange("sel_items", oldValue, selectedItems);
    }
}
