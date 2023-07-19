package mediathek.mainwindow;

import javax.swing.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class SelectedListItemsLabel extends JLabel implements PropertyChangeListener {
    public SelectedListItemsLabel(MediathekGui mediathekGui) {
        setText("0");
        setToolTipText("Ausgewählte Einträge der aktiven Tabelle");
        mediathekGui.selectedListItemsProperty.addSelectedItemsChangeListener(this);
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        SwingUtilities.invokeLater(() -> {
            long items = (long) evt.getNewValue();
            setText(Long.toString(items));
        });
    }
}
