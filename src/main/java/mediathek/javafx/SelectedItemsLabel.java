package mediathek.javafx;

import javafx.beans.property.IntegerProperty;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;

/**
 * Displays the number of currently selected entries
 */
public class SelectedItemsLabel extends Label {
    public SelectedItemsLabel(IntegerProperty selectedItemsProperty) {
        super();
        setTooltip(new Tooltip("Ausgewählte Einträge der aktiven Tabelle"));
        textProperty().bind(selectedItemsProperty.asString());
    }
}
