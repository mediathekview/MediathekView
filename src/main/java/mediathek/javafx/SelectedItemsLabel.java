package mediathek.javafx;

import javafx.beans.property.IntegerProperty;
import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.StackPane;

/**
 * Displays the number of currently selected entries
 */
public class SelectedItemsLabel extends StackPane {

    public SelectedItemsLabel(IntegerProperty selectedItemsProperty) {
        super();

        Label textLabel = new Label();
        textLabel.setTooltip(new Tooltip("Ausgewählte Einträge der aktiven Tabelle"));
        textLabel.textProperty().bind(selectedItemsProperty.asString());

        setMargin(textLabel,new Insets(0,4,0,4));
        getChildren().add(textLabel);
    }
}
