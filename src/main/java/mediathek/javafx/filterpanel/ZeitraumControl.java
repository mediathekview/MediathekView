package mediathek.javafx.filterpanel;

import javafx.scene.control.Label;
import javafx.scene.control.Spinner;
import javafx.scene.layout.FlowPane;

public class ZeitraumControl extends FlowPane {
    public Spinner<String> spinner;

    public ZeitraumControl() {
        spinner = new ZeitraumSpinner();

        setHgap(4);
        getChildren().addAll(new Label("Zeitraum:"),
                spinner,
                new Label("Tage"));
    }
}
