package mediathek.javafx.filterpanel;

import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import org.controlsfx.tools.Borders;

public class FilmLenghtSliderNode extends VBox {
    public final FilmLengthSlider _filmLengthSlider = new FilmLengthSlider();
    private final Label lblMin = new Label("min");
    private final Label lblMax = new Label("max");

    public FilmLenghtSliderNode() {
        var spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);

        HBox hb = new HBox();
        hb.setPadding(new Insets(0, 5, 0, 5));
        Label lblMaximalLaenge = new Label("Maximallänge: ");
        Label lblMindestlaenge = new Label("Mindestlänge: ");
        hb.getChildren().addAll(lblMindestlaenge,
                lblMin,
                spacer,
                lblMaximalLaenge,
                lblMax);

        _filmLengthSlider.lowValueProperty().addListener((observable, oldValue, newValue) -> lblMin.setText(String.valueOf(newValue.intValue())));
        _filmLengthSlider.highValueProperty().addListener((observable, oldValue, newValue) -> lblMax.setText(_filmLengthSlider.getLabelFormatter().toString(newValue)));

        VBox vb2 = new VBox();
        vb2.getChildren().addAll(hb, _filmLengthSlider);

        lblMin.setText(String.valueOf((int) _filmLengthSlider.getLowValue()));
        lblMax.setText(_filmLengthSlider.getLabelFormatter().toString(_filmLengthSlider.getHighValue()));

        var result = Borders.wrap(vb2)
                .lineBorder()
                .innerPadding(4)
                .outerPadding(4)
                .buildAll();

        getChildren().add(result);
    }
}
