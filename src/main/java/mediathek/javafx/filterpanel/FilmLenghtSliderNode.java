package mediathek.javafx.filterpanel;

import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import org.controlsfx.tools.Borders;

public class FilmLenghtSliderNode extends VBox {
    public final FilmLengthSlider _filmLengthSlider;

    public FilmLenghtSliderNode() {
        Label lblMin = new Label("min");
        Label lblMax = new Label("max");

        var spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);

        HBox hb = new HBox();
        hb.setPadding(new Insets(0,5,0,5));
        hb.getChildren().addAll(new Label("Mindestlänge: "), lblMin,
                spacer,
                new Label("Maximallänge: "), lblMax);

        _filmLengthSlider = new FilmLengthSlider();
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
