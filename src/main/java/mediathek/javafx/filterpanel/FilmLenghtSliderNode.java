package mediathek.javafx.filterpanel;

import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.controlsfx.tools.Borders;

public class FilmLenghtSliderNode extends VBox {
    public final FilmLengthSlider _filmLengthSlider;

    public FilmLenghtSliderNode() {
        Label lblMin = new Label("min");
        Label lblMax = new Label("max");

        HBox hb = new HBox();
        hb.getChildren().addAll(new Label("Mindestlänge:"), lblMin);

        HBox hb2 = new HBox();
        hb2.getChildren().addAll(new Label("Maximallänge:"), lblMax);

        VBox vb2 = new VBox();
        vb2.getChildren().addAll(hb, hb2);

        _filmLengthSlider = new FilmLengthSlider();
        _filmLengthSlider.lowValueProperty().addListener((observable, oldValue, newValue) -> lblMin.setText(String.valueOf(newValue.intValue())));
        _filmLengthSlider.highValueProperty().addListener((observable, oldValue, newValue) -> lblMax.setText(_filmLengthSlider.getLabelFormatter().toString(newValue)));
        vb2.getChildren().add(_filmLengthSlider);

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
