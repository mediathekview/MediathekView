package mediathek.javafx.filterpanel;

import javafx.util.StringConverter;
import org.controlsfx.control.RangeSlider;

public class FilmLengthSlider extends RangeSlider {
    public final static int UNLIMITED_VALUE = 110;

    public FilmLengthSlider() {
        super(0,UNLIMITED_VALUE,0,UNLIMITED_VALUE);
        setShowTickMarks(true);
        setShowTickLabels(true);
        setBlockIncrement(1);
        setMajorTickUnit(10);
        setSnapToTicks(true);
        setLabelFormatter(new StringConverter<>() {
            @Override
            public String toString(Number object) {
                if (object.intValue() == UNLIMITED_VALUE)
                    return "∞";
                else
                    return String.valueOf(object.intValue());
            }

            @Override
            public Number fromString(String string) {
                return Double.parseDouble(string);
            }
        });}
}
