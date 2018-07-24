package mediathek.javafx;

import javafx.geometry.Orientation;
import javafx.scene.control.Separator;

/**
 * A JavaFX in default vertical orientation
 */
public class VerticalSeparator extends Separator {
    public VerticalSeparator() {
        super();
        setOrientation(Orientation.VERTICAL);
    }
}
