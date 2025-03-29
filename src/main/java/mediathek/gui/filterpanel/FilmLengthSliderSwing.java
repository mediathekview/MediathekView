package mediathek.gui.filterpanel;

import com.jidesoft.swing.RangeSlider;
;

public class FilmLengthSliderSwing extends RangeSlider {
    public final static int UNLIMITED_VALUE = 110;

    public FilmLengthSliderSwing() {

        super(0, UNLIMITED_VALUE, 0, UNLIMITED_VALUE);
        setSnapToTicks(false);
        setMajorTickSpacing(10);
        setPaintLabels(true);
        setPaintTicks(true);
        setPaintTrack(true);
    }
}
