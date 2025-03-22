package mediathek.javaswing.filterpanel;

import javax.swing.*;
import java.awt.*;
import java.util.Hashtable;

public class FilmLengthSliderSwing extends RangeSliderSwing {
    public static final int UNLIMITED_VALUE = 110;

    public FilmLengthSliderSwing() {
        super(0, UNLIMITED_VALUE); // Ruft den Konstruktor der RangeSliderSwing-Klasse auf

        // Konfiguriere die Eigenschaften des Sliders
        setMajorTickSpacing(10);
        setMinorTickSpacing(1);
        setPaintTicks(true);
        setPaintLabels(true);

        // Setze benutzerdefinierte Labels für die Ticks
        setLabelTable(createLabelTable());

    }

    private Hashtable<Integer, JLabel> createLabelTable() {
        Hashtable<Integer, JLabel> labelTable = new Hashtable<>();

        // Erstelle Labels für die Ticks, wobei jeder Tick-Wert einer JLabel zugeordnet wird
        for (int i = 0; i <= UNLIMITED_VALUE; i += 10) {
            String label = (i == UNLIMITED_VALUE) ? "∞" : String.valueOf(i);
            labelTable.put(i, new JLabel(label));
        }

        return labelTable;
    }
}
