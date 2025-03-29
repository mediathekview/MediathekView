package mediathek.gui.filterpanel;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

public class ZeitraumSpinnerSwing extends JSpinner {
    public static final String UNLIMITED_VALUE = "∞";

    public ZeitraumSpinnerSwing() {
        super();

        List<String> days = new ArrayList<>();
        days.add(UNLIMITED_VALUE);
        for (int i = 1; i <= 365; i++) {
            days.add(String.valueOf(i));
        }

        setModel(new SpinnerListModel(days));
        setValue(UNLIMITED_VALUE);

        if (getEditor() instanceof JSpinner.DefaultEditor) {
            ((JSpinner.DefaultEditor) getEditor()).getTextField().setEditable(true);
        }

        setToolTipText("<html>Geben Sie Werte von 1-365 manuell ein und drücken Sie ENTER.<br>"
                + "Für unbegrenzten Zeitraum das '∞'-Symbol eingeben.</html>");
    }
}
