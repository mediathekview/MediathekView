package mediathek.javaswing.filterpanel;

import javax.swing.JComboBox;

public class ThemaComboBoxSwing extends JComboBox<String> {
    public ThemaComboBoxSwing() {
        super();
        addItem("");
        setSelectedIndex(0);
        setEditable(true);
    }
}
