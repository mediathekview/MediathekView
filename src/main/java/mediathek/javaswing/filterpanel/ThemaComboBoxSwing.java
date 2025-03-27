package mediathek.javaswing.filterpanel;

import java.util.Objects;

public class ThemaComboBoxSwing extends FilterableComboBox<String> {
    public ThemaComboBoxSwing() {
        super();
        addItem("");
        setSelectedIndex(0);
        setEditable(true);
    }

    public String getSelectedThema() {
        return Objects.requireNonNull(getSelectedItem()).toString();
    }

}
