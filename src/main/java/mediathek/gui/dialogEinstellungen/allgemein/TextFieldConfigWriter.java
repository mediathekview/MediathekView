package mediathek.gui.dialogEinstellungen.allgemein;

import mediathek.tool.ApplicationConfiguration;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Generic config writer for textfields.
 * Action will be performed on Swing EDT.
 */
final class TextFieldConfigWriter implements ActionListener {
    private final JTextField control;
    private final String configPropertyKey;

    public TextFieldConfigWriter(JTextField control, String configPropertyKey) {
        this.control = control;
        this.configPropertyKey = configPropertyKey;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        final String controlText = control.getText();
        ApplicationConfiguration.getConfiguration().setProperty(configPropertyKey, controlText);
    }
}
