package mediathek.gui.dialog.reset;

import mediathek.config.Daten;
import mediathek.gui.dialog.StandardCloseDialog;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("serial")
public class ResetSettingsDialog extends StandardCloseDialog {
    private final Daten daten;

    public ResetSettingsDialog(Frame owner, Daten daten) {
        super(owner, "Programm zurücksetzen", true);
        this.daten = daten;
        setResizable(false);

        pack();
    }

    @Override
    public JComponent createContentPanel() {
        return new ResetSettingsPanel(null, daten);
    }
}
