package mediathek.gui.dialog.reset;

import mediathek.config.Daten;
import mediathek.gui.dialog.StandardCloseDialog;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("serial")
public class ResetSettingsDialog extends StandardCloseDialog {
    public ResetSettingsDialog(Frame owner, Daten daten) {
        super(owner, "Programm zurücksetzen", true);
        setResizable(false);
    }

    @Override
    public JComponent createContentPanel() {
        return new ResetSettingsPanel(null);
    }
}
