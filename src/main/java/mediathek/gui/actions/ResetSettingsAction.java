package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.gui.dialog.ResetSettingsDialog;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;
import java.awt.event.ActionEvent;

@SuppressWarnings("serial")
public class ResetSettingsAction extends AbstractAction {
    private JFrame owner = null;
    private Daten daten = null;

    public ResetSettingsAction(JFrame owner, Daten daten) {
        super("Einstellungen zurücksetzen...");
        this.owner = owner;
        this.daten = daten;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        ResetSettingsDialog dialog = new ResetSettingsDialog(owner, daten);
        GuiFunktionen.centerOnScreen(dialog, false);
        dialog.setVisible(true);
    }
}
