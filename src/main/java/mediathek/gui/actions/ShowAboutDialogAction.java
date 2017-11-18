package mediathek.gui.actions;

import mediathek.gui.dialog.AboutDialog;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;
import java.awt.event.ActionEvent;

@SuppressWarnings("serial")
public class ShowAboutDialogAction extends AbstractAction {
    private JFrame owner = null;

    public ShowAboutDialogAction(JFrame owner) {
        super("Über dieses Programm...");
        this.owner = owner;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        AboutDialog aboutDialog = new AboutDialog(owner);
        GuiFunktionen.centerOnScreen(aboutDialog, false);
        aboutDialog.setVisible(true);
        aboutDialog.dispose();
    }
}
