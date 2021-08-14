package mediathek.gui.actions;

import mediathek.gui.history.AboHistoryDialog;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

public class ShowAboHistoryAction extends AbstractAction {
    private final Frame owner;

    public ShowAboHistoryAction(Frame owner) {
        this.owner = owner;

        putValue(Action.NAME, "Abo-Historie anzeigen...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        AboHistoryDialog dialog = new AboHistoryDialog(owner);
        dialog.pack();
        dialog.setVisible(true);
    }

}
