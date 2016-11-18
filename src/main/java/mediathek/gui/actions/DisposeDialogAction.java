package mediathek.gui.actions;

import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * Generic action for disposing dialogs.
 */
public class DisposeDialogAction extends AbstractAction {

    private final JDialog dlg;

    public DisposeDialogAction(JDialog dlg) {
        super();
        putValue(NAME, "Schließen");
        putValue(SHORT_DESCRIPTION, "Dialog schließen");
        this.dlg = dlg;
    }

    public void actionPerformed(ActionEvent e) {
        dlg.dispose();
    }
}
