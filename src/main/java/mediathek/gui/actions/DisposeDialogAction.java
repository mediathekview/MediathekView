package mediathek.gui.actions;

import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * Generic action for disposing dialogs.
 */
public class DisposeDialogAction extends AbstractAction {

    private final JDialog dlg;

    public DisposeDialogAction(JDialog dlg, String description, String shortDescription) {
        super();
        putValue(NAME, description);
        putValue(SHORT_DESCRIPTION, shortDescription);
        this.dlg = dlg;
    }

    public void actionPerformed(ActionEvent e) {
        dlg.dispose();
    }
}
