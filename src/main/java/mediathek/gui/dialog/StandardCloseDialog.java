package mediathek.gui.dialog;

import com.jidesoft.dialog.ButtonPanel;
import com.jidesoft.dialog.StandardDialog;
import mediathek.gui.actions.DisposeDialogAction;
import mediathek.tool.EscapeKeyHandler;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * A standard swing dialog template with a close button.
 */
public abstract class StandardCloseDialog extends StandardDialog {
    public StandardCloseDialog(Frame owner, String title, boolean modal) {
        super(owner, title, modal);
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        EscapeKeyHandler.installHandler(this, this::dispose);
    }

    @Override
    public abstract JComponent createContentPanel();

    @Override
    public final ButtonPanel createButtonPanel() {
        ButtonPanel pnl = new ButtonPanel();
        JButton btn = new JButton(new DisposeDialogAction(this, "Schließen", "Dialog schließen"));
        getRootPane().setDefaultButton(btn);
        pnl.addButton(btn);
        pnl.setBorder(new EmptyBorder(5, 5, 5, 5));
        return pnl;
    }
}
