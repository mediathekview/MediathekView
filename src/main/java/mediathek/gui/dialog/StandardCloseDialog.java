package mediathek.gui.dialog;

import mediathek.gui.actions.DisposeDialogAction;
import mediathek.tool.EscapeKeyHandler;

import javax.swing.*;
import java.awt.*;

/**
 * A standard swing dialog template with a close button.
 */
public abstract class StandardCloseDialog extends JDialog {
    public StandardCloseDialog(Frame owner, String title, boolean modal) {
        super(owner, title, modal);
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        EscapeKeyHandler.installHandler(this, this::dispose);

        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(createContentPanel(), BorderLayout.CENTER);
        var buttonPanel = new ButtonPanel();
        buttonPanel.add(createButtonPanel(),BorderLayout.EAST);
        contentPane.add(buttonPanel, BorderLayout.SOUTH);
        pack();
    }

    public abstract JComponent createContentPanel();

    public final ButtonFlowPanel createButtonPanel() {
        ButtonFlowPanel pnl = new ButtonFlowPanel();
        JButton btn = new JButton(new DisposeDialogAction(this, "Schließen", "Dialog schließen"));
        getRootPane().setDefaultButton(btn);
        pnl.add(btn);
        return pnl;
    }
}
