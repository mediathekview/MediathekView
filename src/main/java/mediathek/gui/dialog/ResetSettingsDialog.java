package mediathek.gui.dialog;

import com.jidesoft.dialog.ButtonPanel;
import com.jidesoft.dialog.StandardDialog;
import mediathek.config.Daten;
import mediathek.gui.ResetSettingsPanel;
import mediathek.gui.actions.DisposeDialogAction;
import mediathek.tool.EscapeKeyHandler;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

@SuppressWarnings("serial")
public class ResetSettingsDialog extends StandardDialog {
    private final Daten daten;

    public ResetSettingsDialog(Frame owner, Daten daten) {
        super(owner, "Programm zurücksetzen", true);
        this.daten = daten;

        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        setResizable(false);

        EscapeKeyHandler.installHandler(this, this::dispose);

        pack();
    }

    @Override
    public JComponent createBannerPanel() {
        return null;
    }

    @Override
    public JComponent createContentPanel() {
        return new ResetSettingsPanel(null, daten);
    }

    @Override
    public ButtonPanel createButtonPanel() {
        ButtonPanel pnl = new ButtonPanel();
        JButton btn = new JButton(new DisposeDialogAction(this, "Schließen", "Dialog schließen"));
        getRootPane().setDefaultButton(btn);
        pnl.addButton(btn);
        pnl.setBorder(new EmptyBorder(5, 5, 5, 5));
        return pnl;
    }
}
