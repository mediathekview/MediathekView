package mediathek.gui.history;

import mediathek.config.Daten;
import mediathek.gui.dialog.StandardCloseDialog;

import javax.swing.*;
import java.awt.*;

public class AboHistoryDialog extends StandardCloseDialog {
    public AboHistoryDialog(Frame owner, Daten daten) {
        super(owner, "Abo-Historie", true);
    }

    @Override
    public JComponent createContentPanel() {
        return new AboHistoryPanel();
    }
}
