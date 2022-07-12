package mediathek.gui.history;

import mediathek.gui.dialog.StandardCloseDialog;

import javax.swing.*;
import java.awt.*;

public class AboHistoryDialog extends StandardCloseDialog {
    public AboHistoryDialog(Frame owner) {
        super(owner, "Abo-Historie", true);
    }

    @Override
    public JComponent createContentPanel() {
        return new PanelErledigteUrls();
    }
}
