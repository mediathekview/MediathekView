package mediathek.gui.history;

import mediathek.config.Daten;
import mediathek.gui.dialog.StandardCloseDialog;

import javax.swing.*;
import java.awt.*;

public class AboHistoryDialog extends StandardCloseDialog {
    private final Daten daten;

    public AboHistoryDialog(Frame owner, Daten daten) {
        super(owner, "Erledigte Abos", true);
        this.daten = daten;
    }

    @Override
    public JComponent createContentPanel() {
        return new AboHistoryPanel(daten);
    }
}
