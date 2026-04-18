package mediathek.gui.actions;

import mediathek.gui.dialog.MemoryMonitorDialog;
import org.jspecify.annotations.NonNull;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class MemoryMonitorAction extends AbstractAction {
    private MemoryMonitorDialog dialog;
    private final JFrame parent;

    public MemoryMonitorAction(@NonNull JFrame parent) {
        this.parent = parent;
        putValue(Action.NAME, "Speicherverbrauch anzeigen");
    }

    public void closeMemoryMonitor() {
        if (dialog != null) {
            dialog.dispose();
        }
    }

    public void showMemoryMonitor() {
        if (dialog == null || !dialog.isDisplayable()) {
            dialog = new MemoryMonitorDialog(parent, () -> {
                dialog = null;
                setEnabled(true);
            });
        }
        setEnabled(false);
        dialog.setVisible(true);
        dialog.toFront();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        showMemoryMonitor();
    }

}
