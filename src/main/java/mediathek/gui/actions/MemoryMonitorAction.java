package mediathek.gui.actions;

import mediathek.gui.dialog.MemoryMonitorDialog;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class MemoryMonitorAction extends AbstractAction {
    private MemoryMonitorDialog dialog;
    private final JFrame parent;

    public MemoryMonitorAction(@NotNull JFrame parent) {
        this.parent = parent;
        putValue(Action.NAME, "Speicherverbrauch anzeigen");
    }

    public void closeMemoryMonitor() {
        if (dialog != null) {
            dialog.dispose();
        }
    }

    public void showMemoryMonitor() {
        if (dialog == null)
            dialog = new MemoryMonitorDialog(parent);
        dialog.setVisible(true);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        showMemoryMonitor();
    }

}
