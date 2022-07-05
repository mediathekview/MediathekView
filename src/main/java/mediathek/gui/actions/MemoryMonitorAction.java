package mediathek.gui.actions;

import mediathek.mainwindow.MemoryUsagePanel;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.concurrent.TimeUnit;

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

    static class MemoryMonitorDialog extends JDialog {
        public MemoryMonitorDialog(@NotNull JFrame parent) {
            super(parent, "Speicherverbrauch", false);
            setType(Type.UTILITY);

            MemoryUsagePanel panel = new MemoryUsagePanel(2, TimeUnit.MINUTES);
            panel.setPreferredSize(new Dimension(480, 240));
            getContentPane().add(panel, BorderLayout.CENTER);
            pack();
            panel.new MemoryUsageDataGenerator(1, TimeUnit.SECONDS).start();
        }
    }
}
