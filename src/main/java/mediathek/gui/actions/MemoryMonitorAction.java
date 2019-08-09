package mediathek.gui.actions;

import javafx.application.Platform;
import mediathek.javafx.MemoryMonitor;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class MemoryMonitorAction extends AbstractAction {
    private MemoryMonitor memoryMonitor;

    public MemoryMonitorAction() {
        putValue(Action.NAME,"Speicherverbrauch anzeigen");
    }

    public void closeMemoryMonitor() {
        if (memoryMonitor != null)
            Platform.runLater(() -> memoryMonitor.close());
    }

    public void showMemoryMonitor() {
        Platform.runLater(() -> {
            if (memoryMonitor == null) {
                memoryMonitor = new MemoryMonitor();
            }

            memoryMonitor.show();
        });
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        showMemoryMonitor();
    }
}
