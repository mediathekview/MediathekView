package mediathek.gui.actions;

import mediathek.javafx.MemoryMonitor;
import mediathek.javafx.tool.JavaFxUtils;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class MemoryMonitorAction extends AbstractAction {
    private MemoryMonitor memoryMonitor;

    public MemoryMonitorAction() {
        putValue(Action.NAME,"Speicherverbrauch anzeigen");
    }

    public void closeMemoryMonitor() {
        if (memoryMonitor != null)
            JavaFxUtils.invokeInFxThreadAndWait(() -> memoryMonitor.close());
    }

    public void showMemoryMonitor() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
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
