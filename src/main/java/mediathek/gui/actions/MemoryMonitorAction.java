package mediathek.gui.actions;

import mediathek.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class MemoryMonitorAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public MemoryMonitorAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.NAME,"Speicherverbrauch anzeigen");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        mediathekGui.showMemoryMonitor();
    }
}
