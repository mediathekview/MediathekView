package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class StartAllDownloadsTimedAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public StartAllDownloadsTimedAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Alle Downloads zeitverzögert starten...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.startAllDownloadsAtSpecificTime();
    }
}
