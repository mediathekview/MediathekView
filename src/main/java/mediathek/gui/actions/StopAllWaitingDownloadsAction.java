package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class StopAllWaitingDownloadsAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public StopAllWaitingDownloadsAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Wartende Downloads stoppen");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.stopAllWaitingDownloads();
    }
}
