package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class StopDownloadsAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public StopDownloadsAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Ausgewählte Downloads stoppen");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.stoppen(false);
    }
}
