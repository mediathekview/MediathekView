package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class StopAllDownloadsAction extends AbstractAction {

    private final GuiDownloads guiDownloads;

    public StopAllDownloadsAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Alle Downloads stoppen");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.stoppen(true);
    }
}
