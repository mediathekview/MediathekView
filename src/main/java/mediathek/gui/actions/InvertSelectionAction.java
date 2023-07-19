package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class InvertSelectionAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public InvertSelectionAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Auswahl umkehren");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.getTableComponent().invertSelection();
    }
}
