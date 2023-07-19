package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class DeleteDownloadAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public DeleteDownloadAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/xmark.svg"));
        putValue(Action.NAME, "gespeicherten Film (Datei) löschen");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.filmLoeschen_();
    }
}
