package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class EditDownloadAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public EditDownloadAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Download ändern");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/pen-to-square.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.editDownload();
    }
}
