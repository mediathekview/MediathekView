package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class CleanupDownloadListAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public CleanupDownloadListAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Liste säubern");
        putValue(Action.SHORT_DESCRIPTION, "Liste säubern");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/eraser.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.cleanupDownloads();
    }
}
