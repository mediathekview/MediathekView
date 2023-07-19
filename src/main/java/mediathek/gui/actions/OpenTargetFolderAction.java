package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class OpenTargetFolderAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public OpenTargetFolderAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Zielordner öffnen");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.zielordnerOeffnen();
    }
}
