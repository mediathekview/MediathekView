package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class DeleteDownloadsAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public DeleteDownloadsAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Downloads aus Liste entfernen");
        putValue(Action.SHORT_DESCRIPTION, "Downloads entfernen");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.downloadLoeschen(true);
    }
}
