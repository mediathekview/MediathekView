package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class AdvanceDownloadsAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public AdvanceDownloadsAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Downloads vorziehen");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/arrow-up.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.downloadsVorziehen();
    }
}
