package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class StartDownloadsAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public StartDownloadsAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Ausgewählte Downloads starten");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/caret-down.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.starten(false);
    }
}
