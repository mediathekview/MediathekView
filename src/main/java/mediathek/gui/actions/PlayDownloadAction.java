package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class PlayDownloadAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public PlayDownloadAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Gespeicherten Film abspielen");
        putValue(Action.SHORT_DESCRIPTION, "Film abspielen");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.filmAbspielen();
    }
}
