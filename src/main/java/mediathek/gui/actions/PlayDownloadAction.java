package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class PlayDownloadAction extends AbstractAction {
    private static final String ACTION_TEXT = "Gespeicherten Film abspielen";
    private final GuiDownloads guiDownloads;

    public PlayDownloadAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, ACTION_TEXT);
        putValue(Action.SHORT_DESCRIPTION, ACTION_TEXT);
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.filmAbspielen();
    }
}
