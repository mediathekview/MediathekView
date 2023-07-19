package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class RefreshDownloadListAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public RefreshDownloadListAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Liste der Downloads aktualisieren");
        putValue(Action.SHORT_DESCRIPTION, "Downloadliste aktualisieren");
        putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_W, KeyEvent.CTRL_DOWN_MASK));
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/arrows-rotate.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.updateDownloads();
    }
}
