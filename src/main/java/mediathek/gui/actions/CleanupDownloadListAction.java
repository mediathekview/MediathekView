package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.swing.IconUtils;
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class CleanupDownloadListAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public CleanupDownloadListAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Liste säubern");
        putValue(Action.SHORT_DESCRIPTION, "Liste säubern");
        putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.ERASER));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.cleanupDownloads();
    }
}
