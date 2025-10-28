package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.swing.IconUtils;
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid;

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
        putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.REDO_ALT));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.updateDownloads();
    }
}
