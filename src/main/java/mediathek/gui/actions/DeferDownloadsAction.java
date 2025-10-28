package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.swing.IconUtils;
import org.kordamp.ikonli.fontawesome6.FontAwesomeRegular;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class DeferDownloadsAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public DeferDownloadsAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.NAME, "Downloads zurückstellen");
        putValue(Action.SHORT_DESCRIPTION, "Downloads zurückstellen");
        putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeRegular.CLOCK));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.downloadLoeschen(false);
    }
}
