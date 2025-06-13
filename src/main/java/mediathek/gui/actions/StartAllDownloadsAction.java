package mediathek.gui.actions;

import mediathek.gui.tabs.tab_downloads.GuiDownloads;
import mediathek.swing.IconUtils;
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class StartAllDownloadsAction extends AbstractAction {
    private final GuiDownloads guiDownloads;

    public StartAllDownloadsAction(GuiDownloads guiDownloads) {
        this.guiDownloads = guiDownloads;
        putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.ANGLE_DOUBLE_DOWN));
        putValue(Action.SHORT_DESCRIPTION, "Alle Downloads starten");
        putValue(Action.NAME, "Alle Downloads starten");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        guiDownloads.starten(true);
    }
}
