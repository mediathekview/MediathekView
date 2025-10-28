package mediathek.gui.actions;

import mediathek.mainwindow.MediathekGui;
import mediathek.swing.IconUtils;
import org.kordamp.ikonli.materialdesign2.MaterialDesignF;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ManageBookmarkAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public ManageBookmarkAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.NAME, "Merkliste verwalten...");
        putValue(Action.SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignF.FILE_DOCUMENT));
        putValue(Action.SHORT_DESCRIPTION, "Merkliste verwalten");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        mediathekGui.tabFilme.showManageBookmarkWindow();
    }
}
