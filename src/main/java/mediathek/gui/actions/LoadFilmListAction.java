package mediathek.gui.actions;

import jiconfont.icons.font_awesome.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.mainwindow.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class LoadFilmListAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public LoadFilmListAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0));
        putValue(Action.SMALL_ICON, IconFontSwing.buildIcon(FontAwesome.CLOUD_DOWNLOAD, 16));
        putValue(Action.NAME, "Neue Filmliste laden...");
        putValue(Action.SHORT_DESCRIPTION, "Neue Filmliste laden");
    }
    @Override
    public void actionPerformed(ActionEvent e) {
        mediathekGui.performFilmListLoadOperation(false);
    }
}
