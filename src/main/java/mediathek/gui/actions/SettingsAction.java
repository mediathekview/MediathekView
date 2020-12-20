package mediathek.gui.actions;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.mainwindow.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class SettingsAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public SettingsAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.NAME, "Einstellungen...");
        putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0));
        putValue(Action.SMALL_ICON, IconFontSwing.buildIcon(FontAwesome.COGS, 16));
    }
    @Override
    public void actionPerformed(ActionEvent e) {
        mediathekGui.getSettingsDialog().setVisible(true);
    }
}
