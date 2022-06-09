package mediathek.gui.actions;

import mediathek.mainwindow.MediathekGui;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class SettingsAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public SettingsAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.NAME, "Einstellungen...");
        putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0));
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/gears.svg"));
        putValue(Action.SHORT_DESCRIPTION, "Einstellungen öffnen");
    }
    @Override
    public void actionPerformed(ActionEvent e) {
        mediathekGui.getSettingsDialog().setVisible(true);
    }
}
