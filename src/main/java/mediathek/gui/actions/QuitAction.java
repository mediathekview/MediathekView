package mediathek.gui.actions;

import mediathek.mainwindow.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class QuitAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public QuitAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.NAME, "Beenden");
        putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, KeyEvent.CTRL_DOWN_MASK));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        mediathekGui.beenden(false, false);
    }
}
