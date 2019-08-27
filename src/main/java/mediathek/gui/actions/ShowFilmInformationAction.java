package mediathek.gui.actions;

import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class ShowFilmInformationAction extends AbstractAction {

    public ShowFilmInformationAction(boolean showAccelerator) {
        putValue(NAME,"Filminformationen anzeigen");
        if (showAccelerator)
            putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_I, GuiFunktionen.getPlatformControlKey()));
    }
    @Override
    public void actionPerformed(ActionEvent e) {
        if (!MediathekGui.ui().getFilmInfoDialog().isVisible()) {
            MediathekGui.ui().getFilmInfoDialog().showInfo();
        }
    }
}
