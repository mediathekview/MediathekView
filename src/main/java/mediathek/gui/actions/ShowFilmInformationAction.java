package mediathek.gui.actions;

import mediathek.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ShowFilmInformationAction extends AbstractAction {

    @Override
    public void actionPerformed(ActionEvent e) {
        if (!MediathekGui.ui().getFilmInfoDialog().isVisible()) {
            MediathekGui.ui().getFilmInfoDialog().showInfo();
        }
    }
}
