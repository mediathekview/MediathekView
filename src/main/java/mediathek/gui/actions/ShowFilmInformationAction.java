package mediathek.gui.actions;

import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class ShowFilmInformationAction extends AbstractAction {

    public ShowFilmInformationAction() {
        putValue(Action.NAME, "Filminformation anzeigen");
        putValue(Action.SHORT_DESCRIPTION, "Filminformation anzeigen");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-info.svg", 14f));
        putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_I, GuiFunktionen.getPlatformControlKey()));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (!MediathekGui.ui().getFilmInfoDialog().isVisible()) {
            MediathekGui.ui().getFilmInfoDialog().showInfo();
        }
    }
}
