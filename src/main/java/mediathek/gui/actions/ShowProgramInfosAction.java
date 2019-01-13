package mediathek.gui.actions;

import mediathek.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ShowProgramInfosAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public ShowProgramInfosAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.NAME,"Programminfos anzeigen...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        mediathekGui.searchForUpdateOrShowProgramInfos(true);
    }
}
