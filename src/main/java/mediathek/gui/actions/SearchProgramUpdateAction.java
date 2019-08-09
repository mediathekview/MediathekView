package mediathek.gui.actions;

import mediathek.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class SearchProgramUpdateAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public SearchProgramUpdateAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.NAME, "Nach Update suchen...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        mediathekGui.searchForUpdateOrShowProgramInfos(false);
    }
}
