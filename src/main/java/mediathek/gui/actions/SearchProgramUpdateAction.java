package mediathek.gui.actions;

import mediathek.update.ProgrammUpdateSuchen;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class SearchProgramUpdateAction extends AbstractAction {
    public SearchProgramUpdateAction() {
        putValue(Action.NAME, "Nach Update suchen...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        new ProgrammUpdateSuchen().checkVersion(true, false, false, false);
    }
}
