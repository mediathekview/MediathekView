package mediathek.gui.actions;

import mediathek.update.ProgrammUpdateSuchen;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ShowProgramInfosAction extends AbstractAction {
    public ShowProgramInfosAction() {
        putValue(Action.NAME,"Programminfos anzeigen...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        new ProgrammUpdateSuchen().checkVersion(false, true, false, false);
    }
}
