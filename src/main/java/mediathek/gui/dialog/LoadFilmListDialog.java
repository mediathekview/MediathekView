package mediathek.gui.dialog;

import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;
import java.awt.*;

public class LoadFilmListDialog extends StandardCloseDialog {
    public LoadFilmListDialog(Frame owner) {
        super(owner,"Filmliste laden",true);
        pack();

        setResizable(false);
        GuiFunktionen.centerOnScreen(this,false);
    }

    @Override
    public JComponent createContentPanel() {
        return new PanelFilmlisteLaden();
    }
}
