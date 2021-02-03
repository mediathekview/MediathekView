package mediathek.gui.actions;

import jiconfont.icons.font_awesome.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.daten.ListeAbo;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class CreateNewAboAction extends AbstractAction {
    private final ListeAbo listeAbo;

    public CreateNewAboAction(ListeAbo listeAbo) {
        this.listeAbo = listeAbo;
        putValue(Action.NAME,"Abo anlegen...");
        putValue(Action.SMALL_ICON, IconFontSwing.buildIcon(FontAwesome.PLUS, 16));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        listeAbo.addAbo("Neu");
    }
}
