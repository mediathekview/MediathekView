package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.res.GetIcon;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ShowBlacklistDialogAction extends AbstractAction {
    private static final String PANEL_BLACKLIST_NAME_POSTFIX = "_2";
    private final JFrame parent;
    private final Daten daten;

    public ShowBlacklistDialogAction(JFrame parent, Daten daten) {
        this.daten = daten;
        this.parent = parent;

        putValue(NAME, "Blacklist öffnen...");
        putValue(SMALL_ICON, GetIcon.getProgramIcon("menue-blacklist.png", 16, 16));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        DialogLeer dialog = new DialogLeer(parent, true);
        dialog.init("Blacklist", new PanelBlacklist(daten, daten.getMediathekGui(), PanelBlacklist.class.getName() + PANEL_BLACKLIST_NAME_POSTFIX));
        dialog.setVisible(true);
    }
}
