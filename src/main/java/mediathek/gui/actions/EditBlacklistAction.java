package mediathek.gui.actions;

import jiconfont.icons.font_awesome.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class EditBlacklistAction extends AbstractAction {
    private final JFrame parent;

    public EditBlacklistAction(JFrame parent) {
        this.parent = parent;

        putValue(NAME, "Blacklist bearbeiten...");
        putValue(SMALL_ICON, IconFontSwing.buildIcon(FontAwesome.SKYATLAS, 16f));
        putValue(SHORT_DESCRIPTION, "Blacklist bearbeiten");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        DialogLeer dialog = new DialogLeer(parent, true);
        dialog.init("Blacklist", new PanelBlacklist(Daten.getInstance(), null, PanelBlacklist.class.getName() + "_3"));
        dialog.setVisible(true);
    }
}
