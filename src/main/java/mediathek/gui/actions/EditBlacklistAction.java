package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelBlacklist;
import mediathek.swing.IconUtils;
import mediathek.tool.GuiFunktionen;
import org.apache.commons.lang3.SystemUtils;
import org.kordamp.ikonli.materialdesign2.MaterialDesignL;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class EditBlacklistAction extends AbstractAction {
    private final JFrame parent;

    public EditBlacklistAction(JFrame parent) {
        this.parent = parent;

        putValue(NAME, "Blacklist bearbeiten...");
        putValue(SMALL_ICON, IconUtils.windowBarSpecificToolbarIcon(MaterialDesignL.LIST_BOX_OUTLINE));
        putValue(SHORT_DESCRIPTION, "Blacklist bearbeiten");
        KeyStroke keyStroke;
        if (SystemUtils.IS_OS_MAC_OSX)
            keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F9, GuiFunktionen.getPlatformControlKey());
        else
            keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_B, GuiFunktionen.getPlatformControlKey());
        putValue(Action.ACCELERATOR_KEY, keyStroke);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        DialogLeer dialog = new DialogLeer(parent, true);
        dialog.init("Blacklist", new PanelBlacklist(Daten.getInstance(), null, PanelBlacklist.class.getName() + "_3"));
        dialog.setVisible(true);
    }
}
