package mediathek.gui.actions;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.MVConfig;
import mediathek.gui.abo.ManageAboDialog;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class ManageAboAction extends AbstractAction {
    private ManageAboDialog dialog;

    public ManageAboAction() {
        putValue(Action.NAME, "Abos verwalten...");
        putValue(Action.SMALL_ICON, IconFontSwing.buildIcon(FontAwesome.DATABASE, 16));
    }

    public void closeDialog() {
        if (dialog != null) {
            dialog.setVisible(false);
        }
    }

    @Override
    public void actionPerformed(ActionEvent e) {

        dialog = new ManageAboDialog(MediathekGui.ui());
        dialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                dialog.getAboPanel().tabelleSpeichern();
                GuiFunktionen.getSize(MVConfig.Configs.SYSTEM_GROESSE_MANAGE_ABO, dialog);
                setEnabled(true);
                dialog = null;
                super.windowClosing(e);
            }
        });
        dialog.setVisible(true);

        setEnabled(false);
    }

}
