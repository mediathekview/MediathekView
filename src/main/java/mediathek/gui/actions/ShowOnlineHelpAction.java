package mediathek.gui.actions;

import mSearch.tool.javafx.FXErrorDialog;
import mediathek.config.Konstanten;
import mediathek.res.GetIcon;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.net.URI;

public class ShowOnlineHelpAction extends AbstractAction {
    public ShowOnlineHelpAction() {
        super();
        putValue(NAME, "Online-Hilfe anzeigen...");
        putValue(SMALL_ICON, GetIcon.getProgramIcon("menue-help.png", 16, 16));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (Desktop.isDesktopSupported()) {
            Desktop d = Desktop.getDesktop();
            try {
                if (d.isSupported(Desktop.Action.BROWSE)) {
                    d.browse(new URI(Konstanten.ADRESSE_ONLINE_HELP));
                }
            } catch (Exception ex) {
                FXErrorDialog.showErrorDialog("Online-Hilfe",
                        "Fehler beim Öffnen der Online-Hilfe",
                        "Es trat ein Fehler beim Öffnen der Online-Hilfe auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                        ex);
            }
        }

    }
}
