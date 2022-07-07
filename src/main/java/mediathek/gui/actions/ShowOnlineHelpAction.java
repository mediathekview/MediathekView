package mediathek.gui.actions;

import mediathek.config.Konstanten;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.SwingErrorDialog;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public class ShowOnlineHelpAction extends AbstractAction {
    public ShowOnlineHelpAction() {
        super();
        putValue(NAME, "Online-Hilfe anzeigen...");
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (Desktop.isDesktopSupported()) {
            Desktop d = Desktop.getDesktop();
            if (d.isSupported(Desktop.Action.BROWSE)) {
                try {
                    d.browse(new URI(Konstanten.ADRESSE_ONLINE_HELP));
                } catch (IOException | URISyntaxException ex) {
                    SwingErrorDialog.showExceptionMessage(MediathekGui.ui(),
                            "Es trat ein Fehler beim Öffnen der Online-Hilfe auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                            ex);
                }
            }
            else {
                JOptionPane.showMessageDialog(MediathekGui.ui(),
                        "<html>Ihr Betriebssystem unterstützt das Öffnen des Browsers nicht.<br>" +
                                "Bitte öffnen Sie <b>" + Konstanten.ADRESSE_ONLINE_HELP + "</b> selbst in Ihrem Browser.</html>",
                        Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
            }
        }
    }
}
