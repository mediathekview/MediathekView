package mediathek.gui.actions;

import mediathek.config.Icons;
import mediathek.config.Konstanten;
import org.jdesktop.swingx.JXErrorPane;
import org.jdesktop.swingx.error.ErrorInfo;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.net.URI;
import java.util.logging.Level;

@SuppressWarnings("serial")
public class ShowOnlineHelpAction extends AbstractAction {
    private Component owner = null;

    public ShowOnlineHelpAction(Component owner) {
        super("Online-Hilfe anzeigen", Icons.ICON_MENUE_HELP);
        this.owner = owner;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        //TODO Merge with UrlHyperlinkAction
        if (Desktop.isDesktopSupported()) {
            Desktop d = Desktop.getDesktop();
            try {
                if (d.isSupported(Desktop.Action.BROWSE)) {
                    d.browse(new URI(Konstanten.ADRESSE_ONLINE_HELP));
                }
            } catch (Exception ex) {
                final ErrorInfo info = new ErrorInfo("Online-Hilfe",
                        "<html>Es trat ein Fehler beim Öffnen der Online-Hilfe auf.<br>" +
                                "Sollte dieser häufiger auftreten kontaktieren Sie bitte " +
                                "das Entwicklerteam.</html>",
                        null,
                        null,
                        ex,
                        Level.SEVERE,
                        null);
                JXErrorPane.showDialog(owner, info);
            }
        }
    }
}
