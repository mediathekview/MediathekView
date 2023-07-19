package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.daten.DatenPset;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.SVGIconUtilities;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class PlayFilmAction extends AbstractAction {
    private final GuiFilme guiFilme;

    public PlayFilmAction(GuiFilme guiFilme) {
        this.guiFilme = guiFilme;
        putValue(Action.NAME, "Film abspielen");
        putValue(Action.SHORT_DESCRIPTION, "Film abspielen");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg"));
        KeyStroke keyStroke;
        if (SystemUtils.IS_OS_MAC_OSX)
            keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_F6, GuiFunktionen.getPlatformControlKey());
        else
            keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_P, GuiFunktionen.getPlatformControlKey());
        putValue(Action.ACCELERATOR_KEY, keyStroke);
    }

    @Override
    public synchronized void actionPerformed(ActionEvent e) {
        DatenPset pset = Daten.listePset.getPsetAbspielen();
        if (pset != null) {
            guiFilme.playerStarten(pset);
        } else {
            JOptionPane.showMessageDialog(MediathekGui.ui(),
                    "Es wurde kein Videoplayer eingerichtet.\n" +
                            "Bitte legen Sie diesen unter \"Einstellungen->Set bearbeiten\" fest.",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE);
        }
    }
}
