package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ShutdownAfterDownloadAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public ShutdownAfterDownloadAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.NAME, "Aktion nach abgeschlossenen Downloads...");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/power-off.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (Daten.getInstance().getListeDownloads().unfinishedDownloads() > 0) {
            // ansonsten gibts keine laufenden Downloads auf die man warten sollte
            mediathekGui.beenden(true, false);
        } else {
            JOptionPane.showMessageDialog(mediathekGui,
                    "Die Downloads müssen zuerst gestartet werden.",
                    "Keine laufenden Downloads",
                    JOptionPane.ERROR_MESSAGE);

        }
    }
}
