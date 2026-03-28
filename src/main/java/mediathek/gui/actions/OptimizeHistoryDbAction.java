package mediathek.gui.actions;

import mediathek.config.Konstanten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.mainwindow.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class OptimizeHistoryDbAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public OptimizeHistoryDbAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(NAME, "History-Datenbank optimieren...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        try (SeenHistoryController controller = new SeenHistoryController()) {
            controller.performDatabaseCompact();
            JOptionPane.showMessageDialog(mediathekGui, "Datenbankoptimierung abgeschlossen.", Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
        }
        catch (RuntimeException ex) {
            JOptionPane.showMessageDialog(mediathekGui, ex.getMessage(), Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
        }
    }
}
