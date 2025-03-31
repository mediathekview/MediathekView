package mediathek.gui.actions;

import mediathek.config.Konstanten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.mainwindow.MediathekGui;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.sql.SQLException;

public class OptimizeHistoryDbAction extends AbstractAction {
    private final MediathekGui mediathekGui;
    private static final Logger logger = LogManager.getLogger();

    public OptimizeHistoryDbAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(NAME, "History-Datenbank optimieren...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        try (SeenHistoryController controller = new SeenHistoryController()) {
            var dupes = controller.checkDuplicates();
            var numDuplicates = dupes.total() - dupes.distinct();
            logger.trace("{} duplicates found in history", numDuplicates);
            if (numDuplicates == 0) {
                //no duplicates
                var msg = "Es sind keine Duplikate vorhanden.\nEine Optimierung ist nicht notwendig.";
                JOptionPane.showMessageDialog(mediathekGui, msg, Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
            }
            else {
                var msg = String.format("Es wurden %d Duplikate gefunden.\nMöchten Sie diese bereinigen?", numDuplicates);
                var answer = JOptionPane.showOptionDialog(mediathekGui, msg, Konstanten.PROGRAMMNAME,
                        JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE, null, null, null);
                if (answer == JOptionPane.YES_OPTION) {
                    controller.removeDuplicates();
                    dupes = controller.checkDuplicates();
                    numDuplicates = dupes.total() - dupes.distinct();
                    logger.trace("{} duplicates found in history after cleanup", numDuplicates);
                    msg = String.format("Datenbank wurde bereinigt.\n%d Duplikate blieben übrig.", numDuplicates);
                    JOptionPane.showMessageDialog(mediathekGui, msg, Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
                }
            }
        } catch (SQLException ex) {
            JOptionPane.showMessageDialog(mediathekGui, ex.getMessage(), Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
        }
    }
}
