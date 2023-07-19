package mediathek.gui.actions;

import mediathek.config.Konstanten;
import mediathek.config.StandardLocations;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.nio.file.Paths;

public class DeleteLocalFilmlistAction extends AbstractAction {
    private final JFrame owner;
    private static final Logger logger = LogManager.getLogger();

    public DeleteLocalFilmlistAction(MediathekGui parent) {
        super();
        owner = parent;

        putValue(NAME, "Lokale Filmliste löschen");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            var filmlistPathStr = StandardLocations.getFilmlistFilePathString();
            var fimlistPath = Paths.get(filmlistPathStr);
            FileUtils.moveToTrash(fimlistPath);
            JOptionPane.showMessageDialog(owner,
                    "Filmliste wurde gelöscht.\nDas Programm wird nun beendet.",
                    Konstanten.PROGRAMMNAME,JOptionPane.INFORMATION_MESSAGE);
            MediathekGui.ui().quitApplication();
        } catch (IOException ex) {
            logger.error("Failed to delete filmlist", ex);
            JOptionPane.showMessageDialog(owner,
                    """
                            Es trat ein Fehler beim Löschen der Filmliste auf.
                            Weitere Informationen finden Sie in der Logdatei.
                            Das Programm wird nun beendet.""",
                    Konstanten.PROGRAMMNAME,JOptionPane.ERROR_MESSAGE);
        }
    }
}
