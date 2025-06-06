package mediathek.gui.actions;

import mediathek.config.Konstanten;
import mediathek.config.StandardLocations;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FileUtils;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.nio.file.Paths;

public class DeleteLocalFilmlistAction extends AbstractAction {
    private final JFrame owner;

    public DeleteLocalFilmlistAction(MediathekGui parent) {
        super();
        owner = parent;

        putValue(NAME, "Lokale Filmliste löschen");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        var filmlistPathStr = StandardLocations.getFilmlistFilePathString();
        var fimlistPath = Paths.get(filmlistPathStr);
        FileUtils.moveToTrash(fimlistPath);
        JOptionPane.showMessageDialog(owner,
                "Filmliste wurde gelöscht.\nDas Programm wird nun beendet.",
                Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
        MediathekGui.ui().quitApplication();
    }
}
