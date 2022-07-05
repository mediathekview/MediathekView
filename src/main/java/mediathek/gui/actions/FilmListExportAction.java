package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.filmlisten.writer.FilmListWriter;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FileDialogs;

import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * Exports the current film list to JSON file.
 */
public class FilmListExportAction extends AbstractAction {

    public FilmListExportAction() {
        putValue(NAME, "Lesbare Filmliste...");
    }

    private void showError() {
        JOptionPane.showMessageDialog(MediathekGui.ui(),
                "Es gab einen Fehler beim Export der Filmliste.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE);
    }

    private void showSuccess() {
        JOptionPane.showMessageDialog(MediathekGui.ui(),
                "Der Export wurde erfolgreich abgeschlossen.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        setEnabled(false);

        var selectedFile = FileDialogs.chooseSaveFileLocation(MediathekGui.ui(), "Lesbare Filmliste sichern", "");
        if (selectedFile != null) {
            try {
                FilmListWriter writer = new FilmListWriter(true);
                // do not "compress" the sender tag
                writer.setCompressSenderTag(false);
                writer.setCompressThemaTag(false);
                writer.setDecompressUrls(true);
                writer.writeFilmList(selectedFile.getAbsolutePath(),
                        Daten.getInstance().getListeFilme(),
                        prog -> {
                        });
                showSuccess();
            } catch (Exception ex) {
                showError();
            }
        }

        setEnabled(true);
    }

}
