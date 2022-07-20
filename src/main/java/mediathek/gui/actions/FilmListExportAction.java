package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.filmlisten.writer.FilmListWriter;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FileDialogs;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.File;

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

        ProgressMonitor monitor = new ProgressMonitor(MediathekGui.ui(), "Exportiere Filmliste", "", 0, 100);
        monitor.setMillisToPopup(100);
        monitor.setMillisToDecideToPopup(100);
        var selectedFile = FileDialogs.chooseSaveFileLocation(MediathekGui.ui(), "Lesbare Filmliste sichern", "");
        if (selectedFile != null) {

            ExportWorker worker = new ExportWorker(selectedFile);
            worker.addPropertyChangeListener(evt -> {
                if ("progress".equals(evt.getPropertyName())) {
                    int progress = (int) evt.getNewValue();
                    monitor.setProgress(progress);
                }
            });
            worker.execute();
        } else {
            JOptionPane.showMessageDialog(MediathekGui.ui(), "Export wurde abgebrochen", Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE);
            setEnabled(true);
        }
    }

    class ExportWorker extends SwingWorker<Boolean, Double> {
        private final File selectedFile;

        public ExportWorker(@NotNull File selectedFile) {
            this.selectedFile = selectedFile;
        }

        @Override
        protected void done() {
            try {
                System.out.println("Done");
                var result = get();
                if (result)
                    showSuccess();
                else
                    showError();
            } catch (Exception e) {
                showError();
            }
            setEnabled(true);
        }

        @Override
        protected Boolean doInBackground() throws Exception {
            if (selectedFile != null) {
                FilmListWriter writer = new FilmListWriter(true);
                // do not "compress" the sender tag
                writer.setCompressSenderTag(false);
                writer.setCompressThemaTag(false);
                writer.setDecompressUrls(true);
                writer.writeFilmList(selectedFile.getAbsolutePath(),
                        Daten.getInstance().getListeFilme(),
                        prog -> setProgress((int) Math.round(100d * prog)));
            }

            return true;
        }
    }

}
