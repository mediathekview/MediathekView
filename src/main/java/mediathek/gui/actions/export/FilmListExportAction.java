package mediathek.gui.actions.export;

import mediathek.config.Konstanten;
import mediathek.javafx.tool.FXProgressPane;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FileDialogs;
import org.controlsfx.control.StatusBar;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.concurrent.CompletableFuture;

/**
 * Exports the current film list to JSON file.
 */
public class FilmListExportAction extends AbstractAction {
    private final static String HEADER = "Export der Filmliste";
    private final MediathekGui gui;

    public FilmListExportAction(MediathekGui gui) {
        super();
        this.gui = gui;

        putValue(NAME, "Lesbare Filmliste...");
    }

    private void export(File selectedFile) {
        StatusBar bar = gui.getStatusBarController().getStatusBar();
        FXProgressPane hb = new FXProgressPane();

        FilmListExportWorkerTask task = new FilmListExportWorkerTask(selectedFile, true);
        task.setOnSucceeded(e -> {
            bar.getRightItems().remove(hb);
            showSuccess();
        });
        task.setOnFailed(e -> {
            bar.getRightItems().remove(hb);
            showError();
        });

        bar.getRightItems().add(hb);
        hb.prog.progressProperty().bind(task.progressProperty());

        CompletableFuture.runAsync(task);
    }

    private void showError() {
        SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(MediathekGui.ui(),
                "Es gab einen Fehler beim Export der Filmliste.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE));
    }

    private void showSuccess() {
        SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(MediathekGui.ui(),
                "Der Export wurde erfolgreich abgeschlossen.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        setEnabled(false);

        var selectedFile = FileDialogs.chooseSaveFileLocation(gui,"Lesbare Filmliste sichern","");
        if (selectedFile != null) {
            JavaFxUtils.invokeInFxThreadAndWait(() -> export(selectedFile));
        }

        setEnabled(true);
    }

}
