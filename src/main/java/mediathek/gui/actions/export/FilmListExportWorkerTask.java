package mediathek.gui.actions.export;

import javafx.concurrent.Task;
import mediathek.config.Daten;
import mediathek.filmlisten.writer.FilmListWriter;

import java.io.File;

/**
 * JavaFX worker task which will export the filmlist and handle progress reporting.
 */
class FilmListExportWorkerTask extends Task<Void> {
    private final File selectedFile;

    public FilmListExportWorkerTask(File selectedFile) {
        super();
        this.selectedFile = selectedFile;
    }

    @Override
    protected Void call() {
        FilmListWriter writer = new FilmListWriter();
        writer.writeFilmList(selectedFile.getAbsolutePath(),
                Daten.getInstance().getListeFilme(),
                prog -> updateProgress(prog, 1d));
        return null;
    }
}
