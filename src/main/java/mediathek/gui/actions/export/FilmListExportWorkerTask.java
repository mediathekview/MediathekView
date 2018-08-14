package mediathek.gui.actions.export;

import javafx.concurrent.Task;
import mSearch.filmlisten.writer.FilmListWriter;
import mediathek.config.Daten;

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
        writer.disableEvents();
        writer.writeFilmList(selectedFile.getAbsolutePath(),
                Daten.getInstance().getListeFilme(),
                prog -> updateProgress(prog, 1d));
        return null;
    }
}
