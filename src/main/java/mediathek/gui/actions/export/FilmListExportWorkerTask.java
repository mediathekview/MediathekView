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
    private final boolean readable;

    public FilmListExportWorkerTask(File selectedFile, boolean readable) {
        super();
        this.selectedFile = selectedFile;
        this.readable = readable;
    }

    @Override
    protected Void call() {
        FilmListWriter writer = new FilmListWriter(readable);
        writer.writeFilmList(selectedFile.getAbsolutePath(),
                Daten.getInstance().getListeFilme(),
                prog -> updateProgress(prog, 1d));
        return null;
    }
}
