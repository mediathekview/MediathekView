package mediathek.gui.actions;

import javafx.concurrent.Task;
import mediathek.config.Daten;
import mediathek.config.StandardLocations;
import mediathek.filmlisten.writer.FilmListWriter;

public class FilmListWriteWorkerTask extends Task<Void> {

    private final Daten daten;

    public FilmListWriteWorkerTask(Daten daten) {
        super();
        this.daten = daten;
    }

    @Override
    protected Void call() {
        FilmListWriter writer = new FilmListWriter(false);
        updateMessage("Schreibe Filmliste");
        updateProgress(0d, 1d);
        writer.writeFilmList(StandardLocations.getFilmlistFilePath(),
                daten.getListeFilme(),
                prog -> updateProgress(prog, 1d));

        return null;
    }
}
