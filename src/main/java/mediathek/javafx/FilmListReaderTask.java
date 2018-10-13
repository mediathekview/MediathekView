package mediathek.javafx;

import javafx.concurrent.Task;
import mSearch.filmlisten.reader.FilmListReader;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.messages.FilmListReadStartEvent;

public class FilmListReaderTask extends Task<Void> {
    private final Daten daten;

    public FilmListReaderTask() {
        super();
        daten = Daten.getInstance();
    }

    @Override
    protected Void call() {
        daten.getMessageBus().publishAsync(new FilmListReadStartEvent());

        updateProgress(-1, 4);
        updateMessage("Lese lokale Filmliste");
        try (FilmListReader reader = new FilmListReader()) {
            reader.readFilmListe(Daten.getDateiFilmliste(), daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
        }

        return null;
    }
}
