package mediathek.javafx;

import javafx.concurrent.Task;
import mSearch.filmlisten.reader.FilmListReader;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.gui.messages.FilmListReadStartEvent;
import mediathek.tool.GuiFunktionen;

public class PerformFilmListFilterOperationsTask extends Task<Void> {
    private final Daten daten;

    public PerformFilmListFilterOperationsTask() {
        super();
        daten = Daten.getInstance();
    }

    private void readLocalFilmList() {
        try (FilmListReader reader = new FilmListReader()) {
            reader.readFilmListe(Daten.getDateiFilmliste(), daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
        }
    }

    @Override
    protected Void call() {
        daten.getMessageBus().publishAsync(new FilmListReadStartEvent());

        updateProgress(-1, 4);
        updateMessage("Lese Filmliste");
        readLocalFilmList();

        if (GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUTO && daten.getListeFilme().isTooOld()) {
            updateMessage("Lese Filmliste Netzwerk");
            daten.getFilmeLaden().loadFilmListFromNetwork();
        }

        return null;
    }
}
