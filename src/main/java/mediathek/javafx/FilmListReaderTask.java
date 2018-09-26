package mediathek.javafx;

import javafx.concurrent.Task;
import mSearch.daten.DatenFilm;
import mSearch.filmlisten.reader.FilmListReader;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.messages.FilmListReadStartEvent;
import mediathek.tool.FilmListUpdateType;
import mediathek.tool.GuiFunktionen;

public class FilmListReaderTask extends Task<Void> {
    private final Daten daten;

    public FilmListReaderTask() {
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
        updateMessage("Initialisiere Datenbank");
        DatenFilm.Database.initializeDatabase();



        updateProgress(-1, 4);
        updateMessage("Lese Filmliste");
        readLocalFilmList();

        if (GuiFunktionen.getImportArtFilme() == FilmListUpdateType.AUTOMATIC && daten.getListeFilme().isTooOld()) {
            updateMessage("Lese Filmliste Netzwerk");
            daten.getFilmeLaden().loadFilmlist("", true);
        }

        return null;
    }
}
