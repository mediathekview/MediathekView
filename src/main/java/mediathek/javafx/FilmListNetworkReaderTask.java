package mediathek.javafx;

import javafx.concurrent.Task;
import mediathek.config.Daten;
import mediathek.tool.FilmListUpdateType;
import mediathek.tool.GuiFunktionen;

public class FilmListNetworkReaderTask extends Task<Void> {

    @Override
    protected Void call() {
        final Daten daten = Daten.getInstance();

        updateProgress(-1, 4);
        updateMessage("Prüfe Alter der Filmliste");

        if (GuiFunktionen.getImportArtFilme() == FilmListUpdateType.AUTOMATIC && daten.getListeFilme().needsUpdate()) {
            updateMessage("Lade Filmliste Netzwerk");
            daten.getFilmeLaden().loadFilmlist("", true);
        }

        return null;
    }
}
