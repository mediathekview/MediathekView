package mediathek.javafx;

import javafx.concurrent.Task;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.reader.FilmListReader;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.gui.messages.FilmListReadStartEvent;
import mediathek.gui.messages.FilmListReadStopEvent;
import mediathek.tool.GuiFunktionen;

import javax.swing.*;

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
    protected Void call() throws Exception {
        daten.getMessageBus().publishAsync(new FilmListReadStartEvent());

        updateProgress(-1,4);
        updateMessage("Lese Filmliste");
        readLocalFilmList();
        updateMessage("Lese Filmliste Netzwerk");

        if (GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUTO && daten.getListeFilme().isTooOld()) {
            if (!daten.getFilmeLaden().loadFilmListFromNetwork()) {
                //if we haven´t loaded anything, let´s continue with the current list and start filtering...
                performFilterOperations();
            }
        } else {
            // beim Neuladen wird es dann erst gemacht
            performFilterOperations();
        }

        return null;
    }

    private void performFilterOperations() {
        daten.getMessageBus().publishAsync(new FilmListReadStopEvent());
        //SwingUtilities.invokeLater(() -> daten.getFilmeLaden().notifyStart(new ListenerFilmeLadenEvent("", "", 0, 0, 0, false)));


        updateMessage("Themen suchen");
        updateProgress(-1,4);
        daten.getListeFilme().fillSenderList();

        updateMessage("Abos eintragen");
        updateProgress(-1,4);
        daten.getListeAbo().setAboFuerFilm(daten.getListeFilme(), false);

        updateMessage("Blacklist filtern");
        updateProgress(-1,4);
        daten.getListeBlacklist().filterListe();

        updateMessage("Fertig.");
        updateProgress(4,4);
        SwingUtilities.invokeLater(() -> daten.getFilmeLaden().notifyFertig(new ListenerFilmeLadenEvent("", "", 100, 100, 0, false)));
    }
}
