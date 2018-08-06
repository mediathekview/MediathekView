package mediathek.filmlisten;

import mSearch.Config;
import mSearch.daten.ListeFilme;
import mSearch.filmlisten.FilmlistenSuchen;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;

class FilmeImportierenAutoThread extends Thread {
    private static final Logger logger = LogManager.getLogger(FilmeImportierenAutoThread.class);
    private final ListeFilme listeFilme;
    private final ListeFilme listeFilmeDiff;
    private final int days;
    private final IAction onFinished;
    private final IDownloadAction downloadAction;
    private final FilmlistenSuchen msFilmlistenSuchen;
    private FilmListDownloadType state;

    public FilmeImportierenAutoThread(FilmlistenSuchen msFilmlisteSuchen, ListeFilme listeFilme, ListeFilme listeFilmeDiff, int days,
                                      IDownloadAction downloadAction, IAction onFinished) {
        this.listeFilme = listeFilme;
        this.listeFilmeDiff = listeFilmeDiff;
        this.days = days;
        this.onFinished = onFinished;
        this.downloadAction = downloadAction;
        this.msFilmlistenSuchen = msFilmlisteSuchen;

        setName("FilmeImportierenAutoThread");
    }

    @Override
    public void run() {
        boolean ret;
        if (listeFilme.isTooOldForDiff()) {
            // dann eine komplette Liste laden
            state = FilmListDownloadType.FULL;
            listeFilme.clear();
            ret = searchFullList(listeFilme);
        } else {
            // nur ein Update laden
            state = FilmListDownloadType.DIFF_ONLY;
            ret = searchFullList(listeFilmeDiff);
            if (!ret || listeFilmeDiff.isEmpty()) {
                // wenn diff, dann nochmal mit einer kompletten Liste versuchen
                state = FilmListDownloadType.FULL;
                listeFilme.clear();
                listeFilmeDiff.clear();
                ret = searchFullList(listeFilme);
            }
        }

        if (!ret) {
            /* listeFilme ist schon wieder null -> "FilmeLaden" */
            logger.error("Es konnten keine Filme geladen werden!");
        }
        onFinished.onFinished(ret);
    }

    private boolean searchFullList(ListeFilme liste) {
        boolean ret = false;
        ArrayList<String> versuchteUrls = new ArrayList<>();
        String updateUrl = "";

        switch (state) {
            case FULL:
                updateUrl = msFilmlistenSuchen.suchenAkt(versuchteUrls);
                break;
            case DIFF_ONLY:
                updateUrl = msFilmlistenSuchen.suchenDiff(versuchteUrls);
                break;
        }

        if (updateUrl.isEmpty()) {
            return false;
        }

        // 5 mal mit einem anderen Server probieren, wenns nicht klappt
        final int maxRetries = state == FilmListDownloadType.DIFF_ONLY ? 2 : 5; //bei diff nur 2x probieren, dann eine akt-liste laden
        for (int i = 0; i < maxRetries; ++i) {
            ret = downloadAction.performDownload(updateUrl, liste, days);
            if (ret && i < 1 && liste.isOlderThan(5 * 60 * 60 /*sekunden*/)) {
                // Laden hat geklappt ABER: Liste zu alt, dann gibts einen 2. Versuch
                logger.info("Filmliste zu alt, neuer Versuch");
                ret = false;
            }

            if (ret) {
                // hat geklappt, nix wie weiter
                return true;
            }

            switch (state) {
                case FULL:
                    updateUrl = msFilmlistenSuchen.listeFilmlistenUrls_akt.getRand(versuchteUrls); //nächste Adresse in der Liste wählen
                    break;
                case DIFF_ONLY:
                    updateUrl = msFilmlistenSuchen.listeFilmlistenUrls_diff.getRand(versuchteUrls); //nächste Adresse in der Liste wählen
                    break;
            }
            versuchteUrls.add(updateUrl);
            // nur wenn nicht abgebrochen, weitermachen
            if (Config.getStop()) {
                break;
            }

        }
        return ret;
    }

}
