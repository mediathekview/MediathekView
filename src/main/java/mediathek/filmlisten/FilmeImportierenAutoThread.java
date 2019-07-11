package mediathek.filmlisten;

import mediathek.config.Konstanten;
import mediathek.daten.ListeFilme;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

class FilmeImportierenAutoThread extends Thread {
    private static final Logger logger = LogManager.getLogger(FilmeImportierenAutoThread.class);
    private final ListeFilme listeFilme;
    private final ListeFilme listeFilmeDiff;
    private final int days;
    private final IAction onFinished;
    private final IDownloadAction downloadAction;

    public FilmeImportierenAutoThread(ListeFilme listeFilme, ListeFilme listeFilmeDiff, int days,
                                      IDownloadAction downloadAction, IAction onFinished) {
        this.listeFilme = listeFilme;
        this.listeFilmeDiff = listeFilmeDiff;
        this.days = days;
        this.onFinished = onFinished;
        this.downloadAction = downloadAction;

        setName("FilmeImportierenAutoThread");
    }

    @Override
    public void run() {
        boolean ret;
        if (listeFilme.isTooOldForDiff()) {
            // dann eine komplette Liste laden
            listeFilme.clear();
            ret = searchFullList(listeFilme, FilmListDownloadType.FULL);
        } else {
            // nur ein Update laden
            ret = searchFullList(listeFilmeDiff, FilmListDownloadType.DIFF_ONLY);
            if (!ret || listeFilmeDiff.isEmpty()) {
                // wenn diff, dann nochmal mit einer kompletten Liste versuchen
                listeFilme.clear();
                listeFilmeDiff.clear();
                ret = searchFullList(listeFilme, FilmListDownloadType.FULL);
            }
        }

        if (!ret) {
            /* listeFilme ist schon wieder null -> "FilmeLaden" */
            logger.error("Es konnten keine Filme geladen werden!");
        }
        onFinished.onFinished(ret);
    }

    private boolean searchFullList(ListeFilme liste, FilmListDownloadType state) {
        String updateUrl = Konstanten.ROUTER_BASE_ADDRESS;

        switch (state) {
            case FULL:
                updateUrl += "Filmliste-akt.xz";
                break;
            case DIFF_ONLY:
                updateUrl += "Filmliste-diff.xz";
                break;
        }

        return downloadAction.performDownload(updateUrl, liste, days);
    }

}
