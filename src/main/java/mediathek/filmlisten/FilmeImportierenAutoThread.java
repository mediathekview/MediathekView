package mediathek.filmlisten;

import mediathek.daten.ListeFilme;
import mediathek.tool.GuiFunktionen;
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
        if (listeFilme.isEmpty() || !listeFilme.metaData().canUseDiffList()) {
            // dann eine komplette Liste laden
            listeFilme.clear();
            ret = downloadAction.performDownload(GuiFunktionen.getFilmListUrl(FilmListDownloadType.FULL), listeFilme, days);
        } else {
            // nur ein Update laden
            ret = downloadAction.performDownload(GuiFunktionen.getFilmListUrl(FilmListDownloadType.DIFF_ONLY), listeFilmeDiff, days);
            if (!ret || listeFilmeDiff.isEmpty()) {
                // wenn diff, dann nochmal mit einer kompletten Liste versuchen
                listeFilme.clear();
                listeFilmeDiff.clear();
                ret = downloadAction.performDownload(GuiFunktionen.getFilmListUrl(FilmListDownloadType.FULL), listeFilme, days);
            }
        }

        if (!ret) {
            /* listeFilme ist schon wieder null -> "FilmeLaden" */
            logger.warn("Es konnten keine Filme geladen werden!");
        }
        onFinished.onFinished(ret);
    }
}
