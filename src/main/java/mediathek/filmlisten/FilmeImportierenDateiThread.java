package mediathek.filmlisten;

import mSearch.daten.ListeFilme;

class FilmeImportierenDateiThread extends Thread {

    private final String pfad;
    private final ListeFilme listeFilme;
    private final int days;
    private final IAction onFinishedAction;
    private final IDownloadAction downloadAction;

    public FilmeImportierenDateiThread(String pfad, ListeFilme listeFilme, int days, IDownloadAction downloadAction,
                                       IAction onFinished) {
        this.pfad = pfad;
        this.listeFilme = listeFilme;
        this.days = days;
        onFinishedAction = onFinished;
        this.downloadAction = downloadAction;

        setName("FilmeImportierenDateiThread");
    }

    @Override
    public void run() {
        final boolean result = downloadAction.performDownload(pfad, listeFilme, days);
        onFinishedAction.onFinished(result);
    }
}
