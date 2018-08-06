package mediathek.filmlisten;

import mSearch.daten.ListeFilme;

interface IDownloadAction {
    boolean performDownload(String dateiUrl, ListeFilme listeFilme, int days);
}
