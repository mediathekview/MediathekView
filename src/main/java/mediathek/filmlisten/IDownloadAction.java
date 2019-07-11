package mediathek.filmlisten;

import mediathek.daten.ListeFilme;

interface IDownloadAction {
    boolean performDownload(String dateiUrl, ListeFilme listeFilme, int days);
}
