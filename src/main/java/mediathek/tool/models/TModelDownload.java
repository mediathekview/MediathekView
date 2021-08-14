package mediathek.tool.models;

import mediathek.daten.DatenDownload;
import mediathek.tool.MVFilmSize;
import mediathek.tool.datum.Datum;

public class TModelDownload extends TModel {
    @Override
    public Class<?> getColumnClass(int columnIndex) {
        return switch (columnIndex) {
            case DatenDownload.DOWNLOAD_NR, DatenDownload.DOWNLOAD_FILM_NR -> Integer.class;
            case DatenDownload.DOWNLOAD_DATUM -> Datum.class;
            case DatenDownload.DOWNLOAD_GROESSE -> MVFilmSize.class;
            case DatenDownload.DOWNLOAD_HD, DatenDownload.DOWNLOAD_UT, DatenDownload.DOWNLOAD_SPOTLIGHT,
                    DatenDownload.DOWNLOAD_UNTERBROCHEN, DatenDownload.DOWNLOAD_PROGRAMM_RESTART,
                    DatenDownload.DOWNLOAD_INFODATEI, DatenDownload.DOWNLOAD_SUBTITLE,
                    DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER, DatenDownload.DOWNLOAD_ZURUECKGESTELLT -> Boolean.class;
            case DatenDownload.DOWNLOAD_REF -> DatenDownload.class;
            default -> String.class;
        };
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case DatenDownload.DOWNLOAD_NR -> "DL Nr";
            case DatenDownload.DOWNLOAD_FILM_NR -> "Film Nr.";
            case DatenDownload.DOWNLOAD_ABO -> "Abo";
            case DatenDownload.DOWNLOAD_SENDER -> "Sender";
            case DatenDownload.DOWNLOAD_THEMA -> "Thema";
            case DatenDownload.DOWNLOAD_TITEL -> "Titel";
            case DatenDownload.DOWNLOAD_BUTTON_START, DatenDownload.DOWNLOAD_BUTTON_DEL -> "";
            case DatenDownload.DOWNLOAD_PROGRESS -> "Fortschritt";
            case DatenDownload.DOWNLOAD_RESTZEIT -> "Restzeit";
            case DatenDownload.DOWNLOAD_BANDBREITE -> "Geschwindigkeit";
            case DatenDownload.DOWNLOAD_GROESSE -> "Größe [MB]";
            case DatenDownload.DOWNLOAD_DATUM -> "Datum";
            case DatenDownload.DOWNLOAD_ZEIT -> "Zeit";
            case DatenDownload.DOWNLOAD_DAUER -> "Dauer";
            case DatenDownload.DOWNLOAD_HD -> "HQ";
            case DatenDownload.DOWNLOAD_UT -> "UT";
            case DatenDownload.DOWNLOAD_UNTERBROCHEN -> "Pause";
            case DatenDownload.DOWNLOAD_GEO -> "Geo";
            case DatenDownload.DOWNLOAD_FILM_URL -> "URL Film";
            case DatenDownload.DOWNLOAD_HISTORY_URL -> "URL History";
            case DatenDownload.DOWNLOAD_URL -> "URL";
            case DatenDownload.DOWNLOAD_URL_RTMP -> "URL RTMP";
            case DatenDownload.DOWNLOAD_URL_SUBTITLE -> "URL Untertitel";
            case DatenDownload.DOWNLOAD_PROGRAMMSET -> "Programmset";
            case DatenDownload.DOWNLOAD_PROGRAMM -> "Programm";
            case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF -> "Programmaufruf";
            case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY -> "Programmaufruf Array";
            case DatenDownload.DOWNLOAD_PROGRAMM_RESTART -> "Restart";
            case DatenDownload.DOWNLOAD_ZIEL_DATEINAME -> "Dateiname";
            case DatenDownload.DOWNLOAD_ZIEL_PFAD -> "Pfad";
            case DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME -> "Pfad-Dateiname";
            case DatenDownload.DOWNLOAD_ART -> "Art";
            case DatenDownload.DOWNLOAD_QUELLE -> "Quelle";
            case DatenDownload.DOWNLOAD_ZURUECKGESTELLT -> "Zurückgestellt";
            case DatenDownload.DOWNLOAD_INFODATEI -> "Infodatei";
            case DatenDownload.DOWNLOAD_SPOTLIGHT -> "Spotlight";
            case DatenDownload.DOWNLOAD_SUBTITLE -> "Untertitel";
            case DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER -> "Remote DL";
            case DatenDownload.DOWNLOAD_REF -> "Ref";
            default -> throw new IndexOutOfBoundsException("UNKNOWN COLUMN NAME: " + column);
        };
    }

    @Override
    public int getColumnCount() {
        return DatenDownload.MAX_ELEM;
    }

    @Override
    public Object getValueAt(int row, int column) {
        final DatenDownload download = (DatenDownload) dataVector.elementAt(row).elementAt(DatenDownload.DOWNLOAD_REF);

        return switch (column) {
            case DatenDownload.DOWNLOAD_HD -> (download.film != null) && download.film.isHighQuality();
            case DatenDownload.DOWNLOAD_UT -> download.film != null && download.film.hasSubtitle();
            case DatenDownload.DOWNLOAD_SPOTLIGHT -> download.isSpotlight();
            case DatenDownload.DOWNLOAD_UNTERBROCHEN -> download.isInterrupted();
            case DatenDownload.DOWNLOAD_PROGRAMM_RESTART -> download.isRestart();
            case DatenDownload.DOWNLOAD_INFODATEI -> download.isInfoFile();
            case DatenDownload.DOWNLOAD_SUBTITLE -> download.isSubtitle();
            case DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER -> download.isDownloadManager();
            case DatenDownload.DOWNLOAD_ZURUECKGESTELLT -> download.istZurueckgestellt();
            default -> super.getValueAt(row, column);
        };
    }
}
