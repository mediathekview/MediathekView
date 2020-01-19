package mediathek.tool.models;

import mediathek.daten.DatenDownload;
import mediathek.tool.Datum;
import mediathek.tool.MVFilmSize;

@SuppressWarnings("serial")
public class TModelDownload extends TModel {
    @Override
    public Class<?> getColumnClass(int columnIndex) {
        Class<?> result;
        switch (columnIndex) {
            case DatenDownload.DOWNLOAD_NR:
            case DatenDownload.DOWNLOAD_FILM_NR:
                result = Integer.class;
                break;

            case DatenDownload.DOWNLOAD_DATUM:
                result = Datum.class;
                break;

            case DatenDownload.DOWNLOAD_GROESSE:
                result = MVFilmSize.class;
                break;

            case DatenDownload.DOWNLOAD_HD:
            case DatenDownload.DOWNLOAD_UT:
            case DatenDownload.DOWNLOAD_SPOTLIGHT:
            case DatenDownload.DOWNLOAD_UNTERBROCHEN:
            case DatenDownload.DOWNLOAD_PROGRAMM_RESTART:
            case DatenDownload.DOWNLOAD_INFODATEI:
            case DatenDownload.DOWNLOAD_SUBTITLE:
            case DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER:
            case DatenDownload.DOWNLOAD_ZURUECKGESTELLT:
                result = Boolean.class;
                break;

            case DatenDownload.DOWNLOAD_REF:
                result = DatenDownload.class;
                break;

            default:
                result = String.class;
                break;
        }
        return result;
    }

    @Override
    public String getColumnName(int column) {
        String result;
        switch (column) {
            case DatenDownload.DOWNLOAD_NR:
                result = "DL Nr";
                break;

            case DatenDownload.DOWNLOAD_FILM_NR:
                result = "Film Nr.";
                break;

            case DatenDownload.DOWNLOAD_ABO:
                result = "Abo";
                break;

            case DatenDownload.DOWNLOAD_SENDER:
                result = "Sender";
                break;

            case DatenDownload.DOWNLOAD_THEMA:
                result = "Thema";
                break;

            case DatenDownload.DOWNLOAD_TITEL:
                result = "Titel";
                break;

            case DatenDownload.DOWNLOAD_BUTTON_START:
            case DatenDownload.DOWNLOAD_BUTTON_DEL:
                result = "";
                break;

            case DatenDownload.DOWNLOAD_PROGRESS:
                result = "Fortschritt";
                break;

            case DatenDownload.DOWNLOAD_RESTZEIT:
                result = "Restzeit";
                break;

            case DatenDownload.DOWNLOAD_BANDBREITE:
                result = "Geschwindigkeit";
                break;

            case DatenDownload.DOWNLOAD_GROESSE:
                result = "Größe [MB]";
                break;

            case DatenDownload.DOWNLOAD_DATUM:
                result = "Datum";
                break;

            case DatenDownload.DOWNLOAD_ZEIT:
                result = "Zeit";
                break;

            case DatenDownload.DOWNLOAD_DAUER:
                result = "Dauer";
                break;

            case DatenDownload.DOWNLOAD_HD:
                result = "HQ";
                break;

            case DatenDownload.DOWNLOAD_UT:
                result = "UT";
                break;

            case DatenDownload.DOWNLOAD_UNTERBROCHEN:
                result = "Pause";
                break;

            case DatenDownload.DOWNLOAD_GEO:
                result = "Geo";
                break;

            case DatenDownload.DOWNLOAD_FILM_URL:
                result = "URL Film";
                break;

            case DatenDownload.DOWNLOAD_HISTORY_URL:
                result = "URL History";
                break;

            case DatenDownload.DOWNLOAD_URL:
                result = "URL";
                break;

            case DatenDownload.DOWNLOAD_URL_RTMP:
                result = "URL RTMP";
                break;

            case DatenDownload.DOWNLOAD_URL_SUBTITLE:
                result = "URL Untertitel";
                break;

            case DatenDownload.DOWNLOAD_PROGRAMMSET:
                result = "Programmset";
                break;

            case DatenDownload.DOWNLOAD_PROGRAMM:
                result = "Programm";
                break;

            case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF:
                result = "Programmaufruf";
                break;

            case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY:
                result = "Programmaufruf Array";
                break;

            case DatenDownload.DOWNLOAD_PROGRAMM_RESTART:
                result = "Restart";
                break;

            case DatenDownload.DOWNLOAD_ZIEL_DATEINAME:
                result = "Dateiname";
                break;

            case DatenDownload.DOWNLOAD_ZIEL_PFAD:
                result = "Pfad";
                break;

            case DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME:
                result = "Pfad-Dateiname";
                break;

            case DatenDownload.DOWNLOAD_ART:
                result = "Art";
                break;

            case DatenDownload.DOWNLOAD_QUELLE:
                result = "Quelle";
                break;

            case DatenDownload.DOWNLOAD_ZURUECKGESTELLT:
                result = "Zurückgestellt";
                break;

            case DatenDownload.DOWNLOAD_INFODATEI:
                result = "Infodatei";
                break;

            case DatenDownload.DOWNLOAD_SPOTLIGHT:
                result = "Spotlight";
                break;

            case DatenDownload.DOWNLOAD_SUBTITLE:
                result = "Untertitel";
                break;

            case DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER:
                result = "Remote DL";
                break;

            case DatenDownload.DOWNLOAD_REF:
                result = "Ref";
                break;

            default:
                throw new IndexOutOfBoundsException("UNKNOWN COLUMN NAME: " + column);
        }

        return result;
    }

    @Override
    public int getColumnCount() {
        return DatenDownload.MAX_ELEM;
    }

    @Override
    public Object getValueAt(int row, int column) {
        Object result;
        final DatenDownload download = (DatenDownload) dataVector.elementAt(row).elementAt(DatenDownload.DOWNLOAD_REF);

        switch (column) {
            case DatenDownload.DOWNLOAD_HD:
                result = (download.film != null) && download.film.isHighQuality();
                break;

            case DatenDownload.DOWNLOAD_UT:
                result = download.film != null && download.film.hasSubtitle();
                break;

            case DatenDownload.DOWNLOAD_SPOTLIGHT:
                result = download.isSpotlight();
                break;

            case DatenDownload.DOWNLOAD_UNTERBROCHEN:
                result = download.isInterrupted();
                break;

            case DatenDownload.DOWNLOAD_PROGRAMM_RESTART:
                result = download.isRestart();
                break;

            case DatenDownload.DOWNLOAD_INFODATEI:
                result = download.isInfoFile();
                break;

            case DatenDownload.DOWNLOAD_SUBTITLE:
                result = download.isSubtitle();
                break;

            case DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER:
                result = download.isDownloadManager();
                break;

            case DatenDownload.DOWNLOAD_ZURUECKGESTELLT:
                result = download.istZurueckgestellt();
                break;

            default:
                result = super.getValueAt(row, column);
                break;
        }

        return result;
    }
}
