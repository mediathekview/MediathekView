package mediathek.tool.models;

import mediathek.daten.DatenFilm;
import mediathek.daten.DatenFilmCaptions;
import mediathek.tool.Datum;
import mediathek.tool.MVFilmSize;

import java.util.Vector;

@SuppressWarnings("serial")
public class TModelFilm extends TModel {
    private static final int COLUMN_COUNT = 15;

    /**
     * Liefert die model row in der die erste Spalte idx enthält.
     * Die Indexspalte ist die SPALTE 0!!!!
     */
    @Override
    public int getIdxRow(int idxWert) {
        int ret = 0;
        for (Vector<?> list : getDataVector()) {
            final DatenFilm film = (DatenFilm) list.get(0);
            if (film.getFilmNr() == idxWert) {
                return ret;
            }
            ++ret;
        }
        return -1;
    }

    @Override
    public int getColumnCount() {
        return COLUMN_COUNT;
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        Class<?> result;
        switch (columnIndex) {
            case DatenFilm.FILM_NR:
                result = Integer.class;
                break;

            case DatenFilm.FILM_DATUM:
                result = Datum.class;
                break;

            case DatenFilm.FILM_GROESSE:
                result = MVFilmSize.class;
                break;

            case DatenFilm.FILM_HD:
            case DatenFilm.FILM_UT:
                result = Boolean.class;
                break;

            default:
                result = String.class;
                break;
        }

        return result;
    }

    @Override
    public String getColumnName(int column) 
    {
    	// TODO: clear caption titles in DatenFilmCaptions?
        if (column == DatenFilm.FILM_ABSPIELEN || column == DatenFilm.FILM_AUFZEICHNEN)
        {
        	return "";
        }

        return DatenFilmCaptions.getTitleByFieldIndex(column);
    }

    @Override
    public Object getValueAt(int row, int column) {
        final DatenFilm film = (DatenFilm) dataVector.elementAt(row).elementAt(0);
        Object result;
        switch (column) {
            case DatenFilm.FILM_NR:
                result = film.getFilmNr();
                break;

            case DatenFilm.FILM_SENDER:
                result = film.getSender();
                break;

            case DatenFilm.FILM_THEMA:
                result = film.getThema();
                break;

            case DatenFilm.FILM_TITEL:
                result = film.getTitle();
                break;

            case DatenFilm.FILM_ABSPIELEN:
            case DatenFilm.FILM_AUFZEICHNEN:
                result = "";
                break;

            case DatenFilm.FILM_DATUM:
                result = film.getDatumFilm();
                break;

            case DatenFilm.FILM_ZEIT:
                result = film.getSendeZeit();
                break;

            case DatenFilm.FILM_DAUER:
                result = film.getDauer();
                break;

            case DatenFilm.FILM_GROESSE:
                result = film.getFilmSize();
                break;

            case DatenFilm.FILM_HD:
                result = film.isHighQuality();
                break;

            case DatenFilm.FILM_UT:
                result = film.hasSubtitle();
                break;

            case DatenFilm.FILM_GEO:
                result = film.getGeo().orElse("");
                break;

            case DatenFilm.FILM_URL:
                result = film.getUrl();
                break;

            case DatenFilm.FILM_ABO_NAME:
                result = film.getAboName();
                break;

            case DatenFilm.FILM_URL_SUBTITLE:
                result = film.getUrlSubtitle();
                break;

            case DatenFilm.FILM_URL_KLEIN:
                result = film.getUrlKlein();
                break;

            case DatenFilm.FILM_URL_HD:
                result = film.getHighQualityUrl();
                break;

            case DatenFilm.FILM_URL_HISTORY:
                result = film.getUrl();
                break;

            case DatenFilm.FILM_REF:
                result = film;
                break;

            case DatenFilm.FILM_DATUM_LONG:
                result = film.getDatumLong();
                break;

            default:
                throw new IndexOutOfBoundsException("UNKNOWN COLUMN VALUE: " + column);
        }

        return result;
    }
}
