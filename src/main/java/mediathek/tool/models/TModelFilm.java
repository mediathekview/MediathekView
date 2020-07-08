package mediathek.tool.models;

import mediathek.daten.DatenFilm;
import mediathek.tool.DatumFilm;
import mediathek.tool.FilmSize;

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
        return switch (columnIndex) {
            case DatenFilm.FILM_NR, DatenFilm.FILM_DAUER -> Integer.class;
            case DatenFilm.FILM_DATUM -> DatumFilm.class;
            case DatenFilm.FILM_GROESSE -> FilmSize.class;
            case DatenFilm.FILM_HD, DatenFilm.FILM_UT -> Boolean.class;
            case DatenFilm.FILM_DATUM_LONG -> Long.class;
            default -> String.class;
        };
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN, DatenFilm.FILM_MERKEN -> "";
            case DatenFilm.FILM_NR -> "Nr";
            case DatenFilm.FILM_SENDER -> "Sender";
            case DatenFilm.FILM_THEMA -> "Thema";
            case DatenFilm.FILM_TITEL -> "Titel";
            case DatenFilm.FILM_DATUM -> "Datum";
            case DatenFilm.FILM_ZEIT -> "Zeit";
            case DatenFilm.FILM_DAUER -> "Dauer";
            case DatenFilm.FILM_GROESSE -> "Größe [MB]";
            case DatenFilm.FILM_HD -> "HQ";
            case DatenFilm.FILM_UT -> "UT";
            case DatenFilm.FILM_GEO -> "Geo";
            case DatenFilm.FILM_URL -> "URL";
            default -> throw new IndexOutOfBoundsException("UNKNOWN COLUMN NAME: " + column);
        };
    }

    @Override
    public Object getValueAt(int row, int column) {
        final DatenFilm film = (DatenFilm) dataVector.elementAt(row).elementAt(0);

        return switch (column) {
            case DatenFilm.FILM_NR -> film.getFilmNr();
            case DatenFilm.FILM_SENDER -> film.getSender();
            case DatenFilm.FILM_THEMA -> film.getThema();
            case DatenFilm.FILM_TITEL -> film.getTitle();
            case DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN, DatenFilm.FILM_MERKEN -> "";
            case DatenFilm.FILM_DATUM -> film.getDatumFilm();
            case DatenFilm.FILM_ZEIT -> film.getSendeZeit();
            case DatenFilm.FILM_DAUER -> film.getDuration();
            case DatenFilm.FILM_GROESSE -> film.getFilmSize();
            case DatenFilm.FILM_HD -> film.isHighQuality();
            case DatenFilm.FILM_UT -> film.hasSubtitle();
            case DatenFilm.FILM_GEO -> film.getGeo().orElse("");
            case DatenFilm.FILM_URL -> film.getUrl();
            case DatenFilm.FILM_REF -> film;
            default -> throw new IndexOutOfBoundsException("UNKNOWN COLUMN VALUE: " + column);
        };
    }
}
