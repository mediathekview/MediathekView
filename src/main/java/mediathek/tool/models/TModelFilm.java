package mediathek.tool.models;

import mediathek.daten.DatenFilm;
import mediathek.tool.FilmSize;
import mediathek.tool.datum.DatumFilm;

import javax.swing.table.AbstractTableModel;
import java.util.ArrayList;
import java.util.List;

public class TModelFilm extends AbstractTableModel {
    private static final int COLUMN_COUNT = 15;
    private final List<DatenFilm> dataList;

    public TModelFilm() {
        dataList = new ArrayList<>();
    }

    public TModelFilm(int capacity) {
        dataList = new ArrayList<>(capacity);
    }

    @Override
    public boolean isCellEditable(int i, int j) {
        return false;
    }

    /**
     * Get model row for a requested film nr.
     *
     * @param reqFilmNr the filmnr of the request
     * @return the model row, otherwise -1
     */
    public int getModelRowForFilmNumber(int reqFilmNr) {
        int ret = 0;

        for (var film : dataList) {
            if (film.getFilmNr() == reqFilmNr)
                return ret;
            else
                ret++;
        }

        return -1;
    }

    @Override
    public int getRowCount() {
        return dataList.size();
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
        final var film = dataList.get(row);

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

    public void addAll(List<DatenFilm> listeFilme) {
        final int oldRowCount = dataList.size();
        dataList.addAll(listeFilme);
        fireTableRowsInserted(oldRowCount, dataList.size());
    }

    public void addRow(DatenFilm film) {
        dataList.add(film);
        final int rowCount = dataList.size();
        fireTableRowsInserted(rowCount, rowCount);
    }
}
