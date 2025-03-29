package mediathek.tool.models;

import mediathek.daten.DatenFilm;
import mediathek.gui.bookmark.BookmarkDataSwing;
import mediathek.tool.FilmSize;
import mediathek.tool.datum.DatumFilm;
import javax.swing.table.AbstractTableModel;
import java.util.ArrayList;
import java.util.List;

public class TModelBookmark extends AbstractTableModel {

    private static final int COLUMN_COUNT = 10;
    private final List<BookmarkDataSwing> dataList;

    public TModelBookmark() {
        dataList = new ArrayList<>();
    }

    public TModelBookmark(int capacity) {
        dataList = new ArrayList<>(capacity);
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
            case BookmarkDataSwing.BOOKMARK_DAUER -> Integer.class;
            case BookmarkDataSwing.BOOKMARK_DATUM -> DatumFilm.class;
            default -> String.class;
        };
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case BookmarkDataSwing.BOOKMARK_ABSPIELEN,BookmarkDataSwing.BOOKMARK_AUFZEICHNEN ->  "";
            case BookmarkDataSwing.BOOKMARK_SENDER -> "Sender";
            case BookmarkDataSwing.BOOKMARK_THEMA -> "Thema";
            case BookmarkDataSwing.BOOKMARK_TITEL -> "Titel";
            case BookmarkDataSwing.BOOKMARK_DAUER -> "Dauer";
            case BookmarkDataSwing.BOOKMARK_DATUM -> "Datum";
            case BookmarkDataSwing.BOOKMARK_URL -> "URL";
            case BookmarkDataSwing.BOOKMARK_VERFUEGBAR -> "Verfügbar bis";
            case BookmarkDataSwing.BOOKMARK_NOTIZ -> "Notiz";
            default -> throw new IndexOutOfBoundsException("UNKNOWN COLUMN NAME: " + column);
        };
    }

    @Override
    public Object getValueAt(int row, int column) {
        final var film = dataList.get(row);

        return switch (column) {
            case BookmarkDataSwing.BOOKMARK_SENDER -> film.getSender();
            case BookmarkDataSwing.BOOKMARK_THEMA -> film.getThema();
            case BookmarkDataSwing.BOOKMARK_TITEL -> film.getTitel();
            case BookmarkDataSwing.BOOKMARK_DATUM -> film.getSendDate();
            case BookmarkDataSwing.BOOKMARK_DAUER -> film.getDauer();
            case BookmarkDataSwing.BOOKMARK_ABSPIELEN, BookmarkDataSwing.BOOKMARK_AUFZEICHNEN -> "";
            case BookmarkDataSwing.BOOKMARK_URL -> film.getWebUrl();
            case BookmarkDataSwing.BOOKMARK_VERFUEGBAR -> film.getExpiry();
            case BookmarkDataSwing.BOOKMARK_NOTIZ -> film.getNote();
            default -> throw new IndexOutOfBoundsException("UNKNOWN COLUMN VALUE: " + column);
        };
    }

    public void addAll(List<BookmarkDataSwing> listeFilme) {
        final int oldRowCount = dataList.size();
        dataList.addAll(listeFilme);
        fireTableRowsInserted(oldRowCount, oldRowCount + dataList.size());
    }
}
