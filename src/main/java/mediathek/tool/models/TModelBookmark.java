package mediathek.tool.models;

import mediathek.daten.bookmark.DatenBookmark;
import mediathek.tool.datum.DatumFilm;
import javax.swing.table.AbstractTableModel;
import java.util.ArrayList;
import java.util.List;

public class TModelBookmark extends AbstractTableModel {

    private final List<DatenBookmark> dataList;

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
        return DatenBookmark.MAX_ELEM;
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        return switch (columnIndex) {
            case DatenBookmark.BOOKMARK_DAUER -> Integer.class;
            case DatenBookmark.BOOKMARK_DATUM -> DatumFilm.class;
            default -> String.class;
        };
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case DatenBookmark.BOOKMARK_ABSPIELEN,DatenBookmark.BOOKMARK_AUFZEICHNEN ->  "";
            case DatenBookmark.BOOKMARK_SENDER -> "Sender";
            case DatenBookmark.BOOKMARK_THEMA -> "Thema";
            case DatenBookmark.BOOKMARK_TITEL -> "Titel";
            case DatenBookmark.BOOKMARK_DAUER -> "Dauer";
            case DatenBookmark.BOOKMARK_DATUM -> "Datum";
            case DatenBookmark.BOOKMARK_URL -> "URL";
            case DatenBookmark.BOOKMARK_VERFUEGBAR_BIS -> "Verfügbar bis";
            case DatenBookmark.BOOKMARK_NOTIZ -> "Notiz";
            default -> throw new IndexOutOfBoundsException("UNKNOWN COLUMN NAME: " + column);
        };
    }

    @Override
    public Object getValueAt(int row, int column) {
        final var bookmark = dataList.get(row);
        return switch (column) {
            case DatenBookmark.BOOKMARK_SENDER -> bookmark.getSender();
            case DatenBookmark.BOOKMARK_THEMA -> bookmark.getThema();
            case DatenBookmark.BOOKMARK_TITEL -> bookmark.getTitel();
            case DatenBookmark.BOOKMARK_DATUM -> bookmark.getSendDate();
            case DatenBookmark.BOOKMARK_DAUER -> bookmark.getDauer();
            case DatenBookmark.BOOKMARK_ABSPIELEN, DatenBookmark.BOOKMARK_AUFZEICHNEN -> "";
            case DatenBookmark.BOOKMARK_URL -> bookmark.getWebUrl();
            case DatenBookmark.BOOKMARK_VERFUEGBAR_BIS -> bookmark.getExpiry();
            case DatenBookmark.BOOKMARK_NOTIZ -> bookmark.getNote();
            default -> throw new IndexOutOfBoundsException("UNKNOWN COLUMN VALUE: " + column);
        };
    }

    public void addAll(List<DatenBookmark> listeFilme) {
        final int oldRowCount = dataList.size();
        dataList.addAll(listeFilme);
        fireTableRowsInserted(oldRowCount, oldRowCount + dataList.size());
    }
}
