package mediathek.tool.models;

import javax.swing.table.AbstractTableModel;
import java.util.List;
import mediathek.daten.bookmark.DatenBookmark;
import mediathek.daten.bookmark.ListeBookmark;

public class BookmarkModel extends AbstractTableModel {

  private final String[] columnNames = { "Sender", "Thema", "Titel", "Dauer", "Sendedatum", "","","URL", "Verfügbar bis", "Notiz" };
  private final ListeBookmark bookmarks;

  public BookmarkModel(ListeBookmark bookmarks) {
    this.bookmarks = bookmarks;
  }

  @Override
  public int getRowCount() {
    return bookmarks.getBookmarks().size();
  }

  @Override
  public int getColumnCount() {
    return columnNames.length;
  }

  @Override
  public String getColumnName(int col) {
    return columnNames[col];
  }

  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    DatenBookmark bookmark = bookmarks.getBookmarks().get(rowIndex);
    switch (columnIndex) {
      case 0: return bookmark.getSender();
      case 1: return bookmark.getThema();
      case 2: return bookmark.getTitel();
      case 3: return bookmark.getDauer();
      case 4: return bookmark.getSendDate();
      case 5: return "";
      case 6: return "";
      case 7: return bookmark.getUrl();
      case 8: return bookmark.getExpiry();
      case 9: return bookmark.getNote();
      default: return null;
    }
  }


  @Override
  public void setValueAt(Object value, int row, int col) {
    DatenBookmark bm = bookmarks.getBookmarks().get(row);
    switch (col) {
      case 0 -> bm.setSeen((Boolean) value);
      case 1 -> bm.setTitel((String) value);
      case 2 -> bm.setSender((String) value);
      case 3 -> bm.setExpiry((String) value);
      case 4 -> bm.setUrl((String) value);
    }
    fireTableCellUpdated(row, col);
  }

  @Override
  public boolean isCellEditable(int row, int col) {
    return false;
  }

  public List<DatenBookmark> getBookmarks() {
    return bookmarks.getBookmarks();
  }
}
